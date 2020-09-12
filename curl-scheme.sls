;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(library (curl-scheme)
  (export http/get)
  (import (rnrs)
          (only (chezscheme)
                put-bytevector
                getenv
                fx>)
          (pffi))

  (define libcurl-object
    (cond ((getenv "LIBCURL_SO")
           => open-shared-object)
          (else
           (error 'libcurl-object "Set LIBCURL_SO env variable!"))))

  (define (string->c-string s)
    (string->utf8 (string-append s "\x0;")))

  (define CURL-GLOBAL-SLL 1)
  (define CURLE-OK 0)
  (define CURLOPT-URL 10002)
  (define CURLOPT-WRITEFUNCTION 20011)
  (define CURLOPT-HTTPGET 80)
  (define CURLOPT-HEADER 23)


  (define %curl-global-init
    (foreign-procedure libcurl-object int curl_global_init (long)))

  (define %curl-easy-init
    (foreign-procedure libcurl-object pointer curl_easy_init ()))
  (define %curl-easy-setopt/long
    (foreign-procedure libcurl-object int curl_easy_setopt
                       (pointer int long)))
  (define %curl-easy-setopt/pointer
    (foreign-procedure libcurl-object int curl_easy_setopt
                       (pointer int pointer)))
  (define (%curl-easy-setopt/string ptr int str)
    (%curl-easy-setopt/pointer
     ptr int
     (bytevector->pointer (string->c-string str))))
  (define %curl-easy-setopt/callback
    (foreign-procedure libcurl-object int curl_easy_setopt
                       (pointer int
                                (callback int (pointer int int pointer)))))
  (define %curl-easy-perform
    (foreign-procedure libcurl-object int curl_easy_perform (pointer)))
  (define %curl-easy-getinfo
    (foreign-procedure libcurl-object int curl_easy_getinfo
                       (pointer int pointer)))
  (define %curl-easy-reset
    (foreign-procedure libcurl-object void curl_easy_reset (pointer)))
  (define %curl-easy-cleanup
    (foreign-procedure libcurl-object void curl_easy_cleanup (pointer)))

  (define (http/get url)
    (define write-callback
      (c-callback int (pointer int int pointer)
                  (lambda (ptr size nmemb stream)
                    (let ((realsize (* size nmemb)))
                      (let loop ((i 0))
                        (unless (fx> i realsize)
                          #;(display (integer->char
                                    (pointer-ref-c-uint8 ptr i)))
                          (loop (fx+ i 1))))
                      realsize))))
    (let ((curl-handle (%curl-easy-init)))
      (%curl-easy-setopt/string curl-handle CURLOPT-URL url)
      (%curl-easy-setopt/long curl-handle CURLOPT-HEADER 1)
      (%curl-easy-setopt/long curl-handle CURLOPT-HTTPGET 1)
      (%curl-easy-setopt/callback
       curl-handle CURLOPT-WRITEFUNCTION write-callback)
      (%curl-easy-perform curl-handle)
      (free-c-callback write-callback)
      (%curl-easy-cleanup curl-handle)))

)
