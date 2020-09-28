;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(library (curl-scheme)
  (export http-response? make-http-response
          http-response-status-code
          http-response-headers
          http-response-port
          http/get
          )
  (import (rnrs (6))
          (curl-scheme private)
          (only (chezscheme)
                put-bytevector
                getenv)
          (srfi :115)
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
  (define CURLOPT-HEADERDATA 10029)
  (define CURLOPT-HEADERFUNCTION 20079)
  (define CURLINFO-LONG #x200000)
  (define CURLINFO-RESPONSE-CODE (+ 2 CURLINFO-LONG))

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

  (define-record-type http-response
    (fields status-code headers port))

  (define (http/get url)
    (define bv #f)
    (define hdrs-hm '())
    (define write-callback
      (c-callback int (pointer int int pointer)
                  (lambda (ptr size nmemb stream)
                    (let ((realsize (* size nmemb)))
                      (when (> realsize (expt 2 16))
                        ;; TODO: handle large data
                        (error 'write-callback "Too large data" realsize))
                      (set! bv (make-bytevector realsize))
                      (let loop ((i 0))
                        (unless (fx>=? i realsize)
                          (bytevector-u8-set! bv i
                                              (pointer-ref-c-uint8 ptr i))
                          (loop (fx+ i 1))))
                      realsize))))
    (define header-callback
      (c-callback int (pointer int int pointer)
                  (lambda (ptr size nmemb stream)
                    (let* ((realsize (- (* size nmemb) 2)) ; subtract CRLF
                           (tmp-bv (make-bytevector realsize)))
                      (let loop ((i 0))
                        (unless (fx>=? i realsize)
                          (bytevector-u8-set!
                           tmp-bv i
                           (pointer-ref-c-uint8 ptr i))
                          (loop (fx+ i 1))))
                      (let ((key-val (header-string->key-value
                                      (utf8->string tmp-bv))))
                        (when key-val
                          (set! hdrs-hm (cons key-val hdrs-hm))))
                      (+ realsize 2)))))
    (let ((curl-handle (%curl-easy-init))
          (resp-ptr (bytevector->pointer (make-bytevector 4))))
      (%curl-easy-setopt/string curl-handle CURLOPT-URL url)
      (%curl-easy-setopt/long curl-handle CURLOPT-HTTPGET 1)
      (%curl-easy-setopt/callback
       curl-handle CURLOPT-HEADERFUNCTION header-callback)
      (%curl-easy-setopt/callback
       curl-handle CURLOPT-WRITEFUNCTION write-callback)
      (%curl-easy-perform curl-handle)
      (free-c-callback header-callback)
      (free-c-callback write-callback)
      (%curl-easy-getinfo curl-handle CURLINFO-RESPONSE-CODE resp-ptr)
      (%curl-easy-cleanup curl-handle)
      (make-http-response
       (pointer-ref-c-long resp-ptr 0)
       hdrs-hm
       (utf8->string bv))))

)
