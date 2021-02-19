;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2021 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(library (curl-scheme)
  (export http-response? make-http-response
          http-response-status-code
          http-response-headers
          http-response-port
          http/get
          http/post
          http-open-connection
          http-close-connection!
          escape
          json-response?)
  (import (rnrs (6))
          (curl-scheme private)
          (only (chezscheme)
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
  (define CURLOPT-POST 47)
  (define CURLOPT-POSTFIELDSIZE 60)
  (define CURLOPT-POSTFIELDS 10015)
  (define CURLOPT-HEADER 23)
  (define CURLOPT-HEADERDATA 10029)
  (define CURLOPT-HEADERFUNCTION 20079)
  (define CURLINFO-LONG #x200000)
  (define CURLINFO-RESPONSE-CODE (+ 2 CURLINFO-LONG))

  (define-syntax check-call
    (syntax-rules ()
      ((_ proc args ...)
       (let ((return-val (proc args ...)))
         (unless (equal? CURLE-OK return-val)
           (error 'proc "Non-zero return value" return-val))
         return-val))))

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
  (define %curl-easy-escape
    (foreign-procedure libcurl-object pointer curl_easy_escape (pointer pointer int)))
  (define %curl-free
    (foreign-procedure libcurl-object void curl_free (pointer)))

  (define %str-len
    (foreign-procedure libcurl-object int strlen (pointer)))

  (define-record-type http-response
    (fields status-code headers port))

  (define (json-response? resp)
    (let* ((resp-headers (http-response-headers resp))
           (content-type (assoc "content-type" resp-headers)))
      (and content-type
           (regexp-matches?
            '(+ bos "application/json" (* any))
            (cdr content-type)))))

  (define-syntax define-method
    (syntax-rules ()
      ((_ method-name)
       (define method-name
         (case-lambda
           ((curl-handle url)
            (method-name curl-handle url ""))
           ((curl-handle url data)
            (define opt-code
              (case 'method-name
                ('http/get CURLOPT-HTTPGET)
                ('http/post CURLOPT-POST)
                (else (error 'opt-code "No such method" 'method-name))))
            (define bv-data (make-bytevector 0))
            (define hdrs-alist '())
            (define write-callback
              (c-callback int (pointer int int pointer)
                          (lambda (ptr size nmemb stream)
                            (let* ((realsize (* size nmemb))
                                   (bv #f))
                              (when (> (bytevector-length bv-data) (expt 2 26))
                                (error 'write-callback "Too large data"
                                       (bytevector-length bv-data)))
                              (set! bv (make-bytevector realsize))
                              (let loop ((i 0))
                                (unless (fx>=? i realsize)
                                  (bytevector-u8-set! bv i
                                                      (pointer-ref-c-uint8 ptr i))
                                  (loop (fx+ i 1))))
                              (set! bv-data (bytevector-append bv-data bv))
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
                                  (set! hdrs-alist (cons key-val hdrs-alist))))
                              (+ realsize 2)))))
            (let ((resp-ptr (bytevector->pointer (make-bytevector 4))))
              (check-call %curl-easy-setopt/string curl-handle CURLOPT-URL url)
              (check-call %curl-easy-setopt/long curl-handle opt-code 1)
              (when (eq? 'method-name 'http/post)
                (let* ((data*
                        (if (string? data)
                            (string->utf8 data)
                            data))
                       (data-ptr
                        (bytevector->pointer
                         data*)))
                  (check-call %curl-easy-setopt/long
                              curl-handle CURLOPT-POSTFIELDSIZE
                              (bytevector-length data*))
                  (check-call %curl-easy-setopt/pointer
                              curl-handle CURLOPT-POSTFIELDS
                              data-ptr)))
              (check-call %curl-easy-setopt/callback
                          curl-handle CURLOPT-HEADERFUNCTION header-callback)
              (check-call %curl-easy-setopt/callback
                          curl-handle CURLOPT-WRITEFUNCTION write-callback)
              (check-call %curl-easy-perform curl-handle)
              (free-c-callback header-callback)
              (free-c-callback write-callback)
              (check-call %curl-easy-getinfo
                          curl-handle CURLINFO-RESPONSE-CODE resp-ptr)
              (make-http-response
               (pointer-ref-c-long resp-ptr 0)
               hdrs-alist
               bv-data))))))))

  (define-method http/get)
  (define-method http/post)

  (define (http-open-connection)
    (%curl-easy-init))

  (define (http-close-connection! connection)
    (%curl-easy-cleanup connection))

  ;; Encode url
  (define (escape url-string)
    (let* ((curl-handle (%curl-easy-init))
           (url-bv (string->utf8 url-string))
           (encoded-url-ptr (%curl-easy-escape
                             curl-handle
                             (bytevector->pointer
                              url-bv)
                             (bytevector-length
                              url-bv)))
           (encoded-url (utf8->string
                         (pointer->bytevector
                          encoded-url-ptr
                          (%str-len encoded-url-ptr)))))
      (%curl-free encoded-url-ptr)
      (%curl-easy-cleanup curl-handle)
      encoded-url))
)
