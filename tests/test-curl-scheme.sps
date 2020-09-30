#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6))
        (srfi :64 testing)
        (curl-scheme)
        (curl-scheme private))

(test-begin "private-procedures")

(test-equal header-string->key-value
  '("connection" . "close")
  (header-string->key-value "connection: close"))

(test-equal header-string->key-value
  '("connection" . "close")
  (header-string->key-value "connection : close"))

(test-equal header-string->key-value
  '("connection" . "close")
  (header-string->key-value "connection :close"))

(test-equal bytevector-append
  (make-bytevector 0)
  (bytevector-append (make-bytevector 0) (make-bytevector 0)))

(test-equal bytevector-append
  (u8-list->bytevector (list 1 2 3 4 5))
  (bytevector-append (u8-list->bytevector '(1 2 3))
                     (u8-list->bytevector '(4 5))))

(test-equal bytevector-append
  (u8-list->bytevector (list 1 2 3 4 5))
  (bytevector-append (u8-list->bytevector '(1 2))
                     (u8-list->bytevector '(3 4 5))))

(test-equal bytevector-append
  (u8-list->bytevector (list 1 2 3 4))
  (bytevector-append (u8-list->bytevector '(1 2))
                     (u8-list->bytevector '(3 4))))

(test-equal bytevector-append
  (u8-list->bytevector (list 1))
  (bytevector-append (u8-list->bytevector '())
                     (u8-list->bytevector '(1))))

(test-equal bytevector-append
  (u8-list->bytevector (list 1))
  (bytevector-append (u8-list->bytevector '(1))
                     (u8-list->bytevector '())))

(test-end)

(test-begin "http/get")

(test-equal http/get
  200
  (http-response-status-code (http/get "https://httpbin.org/get")))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
