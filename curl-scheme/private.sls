;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(library (curl-scheme private)
  (export bytevector-append
          header-string->key-value)
  (import (rnrs (6))
          (srfi :115))

  ;; Converts "key: value\r\n" to '("key" . "value")
  (define (header-string->key-value s)
    (let ((reg (regexp-partition
                '(= 1 (: (* whitespace) ":" (* whitespace))) s)))
      (if (string=? s (car reg))
          #f
          (cons (car reg)
                (apply string-append
                       (cddr reg))))))

  (define (bytevector-append bv1 bv2)
    (let* ((len1 (bytevector-length bv1))
           (len2 (bytevector-length bv2))
           (bv (make-bytevector (+ len1 len2))))
      (bytevector-copy! bv1 0 bv 0 len1)
      (bytevector-copy! bv2 0 bv len1 len2)
      bv))
)
