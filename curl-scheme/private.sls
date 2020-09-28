;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(library (curl-scheme private)
  (export header-string->key-value)
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
)
