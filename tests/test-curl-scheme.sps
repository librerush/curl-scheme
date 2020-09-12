#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Kozhabay Dias
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (curl-scheme))

(test-begin "TODO: Write tests!")
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
