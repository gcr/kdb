;;; kdb-mode.el --- Mode for interacting with kdb databases and expressions. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kimberly Wilber

;; Author: Kimberly Wilber <kimmy@kjwilber.org>

(defgroup kdb nil
  "Settings for interacting with kdb")

(defcustom kdb-process-path "kdb"
  "Path to the kdb binary"
  :type '(string)
  :group 'kdb)

(defun kdb-exec (input)
  "Calls kdb on the given input."
  ;;(with-temp-buffer
  ;;(insert input)
  (call-process ;(point-min) (point-max)
                kdb-process-path
                nil (current-buffer) nil
                "p"
                input))
;;)


(kdb-exec "vocab-for \"hello\"")
