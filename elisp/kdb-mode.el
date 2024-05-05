;;; kdb-mode.el --- Mode for interacting with kdb databases and expressions. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kimberly Wilber

;; Author: Kimberly Wilber <kimmy@kjwilber.org>

(defgroup kdb nil
  "Settings for interacting with kdb")

(defcustom kdb-process-path "kdb"
  "Path to the kdb binary"
  :type '(string)
  :group 'kdb)

(defun kdb-exec (&rest input)
  "Calls kdb on the given input."
  ;;(with-temp-buffer
  ;;(insert input)
  (apply call-process
                kdb-process-path
                nil (current-buffer) nil
                input))


;; kdb compose

;;(defvar-local kdb-compose-finish-action
;;  '(lambda () (kdb-exec (buffer-string)))
;;  "The action to run when finishing posting")

(defvar-local kdb-compose-continuation nil
  "Optional action to run when finishing editing or posting.

You can use this to refresh buffers, for example.")
;;;(defvar-local kdb-compose-editing-id nil
;;;  "If editing, the ID of the post that we are editing")
;(defvar-local kdb-compose-editing-args nil
;  "If editing, which args to use while we're editing")

;;(defun kdb-compose-finish ()
;;  "Actually send the new/updated post to kdb."
;;  (interactive)
;;  (funcall kdb-compose-finish-action)
;;  (let ((cc kdb-compose-continuation))
;;    (quit-window)
;;    (when cc
;;      (funcall cc))))

(defvar kdb-compose-mode-map
  (let ((kdb-compose-mode-map (make-keymap)))
    ;(define-key kdb-compose-mode-map "\C-c\C-c" 'kdb-compose-finish)
    kdb-compose-mode-map))

(define-derived-mode kdb-compose-mode lisp-data-mode "kdb"
  "Major mode for composing kdb Docs."
  ;;(setq kdb-compose-finish-action
  ;;        '(lambda () (call-interactively 'tumblesocks-text-post-from-buffer)))
(add-hook 'write-contents-functions 'kdb-write-contents)
  )

(defun kdb-write-contents ()
  "Write the buffer contents to kdb"
  (if (buffer-modified-p)
      (progn
      (set-buffer-modified-p nil)
      (message "Writing...")
      t)))

(defun kdb-compose-new-doc (&optional continuation)
  "Open a new buffer containing a fresh Doc to begin authoring.

Once you're ready to submit your post, press C-c C-c"
  (interactive)
  (pop-to-buffer "*Kdb: New post*")
  (kdb-compose-mode)
  (setq header-line-format "New kdb doc")
  (setq kdb-compose-continuation continuation)
  )
