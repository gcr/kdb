;;; kdb-mode.el --- Mode for interacting with kdb databases and expressions. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kimberly Wilber

;; kdb is distributed under a license that expressly prohibits military use. See "LICENSE" in this repository for details.


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


;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local kdb-current-parsed-title nil
  "Temporary variable for holding the current title")
(defvar-local kdb-current-parsed-heading-depth 0
  "Temporary variable for holding the current heading depth")

(defun kdb-org-element-parse (tree)
  "Interpret TREE as an Org Element tree and turn it into a Textual tree."
  (let* ((type (org-element-type tree))
         ;; Find interpreter for current object or
         ;; element.  If it doesn't exist (e.g. this is
         ;; a pseudo object or element), return contents,
         ;; if any.
         (interpret
          (let ((fun (intern
                      (format "kdb-org-element--parse-%s" type))))
            (if (fboundp fun) fun (lambda (contents)
                                    (list (list :UNKNOWN contents)))))))
    (funcall interpret tree)))

;; Parsers for org-data elements

(defun kdb-org-element--parse-section (tree)
  "Parse an org-mode section. These need to be flattened."
  (let ()
    ;;((kdb-current-parsed-heading-depth
    ;; (1+ kdb-current-parsed-heading-depth)))
    (-mapcat #'kdb-org-element-parse (org-element-contents tree))))

(defun kdb-org-element--parse-headline (tree)
  "Parse an org-mode headline."
  (cons
   `(heading ,(org-element-property :raw-value tree)
             (depth ,kdb-current-parsed-heading-depth))
   (let ((kdb-current-parsed-heading-depth
          (1+ kdb-current-parsed-heading-depth)))
     (-mapcat #'kdb-org-element-parse (org-element-contents tree)))))

(defun kdb-org-element--parse-keyword (tree)
  "Parse a keyword. Should apply to the current section."
  (pcase (org-element-property :key tree)
    ("TITLE"
     (setq kdb-current-parsed-title (org-element-property :value tree))
     (list))
    (_ (list :UNKNOWN-KEYWORD
             (org-element-property :key tree)
             (org-element-property :value tree)
             ))))

(defun kdb-org-element--parse-paragraph (tree)
  (list (cons 'paragraph
              (-mapcat #'kdb-org-element-parse (org-element-contents tree)))))
(defun kdb-org-element--parse-plain-text (tree)
  ;; tree is a raw string
  (list (list 'span (recursively-trim-string-attributes tree))))
(defun kdb-org-element--parse-link (tree)
  (list (cons 'link (append (-mapcat #'kdb-org-element-parse (org-element-contents tree))
                            (list (list 'url ;tree
                                        (org-element-property :raw-link tree)
                                        ))))))
(defun kdb-org-element--parse-bold (tree)
  "Parse a span of bold characters."
  (list
   (cons 'bold
          (recursively-trim-string-attributes (org-element-contents tree)))))
(defun kdb-org-element--parse-entity (tree)
  "Entities are single symbols that are expanded in org-mode documents."
  (list (list 'span
              (org-element-property :utf-8 tree))))
(defun kdb-org-element--parse-plain-list (tree)
  "Parse a plain (unordered) org-mode list."
  (list
   (cons 'list
         (-mapcat #'kdb-org-element-parse (org-element-contents tree)))))
(defun kdb-org-element--parse-item (tree)
  "Parse a list item."
  (list
   (cons 'item
         (-mapcat #'kdb-org-element-parse (org-element-contents tree)))))
(defun kdb-org-element--parse-planning (tree)
  (let ((ts (or (org-element-property :closed tree)
                (org-element-property :deadline tree)
                (org-element-property :scheduled tree))))
    (list (list 'block-timestamp (org-element-property :raw-value ts)))))

(defun kdb-org-element--parse-org-data (tree)
  "Parse a top-level document."
  (let ((textual-content
         (-mapcat #'kdb-org-element-parse
                  (org-element-contents tree))))
    (if kdb-current-parsed-title
        (list (list 'title kdb-current-parsed-title)
              (cons 'textual textual-content))
      (cons 'textual textual-content))))

(defun recursively-trim-string-attributes (item)
  "Remove all attributes from a deeply nested data structure containing strings.
(Note: does not properly handle cyclic data structures.)"
  (cond
   ((stringp item) (substring-no-properties item))
   ((numberp item) item)
   ((and (consp item) (consp (cdr item)))
    (let ((item (if (plist-get item :parent)
                    (plist-put item :parent nil)
                  item)))
      (mapcar 'recursively-trim-string-attributes item)))
   ((and (consp item))
    (cons (recursively-trim-string-attributes (car item)) nil))
   (t item)))


;;(save-excursion
;;  (with-current-buffer (find-file "~/notes/days/2024-05-08 Wed.org")
;;    (pp-display-expression
;;     (list
;;      (recursively-trim-string-attributes
;;       (kdb-org-element-parse
;;        (org-element-parse-buffer)))
;;      '--------
;;      (recursively-trim-string-attributes
;;       (org-element-parse-buffer)))
;;     "*pp-output*")))

(pp-display-expression
 (--map
  (with-current-buffer (find-file it)
    (recursively-trim-string-attributes
     (list :file it :contents
           (kdb-org-element-parse (org-element-parse-buffer)))))
  (--filter
   (> (file-attribute-size (file-attributes it))
      50)
   (seq-subseq
    (directory-files "~/notes/days/" t "^[^.].*\\.org$")
    50 200)))
  "*all-pp-output*")
