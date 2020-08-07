;;; teal-mode.el --- Teal Lang editing mode -*- lexical-binding: t -*-

;; Author: Ric da Silva <ric@condense9.com>
;; URL:  https://github.com/condense9/teal-mode
;; Keywords: languages
;; License: MIT

;; Copyright (c) 2020 Condense9 Ltd.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the “Software”), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; Commentary:

;; TODO

(require 'cc-langs)

;;

(defvar teal-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table used in `teal-mode' buffers.")

(defvar teal-mode-abbrev-table nil
  "Abbrev table in use in `teal-mode' buffers.")

(define-abbrev-table 'teal-mode-abbrev-table ())

(defvar teal-mode-map
  (let ((map (make-sparse-keymap)))

    map)
  "Keymap used in `teal-mode' buffers.")


;; See http://www.modernemacs.com/post/major-mode-part-1/

(defconst teal--kwds-builtin
  '("import" "future" "print" "sleep" "atomp" "nullp" "list" "conc" "append"
    "first" "rest" "length" "hash" "get" "set" "nth" "parse_float" "signal"
    "sid" "tid")
  "Teal language builtins")

(defconst teal--builtins
  (list
   (rx-to-string
    `(: (or ,@teal--kwds-builtin)))

   '(0 font-lock-builtin-face)))


(defconst teal--definition
  (list
   (rx-to-string
    `(: (group-n 1 "fn")
        (1+ space)
        (group-n 2 (1+ (or word "_")))))

   '(1 font-lock-keyword-face)
   '(2 font-lock-function-name-face)))

(defconst teal--highlights
  (list '("if\\|else\\|async\\|await" . font-lock-keyword-face)
        '("true\\|false\\|nil" . font-lock-constant-face)
        teal--builtins
        teal--definition))


;;;###autoload
(define-derived-mode teal-mode prog-mode "Teal"
  "Major mode for editing Teal code.
\\{teal-mode-map}"
  :syntax-table teal-mode-syntax-table
  :abbrev-table teal-mode-abbrev-table
  ;; (add-hook 'completion-at-point-functions
  ;;           #'teal-complete-keyword-at-point nil 'local)
  (setq comment-start "// ")
  (setq font-lock-defaults '(teal--highlights)))


(provide 'teal-mode)

;;; teal-mode.el ends here
