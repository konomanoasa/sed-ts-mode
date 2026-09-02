;;; sed-ts-mode.el --- Tree-sitter mode for POSIX sed  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/sed-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for POSIX sed.

;;; Code:

(require 'treesit)

(defgroup sed-ts nil
  "Tree-sitter mode for POSIX sed."
  :group 'languages)

(defconst sed-ts-mode--grammar-sources
  '((sed "https://github.com/konomanoasa/tree-sitter-sed"
         :revision "v0.7.0")
    (sed_ere "https://github.com/konomanoasa/tree-sitter-sed"
             :revision "v0.7.0"
             :source-dir "sed_ere/src"))
  "Tree-sitter grammar sources for POSIX sed.")

(defcustom sed-ts-mode-regexp-syntax 'bre
  "Default regular-expression syntax used to parse sed scripts."
  :type '(choice
          (const :tag "Basic regular expressions (BRE)" bre)
          (const :tag "Extended regular expressions (ERE)" ere))
  :safe (lambda (value) (memq value '(bre ere)))
  :group 'sed-ts)

(defun sed-ts-mode--language (syntax)
  "Return the Tree-sitter language for SYNTAX."
  (pcase syntax
    ('bre 'sed)
    ('ere 'sed_ere)
    (_ (user-error "Unsupported sed regular expression syntax: %S"
                   syntax))))

;;;; Syntax

(defvar sed-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `sed-ts-mode'.")

(defvar sed-ts-mode-syntax--query-cache nil
  "Cached syntax queries by language.")

;;;;; Syntax Queries

(defun sed-ts-mode-syntax--query (language)
  "Return the cached syntax query for LANGUAGE."
  (let ((entry (assq language sed-ts-mode-syntax--query-cache)))
    (or (cdr entry)
        (let ((query
               (treesit-query-compile
                language
                '((comment_function
                   verb: (function_verb) @comment)
                  (block_function
                   verb: (function_verb) @delimiter)
                  (closing_brace
                   (closing_brace_token) @delimiter))
                t)))
          (push (cons language query) sed-ts-mode-syntax--query-cache)
          query))))

(defun sed-ts-mode-syntax--captures (parser &optional start end)
  "Return syntax captures from PARSER between START and END."
  (with-current-buffer (treesit-parser-buffer parser)
    (save-restriction
      (widen)
      (let ((start (or start (point-min)))
            (end (or end (point-max)))
            (query (sed-ts-mode-syntax--query
                    (treesit-parser-language parser)))
            captures)
        (dolist (capture
                 (treesit-query-capture
                  (treesit-parser-root-node parser)
                  query start end))
          (let ((node (cdr capture)))
            (push (list (car capture)
                        (treesit-node-start node)
                        (treesit-node-end node))
                  captures)))
        (sort captures
              (lambda (left right) (< (nth 1 left) (nth 1 right))))))))

;;;;; Propertization

(defun sed-ts-mode-syntax--delimiter-syntax (position)
  "Return syntax-table syntax for the delimiter at POSITION."
  (pcase (char-after position)
    (?{ (string-to-syntax "(}"))
    (?} (string-to-syntax "){"))))

(defun sed-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (sed-ts-mode-syntax--captures
                        treesit-primary-parser start end))
        (let* ((name (car capture))
               (position (if (eq name 'comment)
                             (nth 1 capture)
                           (1- (nth 2 capture)))))
          (put-text-property
           position (1+ position) 'syntax-table
           (if (eq name 'comment)
               (string-to-syntax "<")
             (sed-ts-mode-syntax--delimiter-syntax position))))))))

;;;;; Setup

(defun sed-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'sed-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Font Lock

;;;;; Features

(defconst sed-ts-mode-font-lock--feature-list
  '((comment)
    (command label string)
    (number constant escape)
    (operator delimiter punctuation bracket regexp))
  "Font-lock features by decoration level.")

;;;;; Settings

(defconst sed-ts-mode-font-lock--command-patterns
  (append
   (mapcar (lambda (function)
             `(,function verb: (function_verb) @font-lock-keyword-face))
           '(append_function
             branch_function
             change_function
             delete_first_line_function
             delete_function
             exchange_function
             get_append_function
             get_function
             hold_append_function
             hold_function
             insert_function
             label_function
             line_number_function
             list_function
             next_append_function
             next_function
             print_first_line_function
             print_function
             quit_function
             read_function
             substitute_function
             test_function
             translate_function
             write_function))
   '((comment_function
      verb: (function_verb) @font-lock-keyword-face
      comment: (comment
                suppression: (default_output_suppression)
                @font-lock-keyword-face))))
  "Font-lock patterns for sed command verbs.")

(defun sed-ts-mode-font-lock--regexp-patterns (language)
  "Return regular-expression font-lock patterns for LANGUAGE."
  (append
   '((ordinary_character_token) @font-lock-regexp-face

     [(left_anchor_token)
      (right_anchor_token)
      (zero_or_more_operator)] @font-lock-operator-face

     (dup_count_token) @font-lock-number-face

     [(open_bracket)
      (close_bracket_token)] @font-lock-bracket-face

     (nonmatching_list_operator) @font-lock-negation-char-face
     (range_operator) @font-lock-operator-face

     [(quoted_character_token)
      (sed_newline_escape_token)] @font-lock-escape-face
     (escaped_delimiter
      (escaped_delimiter_token) @font-lock-escape-face)

     [(period_token)
      (collating_element_token)
      (class_name)
      (coll_elem_single)
      (coll_elem_multi)
      (meta_char)
      (range_end_hyphen)
      (trailing_hyphen)] @font-lock-constant-face

     (character_class
      ["[" "]"] @font-lock-bracket-face)
     (character_class
      ":" @font-lock-punctuation-face)
     (collating_symbol
      ["[" "]"] @font-lock-bracket-face)
     (collating_symbol
      "." @font-lock-punctuation-face)
     (equivalence_class
      ["[" "]"] @font-lock-bracket-face)
     (equivalence_class
      "=" @font-lock-punctuation-face))
   (pcase language
     ('sed
      '([(back_open_parenthesis)
         (back_close_parenthesis_token)
         (back_open_brace)
         (back_close_brace)] @font-lock-bracket-face
        [(back_bar)
         (back_plus)
         (back_qm)] @font-lock-operator-face
        (backreference_token) @font-lock-constant-face))
     ('sed_ere
      '([(open_parenthesis)
         (close_parenthesis_token)
         (open_brace)
         (close_brace)] @font-lock-bracket-face
        [(ere_alternation_operator_token)
         (one_or_more_operator)
         (zero_or_one_operator)
         (repetition_modifier)] @font-lock-operator-face))
     (_ (user-error "Unsupported sed Tree-sitter language: %S"
                    language)))))

(defun sed-ts-mode-font-lock--settings (language)
  "Return the font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :default-language language

   :feature 'command
   sed-ts-mode-font-lock--command-patterns

   :feature 'comment
   '((comment_function
      verb: (function_verb) @font-lock-comment-face)
     (comment
      suppression: (default_output_suppression) @font-lock-comment-face)
     (comment
      (comment_text) @font-lock-comment-face))

   :feature 'label
   '((label_token) @font-lock-constant-face)

   :feature 'string
   '([(rfile_token)
      (wfile_token)
      (text_literal_token)
      (replacement_literal_token)
      (translation_literal_token)] @font-lock-string-face)

   :feature 'number
   '([(line_number_address)
      (occurrence_flag)] @font-lock-number-face)

   :feature 'constant
   '([(last_line_address)
      (global_flag)
      (case_insensitive_flag)
      (print_flag)
      (substitution_flag)
      (matched_text_reference_token)
      (replacement_backreference_token)] @font-lock-constant-face)

   :feature 'escape
   '([(address_escape)
      (text_backslash_escape_token)
      (replacement_escape_token)
      (translation_escape_token)] @font-lock-escape-face
     (replacement_escaped_delimiter
      (escaped_delimiter_token) @font-lock-escape-face)
     (translation_escaped_delimiter
      (escaped_delimiter_token) @font-lock-escape-face))

   :feature 'operator
   '((negation_operator) @font-lock-operator-face)

   :feature 'delimiter
   '((delimiter_token) @font-lock-delimiter-face)

   :feature 'punctuation
   '([(text_introducer_token)
      (text_escaped_newline_token)
      (escaped_newline_token)] @font-lock-punctuation-face
     [(address_separator_token)
      (interval_separator)] @font-lock-punctuation-face
     ((command_separator) @font-lock-punctuation-face
      (:equal @font-lock-punctuation-face ";")))

   :feature 'bracket
   '((block_function
      verb: (function_verb)
      @font-lock-bracket-face)
     (block_function
      closing: (closing_brace
                (closing_brace_token)
                @font-lock-bracket-face)))

   :feature 'regexp
   (sed-ts-mode-font-lock--regexp-patterns language)))

;;;;; Setup

(defun sed-ts-mode-font-lock-setup ()
  "Configure font locking for the current buffer."
  (setq-local treesit-font-lock-feature-list
              sed-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (sed-ts-mode-font-lock--settings
               (sed-ts-mode--language sed-ts-mode-regexp-syntax))))

;;;; Imenu

(defconst sed-ts-mode--label-function-regexp
  "^label_function$"
  "Regexp matching POSIX sed label definitions.")

(defconst sed-ts-mode-imenu-settings
  `((nil ,sed-ts-mode--label-function-regexp
         sed-ts-mode--label-function-p nil))
  "Tree-sitter Imenu settings for POSIX sed.")

(defun sed-ts-mode--label-function-p (node)
  "Return non-nil when NODE is a named POSIX sed label definition."
  (and (treesit-node-match-p
        node sed-ts-mode--label-function-regexp)
       (let* ((label (treesit-node-child-by-field-name node "label"))
              (name (and label (treesit-node-child label 0 t))))
         (and name (equal (treesit-node-type name) "label_token")))))

(defun sed-ts-mode--defun-name (node)
  "Return the name of the label definition NODE."
  (when (sed-ts-mode--label-function-p node)
    (let* ((label (treesit-node-child-by-field-name node "label"))
           (name (and label (treesit-node-child label 0 t))))
      (treesit-node-text name t))))

(defun sed-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function #'sed-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings sed-ts-mode-imenu-settings))

;;;; Indentation

(defcustom sed-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'sed-ts)

(defconst sed-ts-mode-indent-rules
  (let ((rules '(((node-is "closing_brace") parent-bol 0)
                 ((n-p-gp nil "command_list" "block_function")
                  standalone-parent sed-ts-mode-indent-offset)
                 ((parent-is "command_list") column-0 0))))
    (list (cons 'sed rules) (cons 'sed_ere rules)))
  "Tree-sitter indentation rules for POSIX sed.")

(defun sed-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              sed-ts-mode-indent-rules))

;;;; Mode

(defun sed-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language sed-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (treesit-ensure-installed language)))

(defun sed-ts-mode--require-grammar (language)
  "Ensure that the grammar for LANGUAGE is available."
  (unless (sed-ts-mode--ensure-grammar language)
    (user-error "Tree-sitter grammar `%s' is unavailable" language)))

(defun sed-ts-mode--configure ()
  "Configure Tree-sitter features for the current buffer."
  (sed-ts-mode-syntax-setup)
  (sed-ts-mode-font-lock-setup)
  (sed-ts-mode-imenu-setup)
  (sed-ts-mode-indent-setup)
  (treesit-major-mode-setup))

(defun sed-ts-mode--setup ()
  "Configure `sed-ts-mode' in the current buffer."
  (setq-local sed-ts-mode-regexp-syntax sed-ts-mode-regexp-syntax)
  (let ((language (sed-ts-mode--language sed-ts-mode-regexp-syntax)))
    (sed-ts-mode--require-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (sed-ts-mode--configure))

(defun sed-ts-mode--set-regexp-syntax (syntax)
  "Set regular-expression SYNTAX for the current buffer."
  (let* ((language (sed-ts-mode--language syntax))
         (parser treesit-primary-parser))
    (if (eq language (treesit-parser-language parser))
        (setq-local sed-ts-mode-regexp-syntax syntax)
      (sed-ts-mode--require-grammar language)
      (let ((new-parser (treesit-parser-create language)))
        (setq-local sed-ts-mode-regexp-syntax syntax)
        (setq-local treesit-primary-parser new-parser)
        (sed-ts-mode--configure)
        (treesit-parser-delete parser)
        (save-restriction
          (widen)
          (syntax-ppss-flush-cache (point-min))
          (font-lock-flush))))))

(defun sed-ts-mode--reapply ()
  "Reconfigure the buffer after local variables change."
  (sed-ts-mode--set-regexp-syntax sed-ts-mode-regexp-syntax))

(defun sed-ts-toggle-regexp ()
  "Toggle the current buffer between BRE and ERE."
  (interactive)
  (unless (derived-mode-p 'sed-ts-mode)
    (user-error "This buffer is not using sed-ts-mode"))
  (sed-ts-mode--set-regexp-syntax
   (if (eq sed-ts-mode-regexp-syntax 'bre) 'ere 'bre))
  (when (called-interactively-p 'interactive)
    (message "Using %s regular expressions"
             (upcase (symbol-name sed-ts-mode-regexp-syntax)))))

;;;###autoload
(define-derived-mode sed-ts-mode prog-mode "Sed-TS"
  "Major mode for editing POSIX sed."
  :syntax-table sed-ts-mode-syntax-table
  :group 'sed-ts
  (sed-ts-mode--setup)
  (add-hook 'hack-local-variables-hook #'sed-ts-mode--reapply nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sed\\'" . sed-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("sed" . sed-ts-mode))

(provide 'sed-ts-mode)

;;; sed-ts-mode.el ends here
