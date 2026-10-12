;;; sed-ts-mode.el --- Tree-sitter mode for GNU and POSIX sed  -*- lexical-binding: t; -*-
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
;; Tree-sitter major mode for GNU and POSIX sed.

;;; Code:

(require 'editorconfig)
(require 'elec-pair)
(require 'newcomment)
(require 'treesit)

(defgroup sed-ts nil
  "Tree-sitter mode for GNU and POSIX sed."
  :group 'languages)

;;;; Grammar

(defconst sed-ts-mode--grammar-sources
  '((gsed "https://github.com/konomanoasa/tree-sitter-gsed"
          :revision "v0.3.0")
    (gsed_ere "https://github.com/konomanoasa/tree-sitter-gsed"
              :revision "v0.3.0"
              :source-dir "gsed_ere/src")
    (sed "https://github.com/konomanoasa/tree-sitter-sed"
         :revision "v0.20.0")
    (sed_ere "https://github.com/konomanoasa/tree-sitter-sed"
             :revision "v0.20.0"
             :source-dir "sed_ere/src"))
  "Tree-sitter grammar sources for GNU and POSIX sed.")

(defun sed-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language sed-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun sed-ts-mode--editorconfig-option (properties property)
  "Return boolean PROPERTY from PROPERTIES, defaulting to nil."
  (let ((value (and properties (gethash property properties))))
    (pcase value
      ((or 'nil "false") nil)
      ("true" t)
      (_ (user-error "Invalid EditorConfig %s: %S (expected true or false)"
                     property value)))))

(defun sed-ts-mode--language ()
  "Return the Tree-sitter language selected by EditorConfig."
  (let* ((properties (when buffer-file-name
                       (editorconfig-call-get-properties-function
                        buffer-file-name)))
         (posix (sed-ts-mode--editorconfig-option properties 'posix))
         (ere (sed-ts-mode--editorconfig-option properties 'ere)))
    (if posix
        (if ere 'sed_ere 'sed)
      (if ere 'gsed_ere 'gsed))))

;;;; Incremental Updates

(defconst sed-ts-mode-update--containers
  '("command_list")
  "Container nodes traversed to find an edited source unit.")

(defvar-local sed-ts-mode-update--state nil
  "Bounds and recovery state captured before a buffer edit.")

(defvar-local sed-ts-mode-update--pending-range nil
  "Edited owner range awaiting syntax propertization.")

(defun sed-ts-mode-update--bounds (beg end)
  "Return enclosing source-unit bounds around BEG and END."
  (let ((start beg)
        (finish end)
        (root (treesit-parser-root-node treesit-primary-parser)))
    (dolist (position (list (max (point-min) (1- beg))
                            (min (point-max) end)))
      (let ((node (treesit-node-at position treesit-primary-parser))
            owner)
        (while (and node (not (treesit-node-eq node root)))
          (unless (member (treesit-node-type node)
                          sed-ts-mode-update--containers)
            (setq owner node))
          (setq node (treesit-node-parent node)))
        (when owner
          (setq start (min start (treesit-node-start owner))
                finish (max finish (treesit-node-end owner))))))
    (cons start finish)))

(defun sed-ts-mode-update--before-change (beg end)
  "Save owner bounds and old-tree recovery state before changing BEG to END."
  (when treesit-primary-parser
    (save-restriction
      (widen)
      (setq sed-ts-mode-update--state
            (cons (sed-ts-mode-update--bounds beg end)
                  (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser)
                   'has-error))))))

(defun sed-ts-mode-update--reset-parser ()
  "Replace the primary parser without reusing its old tree."
  (let* ((parser treesit-primary-parser)
         (language (treesit-parser-language parser))
         (tag (treesit-parser-tag parser))
         (ranges (treesit-parser-included-ranges parser))
         (notifiers (treesit-parser-notifiers parser))
         (level (treesit-parser-embed-level parser)))
    (treesit-parser-delete parser)
    (setq treesit-primary-parser (treesit-parser-create language nil nil tag))
    (treesit-parser-set-included-ranges treesit-primary-parser ranges)
    (treesit-parser-set-embed-level treesit-primary-parser level)
    (dolist (notifier notifiers)
      (treesit-parser-add-notifier treesit-primary-parser notifier))))

(defun sed-ts-mode-update--after-change (beg end old-length)
  "Refresh edited owners and avoid stale trees after BEG, END and OLD-LENGTH."
  (when treesit-primary-parser
    (let ((narrowed (buffer-narrowed-p)))
      (save-restriction
        (widen)
        (let* ((state sed-ts-mode-update--state)
               (reset (or (cdr state)
                          (and (= beg 1) (> old-length 0)
                               (eq (char-after 1) #xfeff)))))
          (setq sed-ts-mode-update--state nil)
          (when reset (sed-ts-mode-update--reset-parser))
          (let* ((bounds (if reset (cons (point-min) (point-max))
                           (sed-ts-mode-update--bounds beg end)))
                 (start (min (car bounds) (or (caar state) beg)))
                 (finish (min (point-max)
                              (max (cdr bounds)
                                   (+ (or (cdar state) (+ beg old-length))
                                      (- end beg old-length)))))
                 (pending sed-ts-mode-update--pending-range)
                 (pending-end (cdr pending)))
            (when (and pending-end (>= pending-end beg))
              (setq pending-end (max end (+ pending-end (- end beg old-length)))))
            (setq sed-ts-mode-update--pending-range
                  (cons (min start (or (car pending) start))
                        (min (point-max) (max finish (or pending-end finish)))))
            (syntax-ppss-flush-cache start)
            (font-lock-flush start finish)
            (when narrowed (syntax-propertize finish))))))))

(defun sed-ts-mode-update--extend-region (start end)
  "Include edited owners when propertizing START to END."
  (when sed-ts-mode-update--pending-range
    (let ((bounds sed-ts-mode-update--pending-range))
      (setq sed-ts-mode-update--pending-range nil)
      (cons (max (point-min) (min start (car bounds)))
            (min (point-max) (max end (cdr bounds)))))))

(defun sed-ts-mode-update--setup ()
  "Configure incremental updates for the current buffer."
  (add-hook 'before-change-functions #'sed-ts-mode-update--before-change nil t)
  (add-hook 'after-change-functions #'sed-ts-mode-update--after-change nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'sed-ts-mode-update--extend-region nil t))

;;;; Syntax

(defvar sed-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?' ?` ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar sed-ts-mode-syntax-table
  (let ((table (copy-syntax-table sed-ts-mode-syntax--text-table)))
    (dolist (entry '((?\( . "()") (?\) . ")(")
                     (?\[ . "(]") (?\] . ")[")
                     (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `sed-ts-mode'.")

;;;;; Syntax Queries

(defconst sed-ts-mode-syntax--queries
  (mapcar (lambda (source)
            (let ((language (car source)))
              (cons language
                    (treesit-query-compile
                     language
                     (if (memq language '(gsed gsed_ere))
                         '([(comment) (directive)] @comment
                           (block opening: "{" @brace-open)
                           (block closing: "}" @brace-close))
                       '((comment_function
                          verb: (function_verb) @comment)
                         (block_function
                          verb: (function_verb) @brace-open)
                         (block_function
                          closing: (closing_brace) @brace-close)))))))
          sed-ts-mode--grammar-sources)
  "Compiled syntax queries by language.")

;;;;; Propertization

(defun sed-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table sed-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        (alist-get (treesit-parser-language treesit-primary-parser)
                                   sed-ts-mode-syntax--queries)
                        start end))
        (put-text-property
         (treesit-node-start (cdr capture))
         (if (eq (car capture) 'comment)
             (1+ (treesit-node-start (cdr capture)))
           (treesit-node-end (cdr capture)))
         'syntax-table
         (string-to-syntax
          (pcase (car capture)
            ('comment "<")
            ('brace-open "(}")
            ('brace-close "){"))))))))

;;;;; Setup

(defun sed-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'sed-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t))

;;;; Comment Commands

(defun sed-ts-mode-comment--uncomment-region (beg end &optional arg)
  "Uncomment BEG through END using syntax classified before editing.
Pass ARG to `uncomment-region-default'."
  (syntax-propertize end)
  (unwind-protect
      (let ((syntax-propertize-function nil))
        (uncomment-region-default beg end arg))
    (syntax-ppss-flush-cache beg)))

(defun sed-ts-mode-comment--insert-comment ()
  "Insert a standalone comment at a line boundary."
  (let* ((bol (line-beginning-position))
         (position (max bol (1- (point))))
         (start
          (save-restriction
            (widen)
            (let ((command (treesit-parent-until
                            (treesit-node-at position treesit-primary-parser)
                            (lambda (node)
                              (member (treesit-node-type node)
                                      '("editing_command" "command"))))))
              (when (and command
                         (< (treesit-node-start command) bol)
                         (> (treesit-node-end command) bol)
                         (not (treesit-search-subtree
                               command
                               (lambda (node)
                                 (member (treesit-node-type node)
                                         '("block_function" "block")))
                               nil nil 2)))
                (treesit-node-start command))))))
    (when start
      (when (< start (point-min))
        (user-error "The command starts outside the accessible region"))
      (goto-char start)))
  (beginning-of-line)
  (if (looking-at-p "[ \t]*$")
      (delete-horizontal-space)
    (save-excursion (insert "\n")))
  (insert (comment-padright comment-start))
  (indent-according-to-mode))

(defun sed-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t)
  (setq-local uncomment-region-function #'sed-ts-mode-comment--uncomment-region)
  (setq-local comment-insert-comment-function #'sed-ts-mode-comment--insert-comment))

;;;; Electric Pair

(defun sed-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil for a CST block delimiter pair around the newline."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (member (treesit-node-type owner) '("block_function" "block"))
           (treesit-node-eq opening
                            (treesit-node-child-by-field-name
                             owner (if (equal (treesit-node-type owner) "block")
                                       "opening" "verb")))
           (treesit-node-eq closing (treesit-node-child-by-field-name owner "closing"))))))

(defun sed-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (sed-ts-mode-electric-pair--newline-context-p))))))

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
   '((ordinary_character) @font-lock-regexp-face
     (duplication_count) @font-lock-number-face

     (bracket_expression
      ["[" "]"] @font-lock-bracket-face)
     (nonmatching_list
      "^" @font-lock-negation-char-face)
     (start_range
      "-" @font-lock-operator-face)
     (shared_range_endpoint
      "-" @font-lock-operator-face)
     (range_expression
      "-" @font-lock-constant-face)
     (bracket_list
      "-" @font-lock-constant-face)

     [(quoted_character)
      (sed_newline_escape)
      (escaped_delimiter)] @font-lock-escape-face

     [(class_name)
      (collating_element_single)
      (collating_element_multi)
      (meta_character)] @font-lock-constant-face

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
      "=" @font-lock-punctuation-face)
     (unexpected_interval_separator
      "," @font-lock-punctuation-face))
   (pcase language
     ('sed
      '((bre_expression
         ["^" "$"] @font-lock-operator-face)
        (nondupl_bre
         ["\\(" "\\)"] @font-lock-bracket-face)
        (one_char_or_coll_elem_bre
         "." @font-lock-constant-face)
        (bre_dupl_symbol
         ["\\{" "\\}"] @font-lock-bracket-face)
        (bre_dupl_symbol
         "*" @font-lock-operator-face)
        (bre_dupl_symbol
         "," @font-lock-punctuation-face)
        (backreference) @font-lock-constant-face))
     ('sed_ere
      '((extended_reg_exp
         "|" @font-lock-operator-face)
        (ere_expression
         ["^" "$"] @font-lock-operator-face)
        (ere_expression
         ["(" ")"] @font-lock-bracket-face)
        (one_char_or_coll_elem_ere
         "." @font-lock-constant-face)
        (ere_dupl_symbol
         ["{" "}"] @font-lock-bracket-face)
        (ere_dupl_symbol
         ["*" "+" "?"] @font-lock-operator-face)
        (ere_dupl_symbol
         modifier: (repetition_modifier) @font-lock-operator-face)
        (ere_dupl_symbol
         "," @font-lock-punctuation-face)))
     (_ (user-error "Unsupported sed Tree-sitter language: %S"
                    language)))))

(defun sed-ts-mode-font-lock--posix-settings (language)
  "Return font-lock settings for LANGUAGE."
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
   '((label_literal) @font-lock-constant-face)

   :feature 'string
   '([(file_literal)
      (text_literal)
      (replacement_literal)
      (translation_literal)] @font-lock-string-face)

   :feature 'number
   '([(line_number_address)
      (occurrence_flag)] @font-lock-number-face)

   :feature 'constant
   '([(last_line_address)
      (global_flag)
      (case_insensitive_flag)
      (print_flag)
      (substitution_flag)
      (matched_text_reference)
      (replacement_backreference)] @font-lock-constant-face)

   :feature 'escape
   '([(address_escape)
      (text_backslash_escape)
      (replacement_escape)
      (translation_escape)
      (replacement_escaped_delimiter)
      (translation_escaped_delimiter)] @font-lock-escape-face)

   :feature 'operator
   '((negation_operator) @font-lock-operator-face)

   :feature 'delimiter
   '((delimiter) @font-lock-delimiter-face)

   :feature 'punctuation
   '([(text_introducer)
      (text_escaped_newline)
      (escaped_newline)] @font-lock-punctuation-face
      (address_separator) @font-lock-punctuation-face
      ((command_separator) @font-lock-punctuation-face
       (:equal @font-lock-punctuation-face ";")))

   :feature 'bracket
   '((block_function
      verb: (function_verb)
      @font-lock-bracket-face)
     (block_function
      closing: (closing_brace) @font-lock-bracket-face))

   :feature 'regexp
   (sed-ts-mode-font-lock--regexp-patterns language)))

(defun sed-ts-mode-font-lock--regexp-p (node)
  "Return non-nil when NODE belongs to a GNU regular expression."
  (treesit-parent-until node "^regular_expression$"))

(defun sed-ts-mode-font-lock--outside-regexp-p (node)
  "Return non-nil when NODE is outside a GNU regular expression."
  (not (sed-ts-mode-font-lock--regexp-p node)))

(defun sed-ts-mode-font-lock--gnu-settings (language)
  "Return GNU sed font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :default-language language

   :feature 'command
   '([(command_name) (directive_text)] @font-lock-keyword-face)

   :feature 'comment
   '([(comment_text) (directive_text)] @font-lock-comment-face)

   :feature 'label
   '((label_text) @font-lock-constant-face)

   :feature 'string
   '([(filename_text) (shell_text) (version_text)] @font-lock-string-face
     (text (literal) @font-lock-string-face)
     (replacement (literal) @font-lock-string-face)
     (translation_string (literal) @font-lock-string-face))

   :feature 'number
   '(((number) @font-lock-number-face
      (:pred sed-ts-mode-font-lock--outside-regexp-p @font-lock-number-face)))

   :feature 'constant
   '([(last_line_address) (flag) (address_modifier)
      (replacement_reference) (matched_text_reference)] @font-lock-constant-face)

   :feature 'escape
   '(([(escape) (character_escape) (case_conversion)] @font-lock-escape-face
      (:pred sed-ts-mode-font-lock--outside-regexp-p @font-lock-escape-face)))

   :feature 'operator
   '((address_operator) @font-lock-operator-face
     (negation "!" @font-lock-operator-face)
     (duplicate_negation "!" @font-lock-operator-face))

   :feature 'delimiter
   '((delimiter) @font-lock-delimiter-face)

   :feature 'punctuation
   '([(address_separator) (line_continuation)
      (text_introducer)] @font-lock-punctuation-face
      ((command_separator) @font-lock-punctuation-face
       (:equal @font-lock-punctuation-face ";")))

   :feature 'bracket
   '((block ["{" "}"] @font-lock-bracket-face)
     (unmatched_closing_brace "}" @font-lock-bracket-face))

   :feature 'regexp
   '((regex_character (literal) @font-lock-regexp-face)
     (bracket_character (literal) @font-lock-regexp-face)
     (invalid_interval_character (literal) @font-lock-regexp-face)
     (wildcard (regex_operator) @font-lock-constant-face)
     (regex_character_class (regex_operator) @font-lock-constant-face)
     [(class_name_text) (collating_text) (regex_reference)] @font-lock-constant-face
     [(regex_operator) (range_operator)] @font-lock-operator-face
     [(group_open) (group_close) (interval_open) (interval_close)
      (bracket_open) (bracket_close) (compound_marker)
      (class_open) (class_close) (collating_open) (collating_close)
      (equivalence_open) (equivalence_close)] @font-lock-bracket-face
     (interval_separator) @font-lock-punctuation-face
     ((number) @font-lock-number-face
      (:pred sed-ts-mode-font-lock--regexp-p @font-lock-number-face))
     ([(escape) (character_escape)] @font-lock-escape-face
      (:pred sed-ts-mode-font-lock--regexp-p @font-lock-escape-face)))))

;;;;; Setup

(defun sed-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              sed-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (let ((language (treesit-parser-language treesit-primary-parser)))
                (if (memq language '(gsed gsed_ere))
                    (sed-ts-mode-font-lock--gnu-settings language)
                  (sed-ts-mode-font-lock--posix-settings language)))))

;;;; Navigation

(defconst sed-ts-mode-navigation--label-regexp
  (concat "^" (regexp-opt '("label_function" "label_command")) "$")
  "Regexp matching sed label definitions.")

(defun sed-ts-mode-navigation--defun-p (node)
  "Return non-nil when NODE is a named sed label definition."
  (and (treesit-node-match-p
        node sed-ts-mode-navigation--label-regexp)
       (let ((label (treesit-node-child-by-field-name node "label")))
         (and label (equal (treesit-node-type label) "label")))))

(defconst sed-ts-mode-navigation--settings
  (let ((things '((sexp (or "^editing_command$" "^block_function$"))
                  (defun sed-ts-mode-navigation--defun-p))))
    (append (list (cons 'sed things) (cons 'sed_ere things))
            (mapcar (lambda (language)
                      (list language
                            '(sexp (or "^command$" "^block$"))
                            '(defun sed-ts-mode-navigation--defun-p)))
                    '(gsed gsed_ere))))
  "Tree-sitter thing definitions for sed.")

(defun sed-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              sed-ts-mode-navigation--settings))

;;;; Imenu

(defun sed-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (sed-ts-mode-navigation--defun-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "label") t)))

(defconst sed-ts-mode-imenu--settings
  `(("Label" ,sed-ts-mode-navigation--label-regexp
     sed-ts-mode-navigation--defun-p nil))
  "Tree-sitter Imenu settings for sed.")

(defun sed-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'sed-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              sed-ts-mode-imenu--settings))

;;;; Indentation

(defcustom sed-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'sed-ts)

(defconst sed-ts-mode-indent--rules
  (let ((rules '(((node-is "closing_brace") parent-bol 0)
                 ((n-p-gp nil "command_list" "block_function")
                  parent-bol sed-ts-mode-indent-offset)
                 ((parent-is "command_list") column-0 0))))
    (append (list (cons 'sed rules) (cons 'sed_ere rules))
            (mapcar (lambda (language)
                      (cons language
                            '(((node-is "}") parent-bol 0)
                              ((n-p-gp nil "command_list" "block")
                               parent-bol sed-ts-mode-indent-offset)
                              ((parent-is "command_list") column-0 0))))
                    '(gsed gsed_ere))))
  "Tree-sitter indentation rules for sed.")

(defun sed-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              sed-ts-mode-indent--rules))

;;;; Mode

(defun sed-ts-mode--setup ()
  "Configure `sed-ts-mode' in the current buffer."
  (let ((language (sed-ts-mode--language)))
    (sed-ts-mode--ensure-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (sed-ts-mode-syntax--setup)
  (sed-ts-mode-comment--setup)
  (sed-ts-mode-electric-pair--setup)
  (sed-ts-mode-font-lock--setup)
  (sed-ts-mode-navigation--setup)
  (sed-ts-mode-imenu--setup)
  (sed-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (sed-ts-mode-update--setup))

;;;###autoload
(define-derived-mode sed-ts-mode prog-mode "Sed-TS"
  "Major mode for editing GNU and POSIX sed."
  :syntax-table sed-ts-mode-syntax-table
  :group 'sed-ts
  (sed-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sed\\'" . sed-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("sed" . sed-ts-mode))

(provide 'sed-ts-mode)

;;; sed-ts-mode.el ends here
