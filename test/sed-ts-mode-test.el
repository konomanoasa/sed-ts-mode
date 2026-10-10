;;; sed-ts-mode-test.el --- Tests for sed-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'sed-ts-mode)

(dolist (language '(gsed gsed_ere sed sed_ere))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun sed-ts-mode-test--position (fragment &optional line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun sed-ts-mode-test--face (fragment &optional offset line)
  (get-text-property (+ (sed-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun sed-ts-mode-test--comment-p (fragment &optional offset line)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss (+ (sed-ts-mode-test--position fragment line)
                         (or offset 0)))))

(defun sed-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (sed-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun sed-ts-mode-test--should-have-faces (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (ert-info ((format "%S: %S" line fragment))
      (should (eq (sed-ts-mode-test--face fragment nil line) face)))))

(defmacro sed-ts-mode-test--with-settings (posix ere &rest body)
  (declare (indent 2) (debug (form form body)))
  `(let ((default-directory
          (file-name-as-directory (make-temp-file "sed-ts-mode-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".editorconfig")
             (insert "root = true\n[*]\nposix = " ,posix "\nere = " ,ere "\n"))
           ,@body)
       (delete-directory default-directory t))))

(defmacro sed-ts-mode-test--with-language (language &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((selected (make-symbol "language")))
    `(let ((,selected ,language))
       (ert-info ((format "Language: %s" ,selected))
         (sed-ts-mode-test--with-settings
             (if (memq ,selected '(sed sed_ere)) "true" "false")
             (if (memq ,selected '(sed_ere gsed_ere)) "true" "false")
           ,@body)))))

(defmacro sed-ts-mode-test--with-script (syntax source &rest body)
  (declare (indent 2) (debug (form form body)))
  `(sed-ts-mode-test--with-language ,syntax
     (with-temp-buffer
       (setq buffer-file-name (expand-file-name "script.sed"))
       (insert ,source)
       (sed-ts-mode)
       ,@body)))

(defun sed-ts-mode-test--face-map ()
  (let ((position (point-min)) faces)
    (while (< position (point-max))
      (push (get-text-property position 'face) faces)
      (setq position (1+ position)))
    (nreverse faces)))

(defun sed-ts-mode-test--indent (syntax source &optional offset)
  (sed-ts-mode-test--with-script syntax source
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local sed-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun sed-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun sed-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (sed-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (sed-ts-mode))
      (should (equal state (sed-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest sed-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source sed-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (sed-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest sed-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (let ((buffer-file-name nil))
              (should-error (sed-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest sed-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "p\n")
    (sed-ts-mode)
    (should (eq major-mode 'sed-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'gsed))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "script"))
    (sed-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

(ert-deftest sed-ts-mode-ensures-only-selected-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language) (push language received) t))
          (dolist (language '(gsed gsed_ere sed sed_ere))
            (setq received nil)
            (sed-ts-mode-test--with-script language "p\n"
              (should (equal received (list language))))))
      (fset 'treesit-ensure-installed ensure))))

;;;; Mode Selection

(ert-deftest sed-ts-mode-selects-editorconfig-conditions-on-file-visits ()
  (dolist (enabled '(nil t))
    (pcase-dolist (`(,posix ,ere ,language)
                   '(("false" "false" gsed) ("false" "true" gsed_ere)
                     ("true" "false" sed) ("true" "true" sed_ere)
                     ("unset" "unset" gsed)))
      (let ((file "script.sed"))
        (sed-ts-mode-test--with-settings posix ere
          (with-temp-file (expand-file-name file) (insert "/a+/p\n"))
          (let ((previous editorconfig-mode))
            (unwind-protect
                (progn
                  (editorconfig-mode (if enabled 1 -1))
                  (with-current-buffer (find-file-noselect (expand-file-name file))
                    (unwind-protect
                        (progn
                          (should (eq major-mode 'sed-ts-mode))
                          (should (eq (treesit-parser-language
                                       treesit-primary-parser) language))
                          (should (eq editorconfig-mode enabled)))
                      (kill-buffer))))
              (editorconfig-mode (if previous 1 -1)))))))))

(ert-deftest sed-ts-mode-defaults-to-gnu-bre ()
  (dolist (file '(nil "script.sed"))
    (sed-ts-mode-test--with-settings "unset" "unset"
      (with-temp-buffer
        (setq buffer-file-name (and file (expand-file-name file)))
        (insert "/a+/p\n")
        (sed-ts-mode)
        (should (eq (treesit-parser-language treesit-primary-parser) 'gsed)))))
  (sed-ts-mode-test--with-language 'sed_ere
    (with-temp-buffer
      (sed-ts-mode)
      (should (eq (treesit-parser-language treesit-primary-parser) 'gsed)))))

(ert-deftest sed-ts-mode-rejects-invalid-editorconfig-conditions ()
  (dolist (property '(posix ere))
    (dolist (value '("1" "yes" "TRUE" "bre" "invalid"))
      (sed-ts-mode-test--with-settings
          (if (eq property 'posix) value "false")
          (if (eq property 'ere) value "false")
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "script.sed"))
          (let ((error-data (should-error (sed-ts-mode) :type 'user-error)))
            (should (equal (error-message-string error-data)
                           (format "Invalid EditorConfig %s: %S (expected true or false)"
                                   property value))))
          (should-not (treesit-parser-list nil nil t)))))))

(ert-deftest sed-ts-mode-reloads-editorconfig-only-on-mode-restart ()
  (let ((treesit-font-lock-level 4))
    (sed-ts-mode-test--with-script 'gsed "/a+/p\n"
      (pcase-dolist (`(,posix ,ere ,language ,face)
                     '(("true" "true" sed_ere font-lock-operator-face)
                       ("false" "true" gsed_ere font-lock-operator-face)
                       ("true" "false" sed font-lock-regexp-face)
                       ("false" "false" gsed font-lock-regexp-face)))
        (let ((parser treesit-primary-parser))
          (with-temp-file (expand-file-name ".editorconfig")
            (insert "root = true\n[*]\nposix = " posix "\nere = " ere "\n"))
          (font-lock-flush)
          (font-lock-ensure)
          (should (eq treesit-primary-parser parser))
          (sed-ts-mode)
          (font-lock-ensure)
          (should (eq (treesit-parser-language treesit-primary-parser) language))
          (should (equal (treesit-parser-list) (list treesit-primary-parser)))
          (should (eq (sed-ts-mode-test--face "+") face)))))))

(ert-deftest sed-ts-mode-selects-files ()
  (pcase-dolist (`(,file ,selected)
                 '(("example.sed" t) ("example.gsed" nil)))
    (sed-ts-mode-test--with-settings "unset" "unset"
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name file))
        (set-auto-mode)
        (should (eq (eq major-mode 'sed-ts-mode) selected))
        (when selected
          (should (eq (treesit-parser-language treesit-primary-parser) 'gsed)))))))

(ert-deftest sed-ts-mode-selects-interpreters-without-interpreting-options ()
  (should (eq (alist-get "sed" interpreter-mode-alist nil nil #'equal)
              'sed-ts-mode))
  (pcase-dolist (`(,interpreter ,selected) '(("sed" t) ("gsed" nil)))
    (dolist (prefix '("#!/usr/bin/" "#!/usr/bin/env " "#!/usr/bin/env -S "))
      (sed-ts-mode-test--with-settings "unset" "unset"
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "script"))
          (insert prefix interpreter " -E --posix -f\np\n")
          (set-auto-mode)
          (should (eq (eq major-mode 'sed-ts-mode) selected))
          (when selected
            (should (eq (treesit-parser-language treesit-primary-parser) 'gsed))))))))

(ert-deftest sed-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "sed-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "sed-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'sed-ts-mode" "(add-to-list 'auto-mode-alist" "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest sed-ts-mode-classifies-gnu-delimiters-and-source-escapes ()
  (pcase-dolist (`(,language ,regexp)
                 '((gsed "s/\\(a[bc]\\)\\{2\\}/x/")
                   (gsed_ere "s/(a[bc]){2}/x/")))
    (sed-ts-mode-test--with-script language (concat "{\n" regexp "\n}\n")
      (should (= (sed-ts-mode-test--syntax-class "{") 4))
      (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))
      (should (= (scan-sexps 1 1) (1+ (sed-ts-mode-test--position "}" "}"))))
      (dotimes (offset (length regexp))
        (let ((position (+ 3 offset)))
          (when (memq (char-after position) '(?\( ?\) ?\[ ?\] ?{ ?} ?\\))
            (should (= (syntax-class (syntax-after position)) 1)))))))
  (dolist (language '(gsed gsed_ere))
    (sed-ts-mode-test--with-script language "s/\\x5ba\\x5d/\\U&/\n"
      (syntax-propertize (point-max))
      (should (= (sed-ts-mode-test--syntax-class "\\x5b") 1))
      (should-not (nth 3 (syntax-ppss (point-max)))))))

(ert-deftest sed-ts-mode-classifies-delimiters ()
  (dolist (case
           '((sed "s/\\(a[bc]\\)\\{2\\}/x/"
                  ("\\(" "\\)" "[" "]" "\\{" "\\}"))
             (sed_ere "s/(a[bc]){2}/x/"
                      ("(" ")" "[" "]" "{" "}"))))
    (pcase-let ((`(,syntax ,regexp ,delimiters) case))

      (sed-ts-mode-test--with-script syntax
          (concat "{\n" regexp "\n}\n")
        (dolist (pair '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
          (should (eq (matching-paren (car pair)) (cdr pair))))
        (should (= (sed-ts-mode-test--syntax-class "{" nil "{") 4))
        (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))
        (should (= (scan-sexps
                    (sed-ts-mode-test--position "{" "{") 1)
                   (1+ (sed-ts-mode-test--position "}" "}"))))
        (dolist (delimiter delimiters)
          (should
           (= (sed-ts-mode-test--syntax-class delimiter nil regexp)
              1)))))))

(ert-deftest sed-ts-mode-classifies-comments ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))

    (sed-ts-mode-test--with-script syntax
        (concat
         "#n directive\n"
         "#\n"
         "# plain\n"
         "p;# trailing\n"
         "s/#/x/\n"
         "s/x/#/\n"
         "y/#/x/\n"
         "a\\\n"
         "# text\n"
         "r #file\n"
         "r \"file\n"
         "# after-quote\n")
      (pcase-dolist (`(,line ,fragment ,offset)
                     '(("#n directive" "#" 1)
                       ("#" "#" 1)
                       ("# plain" "#" 1)
                       ("p;# trailing" "#" 1)
                       ("# after-quote" "#" 1)))
        (should (sed-ts-mode-test--comment-p fragment offset line)))
      (pcase-dolist (`(,line ,fragment ,offset)
                     '(("s/#/x/" "#" 1)
                       ("s/x/#/" "#" 1)
                       ("y/#/x/" "#" 1)
                       ("# text" "#" 1)
                       ("r #file" "#" 1)))
        (should-not (sed-ts-mode-test--comment-p fragment offset line))))
    (dolist (source '("# note" "#"))
      (sed-ts-mode-test--with-script syntax source
        (syntax-propertize (point-max))
        (should (nth 4 (syntax-ppss (point-max))))))))

(ert-deftest sed-ts-mode-keeps-operand-quotes-and-escapes-as-punctuation ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (sed-ts-mode-test--with-script syntax "r file'\"`\\()[]{}\n"
      (syntax-propertize (point-max))
      (dolist (character '(?' ?\" ?` ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
        (let ((position (sed-ts-mode-test--position (char-to-string character))))
          (should (= (syntax-class (syntax-after position)) 1))
          (should-not (nth 3 (syntax-ppss (1+ position)))))))))

;;;; Comment Commands

(ert-deftest sed-ts-mode-inserts-comments-before-gnu-shell-operands ()
  (dolist (language '(gsed gsed_ere))
    (sed-ts-mode-test--with-script language "{\n  e echo one\\\n    two\n}\n"
      (goto-char (sed-ts-mode-test--position "two"))
      (comment-dwim nil)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "{\n  # \n  e echo one\\\n    two\n}\n")))))

(ert-deftest sed-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "p\n")
    (sed-ts-mode)
    (comment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# p\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "p\n"))))

(ert-deftest sed-ts-mode-uncomments-multiline-source ()
  (dolist (case
           '(("a\\\n# text\n" "# a\\\n# # text\n")
             ("s/a/one\\\ntwo/\n" "# s/a/one\\\n# two/\n")))
    (dolist (syntax '(gsed gsed_ere sed sed_ere))
      (sed-ts-mode-test--with-script syntax (car case)
        (let ((state (sed-ts-mode-test--buffer-state)))
          (comment-or-uncomment-region (point-min) (point-max))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         (cadr case)))
          (comment-or-uncomment-region (point-min) (point-max))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         (car case)))
          (should (equal (sed-ts-mode-test--buffer-state) state)))))))

(ert-deftest sed-ts-mode-inserts-standalone-comments ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (pcase-dolist (`(,source ,line ,expected)
                   '(("p" 1 "# \np")
                     ("{\n  p\n}\n" 2 "{\n  # \n  p\n}\n")
                     ("a\\\ntext\n" 2 "# \na\\\ntext\n")
                     ("s/a/x\\\ny/\n" 2 "# \ns/a/x\\\ny/\n")
                     ("{\n  a\\\ntext\n}\n" 3 "{\n  # \n  a\\\ntext\n}\n")
                     ("{\n  p\n}\n" 3 "{\n  p\n  # \n}\n")
                     ("" 1 "# ")
                     ("  " 1 "# ")))
      (sed-ts-mode-test--with-script syntax source
        (setq-local indent-tabs-mode nil)
        (goto-char (point-min))
        (forward-line (1- line))
        (end-of-line)
        (call-interactively (key-binding (kbd "M-;")))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       expected))
        (should (nth 4 (syntax-ppss))))))
  (sed-ts-mode-test--with-script 'sed "a\\\ntext\n"
    (narrow-to-region 4 (point-max))
    (goto-char (point-max))
    (forward-char -1)
    (should-error (comment-dwim nil) :type 'user-error)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "text\n"))))

;;;; Electric Pair

(ert-deftest sed-ts-mode-supplies-electric-pairs ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil))
    (dolist (syntax '(gsed gsed_ere sed sed_ere))
      (pcase-dolist (`(,prefix ,opening ,expected)
                     '(("" ?{ "{}")
                       ("s/" ?\( "s/()")
                       ("s/" ?\[ "s/[]")))
        (ert-info ((format "%S / %c" prefix opening))
          (sed-ts-mode-test--with-script syntax prefix

            (should-not electric-pair-mode)
            (should (local-variable-p 'electric-pair-pairs))
            (should (equal (assq ?% electric-pair-pairs) '(?% . ?%)))
            (electric-pair-local-mode 1)
            (let ((last-command-event opening)) (self-insert-command 1))
            (should (equal (buffer-string) expected))
            (should (= (point) (1- (point-max))))))))
    (should (equal electric-pair-pairs '((?% . ?%)))))
  (let ((electric-pair-pairs '((?{ . ?>))))
    (dolist (syntax '(gsed gsed_ere sed sed_ere))
      (pcase-dolist (`(,prefix ,opening ,expected)
                     '(("" ?{ "{>")))
        (sed-ts-mode-test--with-script syntax prefix

          (electric-pair-local-mode 1)
          (let ((last-command-event opening)) (self-insert-command 1))
          (should (equal (buffer-string) expected))
          (should (= (point) (1- (point-max)))))))
    (should (equal electric-pair-pairs '((?{ . ?>))))))

(ert-deftest sed-ts-mode-respects-pair-newline-preferences ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (let ((calls 0))
      (pcase-dolist (`(,setting ,expected)
                     (list (list nil "{\n}")
                           (list t "{\n\n}")
                           (list (lambda () (setq calls (1+ calls)) nil) "{\n}")
                           (list (lambda () (setq calls (1+ calls)) t) "{\n\n}")))
        (setq calls 0)
        (let ((electric-pair-open-newline-between-pairs setting))
          (sed-ts-mode-test--with-script syntax "{}"

            (electric-indent-local-mode -1)
            (electric-pair-local-mode 1)
            (goto-char 2)
            (call-interactively (key-binding (kbd "RET")))
            (should (equal (buffer-string) expected)))
          (should (eq (> calls 0) (functionp setting)))
          (should (eq electric-pair-open-newline-between-pairs setting)))))))

(ert-deftest sed-ts-mode-pairs-delimiters-and-indents-on-return ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (pcase-dolist (`(,prefix ,suffix ,offset ,expand ,expected ,column)
                   '(("1,2" "" 2 t "1,2{\n  \n}" 2)
                     ("" "" 4 t "{\n    \n}" 4)
                     ("{\n  /x/" "\n}" 2 t "{\n  /x/{\n    \n  }\n}" 4)
                     ("1,2" "" 2 nil "1,2{\n}" 0)))
      (ert-info ((format "%s, expand %S, offset %s: %S" syntax expand offset prefix))
        (let ((electric-pair-open-newline-between-pairs expand))
          (sed-ts-mode-test--with-script syntax (concat prefix suffix)
            (setq-local indent-tabs-mode nil)
            (setq-local sed-ts-mode-indent-offset offset)
            (electric-indent-local-mode 1)
            (electric-pair-local-mode 1)
            (goto-char (1+ (length prefix)))
            (let ((last-command-event ?{))
              (self-insert-command 1))
            (call-interactively (key-binding (kbd "RET")))
            (should (equal (buffer-string) expected))
            (should (= (current-column) column))
            (when expand
              (should (eolp))
              (should (= (line-number-at-pos)
                         (1+ (length (split-string prefix "\n"))))))))))))

(ert-deftest sed-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (let ((electric-pair-open-newline-between-pairs t))
    (dolist (syntax '(gsed gsed_ere sed sed_ere))
      (pcase-dolist (`(,before ,after ,expected)
                     '(("{" "}" "{\n\n}")
                       ("1,3!{" "}" "1,3!{\n\n}")
                       ("s/{" "}/x/\n" "s/{\n}/x/\n")
                       ("s/(" ")/x/\n" "s/(\n)/x/\n")
                       ("s/[" "]/x/\n" "s/[\n]/x/\n")
                       ("s/x/{" "}/\n" "s/x/{\n}/\n")
                       ("a\\\n{" "}\n" "a\\\n{\n}\n")
                       ("r file{" "}\n" "r file{\n}\n")
                       (":label{" "}\n" ":label{\n}\n")
                       ("# {" "}\n" "# {\n}\n")))
        (ert-info ((format "%s: %S / %S" syntax before after))
          (sed-ts-mode-test--with-script syntax (concat before after)
            (electric-indent-local-mode -1)
            (electric-pair-local-mode 1)
            (goto-char (1+ (length before)))
            (call-interactively (key-binding (kbd "RET")))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           expected))))))))

;;;; Font Lock

(ert-deftest sed-ts-mode-fontifies-gnu-extensions-by-level-and-context ()
  (dolist (language '(gsed gsed_ere))
    (dolist (level '(1 2 3 4))
      (let ((treesit-font-lock-level level))
        (sed-ts-mode-test--with-script language
            (concat "#n note\n:again\n1~2p\n2,+3d\n$=\n/a/IM!p\n"
                    "s/a/\\U&\\E\\1\\n/2gpew output\n"
                    "s/\\x5ba\\x5d/str/\n"
                    "e echo\\n\nv 4.10\nR input\nQ 7\nT again\nW output\nz\nF\n")
          (font-lock-ensure)
          (pcase-dolist (`(,line ,fragment ,minimum ,face)
                         '(("#n note" "note" 1 font-lock-comment-face)
                           (":again" ":" 2 font-lock-keyword-face)
                           (":again" "again" 2 font-lock-constant-face)
                           ("1~2p" "1" 3 font-lock-number-face)
                           ("1~2p" "~" 4 font-lock-operator-face)
                           ("2,+3d" "," 4 font-lock-punctuation-face)
                           ("2,+3d" "+" 4 font-lock-operator-face)
                           ("$=" "$" 3 font-lock-constant-face)
                           ("/a/IM!p" "I" 3 font-lock-constant-face)
                           ("/a/IM!p" "M" 3 font-lock-constant-face)
                           ("/a/IM!p" "!" 4 font-lock-operator-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "\\U" 3 font-lock-escape-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "&" 3 font-lock-constant-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "\\1" 3 font-lock-constant-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "\\n" 3 font-lock-escape-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "2" 3 font-lock-number-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "ew output" 3 font-lock-constant-face)
                           ("s/a/\\U&\\E\\1\\n/2gpew output" "output" 2 font-lock-string-face)
                           ("s/\\x5ba\\x5d/str/" "\\x5b" 4 font-lock-escape-face)
                           ("s/\\x5ba\\x5d/str/" "str" 2 font-lock-string-face)
                           ("e echo\\n" "e" 2 font-lock-keyword-face)
                           ("e echo\\n" "echo" 2 font-lock-string-face)
                           ("e echo\\n" "\\n" 3 font-lock-escape-face)
                           ("v 4.10" "4.10" 2 font-lock-string-face)
                           ("R input" "R" 2 font-lock-keyword-face)
                           ("R input" "input" 2 font-lock-string-face)
                           ("Q 7" "7" 3 font-lock-number-face)
                           ("T again" "T" 2 font-lock-keyword-face)
                           ("W output" "W" 2 font-lock-keyword-face)
                           ("z" "z" 2 font-lock-keyword-face)
                           ("F" "F" 2 font-lock-keyword-face)))
            (ert-info ((format "Level %s: %s / %s" level line fragment))
              (should (eq (sed-ts-mode-test--face fragment nil line)
                          (and (>= level minimum) face))))))))))

(ert-deftest sed-ts-mode-fontifies-gnu-regexp-leaves-at-level-four ()
  (pcase-dolist (`(,language ,group ,interval ,alternation)
                 '((gsed "\\(a\\)" "a\\{2,3\\}" "a\\|b")
                   (gsed_ere "(a)" "a{2,3}" "a|b")))
    (dolist (level '(1 2 3 4))
      (let ((treesit-font-lock-level level))
        (sed-ts-mode-test--with-script language
            (concat "s/" group interval alternation ".\\w\\b\\1[^a-z[:alpha:]]/x/\n")
          (font-lock-ensure)
          (pcase-dolist (`(,fragment ,offset ,face)
                         `((,group 0 font-lock-bracket-face)
                           (,interval 1 font-lock-bracket-face)
                           ("2,3" 0 font-lock-number-face)
                           ("," 0 font-lock-punctuation-face)
                           (,alternation 1 font-lock-operator-face)
                           ("." 0 font-lock-constant-face)
                           ("\\w" 0 font-lock-constant-face)
                           ("\\b" 0 font-lock-operator-face)
                           ("\\1" 0 font-lock-constant-face)
                           ("[^" 0 font-lock-bracket-face)
                           ("[^" 1 font-lock-operator-face)
                           ("a-z" 1 font-lock-operator-face)
                           ("[:" 0 font-lock-bracket-face)
                           ("[:" 1 font-lock-bracket-face)
                           ("alpha" 0 font-lock-constant-face)))
            (ert-info ((format "Level %s: %S" level fragment))
              (should (eq (sed-ts-mode-test--face fragment offset)
                          (and (= level 4) face))))))))))

(ert-deftest sed-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (with-temp-buffer
      (insert "#n note\n:label\ns/a/b/g\n")
      (let ((treesit-font-lock-level level)) (sed-ts-mode))
      (font-lock-ensure)
      (pcase-dolist (`(,fragment ,minimum ,face)
                     '(("note" 1 font-lock-comment-face) ("label" 2 font-lock-constant-face)
                       ("b/g" 2 font-lock-string-face) ("g" 3 font-lock-constant-face)
                       ("a/b" 4 font-lock-regexp-face)))
        (ert-info ((format "Level %s: %S" level fragment))
          (should (eq (sed-ts-mode-test--face fragment) (and (>= level minimum) face)))))
      (should (eq (sed-ts-mode-test--face "#n")
                  (if (= level 1) 'font-lock-comment-face 'font-lock-keyword-face))))))

(ert-deftest sed-ts-mode-fontifies-each-command-verb ()
  (let ((treesit-font-lock-level 4)
        (commands '("a\\" "b target" "c\\" "d" "D" "g" "G" "h" "H" "i\\" "l" "n"
                    "N" "p" "P" "q" "r input.txt" "s/a/b/" "t target"
                    "w output.txt" "x" "y/a/b/" ":target" "=")))
    (sed-ts-mode-test--with-script 'sed
        (concat (mapconcat (lambda (command)
                             (if (string-suffix-p "\\" command)
                                 (concat command "\ntext\n")
                               (concat command "\n")))
                           commands)
                "{\n}\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-have-faces
       (append
        (mapcar (lambda (command)
                  (list command (substring command 0 1)
                        'font-lock-keyword-face))
                commands)
        '(("{" "{" font-lock-bracket-face)
          ("}" "}" font-lock-bracket-face)))))))

(ert-deftest sed-ts-mode-fontifies-language-syntax ()
  (let* ((treesit-font-lock-level 4)
         (substitution "s/^[[:alnum:]]*$/[&]/gipw substitutions.out"))
    (sed-ts-mode-test--with-script 'sed
        (concat "# Addresses, arguments and data\n"
                "1,10p\n"
                "$=\n"
                "/^skip$/!d\n"
                "\\%^start$%p\n"
                substitution "\n"
                "s/a/b/2\n"
                "s/a/\\1/\n"
                "s/a/\\0/\n"
                "s/a/one\\\n"
                "two/\n"
                "y/a\\nc/xyz/\n"
                ":again\n"
                "b again\n"
                "a\\\n"
                "appended\\\n"
                "text\n"
                "i\\\n"
                "back\\\\slash\n"
                "r input.txt\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-have-faces
       `(("# Addresses, arguments and data" "#" font-lock-comment-face)
         ("# Addresses, arguments and data" "Addresses" font-lock-comment-face)
         ("1,10p" "1" font-lock-number-face)
         ("1,10p" "," font-lock-punctuation-face)
         ("$=" "$" font-lock-constant-face)
         ("/^skip$/!d" "/" font-lock-delimiter-face)
         ("/^skip$/!d" "^" font-lock-operator-face)
         ("/^skip$/!d" "!" font-lock-operator-face)
         ("\\%^start$%p" "\\" font-lock-escape-face)
         (,substitution "g" font-lock-constant-face)
         (,substitution "i" font-lock-constant-face)
         (,substitution "p" font-lock-constant-face)
         (,substitution "w" font-lock-constant-face)
         (,substitution "substitutions.out" font-lock-string-face)
         (,substitution "&" font-lock-constant-face)
         ("s/a/b/2" "b" font-lock-string-face)
         ("s/a/b/2" "2" font-lock-number-face)
         ("s/a/\\1/" "\\1" font-lock-constant-face)
         ("s/a/\\0/" "\\0" font-lock-constant-face)
         ("s/a/one\\" "\\" font-lock-punctuation-face)
         ("y/a\\nc/xyz/" "\\n" font-lock-escape-face)
         ("y/a\\nc/xyz/" "xyz" font-lock-string-face)
         (":again" "again" font-lock-constant-face)
         ("b again" "again" font-lock-constant-face)
         ("a\\" "\\" font-lock-punctuation-face)
         ("appended\\" "appended" font-lock-string-face)
         ("appended\\" "\\" font-lock-punctuation-face)
         ("text" "text" font-lock-string-face)
         ("back\\\\slash" "\\\\" font-lock-escape-face)
         ("r input.txt" "input.txt" font-lock-string-face)))
      (dolist (line '("s/a/one\\" "a\\" "appended\\"))
        (let ((backslash (sed-ts-mode-test--position "\\" line)))
          (should (eq (get-text-property (1+ backslash) 'face)
                      'font-lock-punctuation-face)))))))

(ert-deftest sed-ts-mode-fontifies-compound-bracket-delimiters ()
  (dolist (entry '((sed . sed) (sed_ere . sed_ere)))

    (let ((line "/[[.x.][=y=][:alpha:]]/p")
          (treesit-font-lock-level 4))
      (sed-ts-mode-test--with-script (car entry) (concat line "\n")
        (font-lock-ensure)
        (dolist (expectation
                 '(("[." 0 font-lock-bracket-face)
                   ("[." 1 font-lock-punctuation-face)
                   (".]" 0 font-lock-punctuation-face)
                   (".]" 1 font-lock-bracket-face)
                   ("[=" 0 font-lock-bracket-face)
                   ("[=" 1 font-lock-punctuation-face)
                   ("=]" 0 font-lock-punctuation-face)
                   ("=]" 1 font-lock-bracket-face)
                   ("[:" 0 font-lock-bracket-face)
                   ("[:" 1 font-lock-punctuation-face)
                   (":]" 0 font-lock-punctuation-face)
                   (":]" 1 font-lock-bracket-face)))
          (should
           (eq (sed-ts-mode-test--face (car expectation) (nth 1 expectation) line)
               (nth 2 expectation))))
        (dolist (payload '("x" "y" "alpha"))
          (should (eq (sed-ts-mode-test--face payload nil line)
                      'font-lock-constant-face)))))))

(ert-deftest sed-ts-mode-fontifies-bre-components ()
  (let* ((treesit-font-lock-level 4)
         (expression "/^a\\.\\([[:alpha:]x-z]\\{2,3\\}\\)\\1.*$/p")
         (bracket "/[[.hyphen.][=e=][:digit:]a--]/p")
         (collating-symbol "/[[.^.]]/p")
         (negated-list "/[^a]/p")
         (trailing-hyphen "/[a-]/p")
         (escaped-delimiter "/a\\/b/p")
         (control-escape "/a\\nb/p")
         (extensions "/a\\|b\\+c\\?/p"))
    (sed-ts-mode-test--with-script 'sed
        (concat expression "\n"
                bracket "\n"
                collating-symbol "\n"
                negated-list "\n"
                trailing-hyphen "\n"
                escaped-delimiter "\n"
                control-escape "\n"
                extensions "\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-have-faces
       `((,expression "/" font-lock-delimiter-face)
         (,expression "^" font-lock-operator-face)
         (,expression "a" font-lock-regexp-face)
         (,expression "\\." font-lock-escape-face)
         (,expression "\\(" font-lock-bracket-face)
         (,expression "[" font-lock-bracket-face)
         (,expression "[:alpha" font-lock-bracket-face)
         (,expression "alpha" font-lock-constant-face)
         (,expression ":]x" font-lock-punctuation-face)
         (,expression "x" font-lock-constant-face)
         (,expression "-" font-lock-operator-face)
         (,expression "\\{" font-lock-bracket-face)
         (,expression "2" font-lock-number-face)
         (,expression "," font-lock-punctuation-face)
         (,expression "\\}" font-lock-bracket-face)
         (,expression "\\)" font-lock-bracket-face)
         (,expression "\\1" font-lock-constant-face)
         (,expression ".*" font-lock-constant-face)
         (,expression "*" font-lock-operator-face)
         (,bracket "[." font-lock-bracket-face)
         (,bracket "hyphen" font-lock-constant-face)
         (,bracket ".]" font-lock-punctuation-face)
         (,bracket "[=" font-lock-bracket-face)
         (,bracket "e=" font-lock-constant-face)
         (,bracket "=]" font-lock-punctuation-face)
         (,bracket "[:digit" font-lock-bracket-face)
         (,bracket ":]" font-lock-punctuation-face)
         (,bracket "-]" font-lock-constant-face)
         (,bracket "]/p" font-lock-bracket-face)
         (,collating-symbol "^" font-lock-constant-face)
         (,negated-list "^" font-lock-negation-char-face)
         (,trailing-hyphen "-]" font-lock-constant-face)
         (,escaped-delimiter "\\/" font-lock-escape-face)
         (,control-escape "\\n" font-lock-escape-face)
         (,extensions "\\|" nil)
         (,extensions "\\+" nil)
         (,extensions "\\?" nil))))))

(ert-deftest sed-ts-mode-fontifies-ere-components ()
  (let* ((treesit-font-lock-level 4)
         (expression "/^a\\.(a|[^[:alpha:]x-z]{2,3})+?.*$/p")
         (optional "/a?b/p"))
    (sed-ts-mode-test--with-script 'sed_ere
        (concat expression "\n" optional "\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-have-faces
       `((,expression "a" font-lock-regexp-face)
         (,expression "\\." font-lock-escape-face)
         (,expression "(" font-lock-bracket-face)
         (,expression "|" font-lock-operator-face)
         (,expression "[" font-lock-bracket-face)
         (,expression "^" font-lock-operator-face)
         (,expression "[:alpha" font-lock-bracket-face)
         (,expression "alpha" font-lock-constant-face)
         (,expression ":]x" font-lock-punctuation-face)
         (,expression "-" font-lock-operator-face)
         (,expression "{" font-lock-bracket-face)
         (,expression "2" font-lock-number-face)
         (,expression "}" font-lock-bracket-face)
         (,expression ")" font-lock-bracket-face)
         (,expression "+" font-lock-operator-face)
         (,expression "?" font-lock-operator-face)
         (,expression ".*" font-lock-constant-face)
         (,expression "$" font-lock-operator-face)
         (,optional "?" font-lock-operator-face)))
      (should (eq (sed-ts-mode-test--face "[^" 1 expression)
                  'font-lock-negation-char-face)))))

(ert-deftest sed-ts-mode-fontifies-source-leaves-within-syntax-issues ()
  (dolist (syntax '(sed sed_ere))
    (let ((treesit-font-lock-level 4))
      (pcase-dolist (`(,source ,fragment ,face)
                     `((,(if (eq syntax 'sed) "/a\\{1,,2\\}/p\n" "/a{1,,2}/p\n")
                        ",," font-lock-punctuation-face)
                       ("/[a-c-e]/p\n" "-e" font-lock-operator-face)
                       (":abcdefghi\n" "abcdefghi" font-lock-constant-face)
                       (":aαb\n" "α" font-lock-constant-face)))
        (ert-info ((format "%S: %S" syntax source))
          (sed-ts-mode-test--with-script syntax source
            (font-lock-ensure)
            (should (eq (sed-ts-mode-test--face fragment) face))
            (when (equal fragment ",,")
              (should (eq (sed-ts-mode-test--face fragment 1) face)))
            (should (treesit-query-capture
                     (treesit-buffer-root-node syntax)
                     '((syntax_issue) @issue)))))))))

(ert-deftest sed-ts-mode-fontifies-only-written-command-separators ()
  (dolist (syntax '(sed sed_ere))

    (let ((treesit-font-lock-level 4))
      (sed-ts-mode-test--with-script syntax "2p;3d\n  ;;  ;p; \n"
        (font-lock-ensure)
        (goto-char (point-min))
        (while (not (eobp))
          (should (eq (get-text-property (point) 'face)
                      (pcase (char-after)
                        (?\; 'font-lock-punctuation-face)
                        ((or ?2 ?3) 'font-lock-number-face)
                        ((or ?p ?d) 'font-lock-keyword-face)
                        (_ nil))))
          (forward-char 1))))))

(ert-deftest sed-ts-mode-distinguishes-regexp-control-and-general-escapes ()
  (let ((treesit-font-lock-level 4)
        (control "s/\\n/\\&/")
        (replacement "s/a/x\\/y/")
        (translation "y/abc/x\\/z/"))
    (sed-ts-mode-test--with-script 'sed
        (concat control "\n" replacement "\n" translation "\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-have-faces
       `((,control "\\n" font-lock-escape-face)
         (,control "\\&" font-lock-escape-face)
         (,replacement "\\/" font-lock-escape-face)
         (,translation "\\/" font-lock-escape-face))))))

(ert-deftest sed-ts-mode-captures-only-leaves ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))

    (sed-ts-mode-test--with-language syntax
      (dolist (source '("#n note\n{\ns/[[:alpha:]]*/text/g\n}\n:again\nr input\nw output\n"
                        "a\\\ntext\ns/a/\\1\\n&/\ny/a/b/\n"
                        ":a\000b\nb a\000b\nt a\000b\nr a\000b\nw a\000b\ns/a/b/w a\000b\n"))
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "script.sed"))
          (insert source)
          (let ((treesit-font-lock-level 4)) (sed-ts-mode))
          (let ((root (treesit-parser-root-node treesit-primary-parser)))
            (should-not (treesit-node-check root 'has-error))
            (dolist (setting treesit-font-lock-settings)
              (dolist (capture (treesit-query-capture root (car setting)))
                (ert-info ((treesit-node-type (cdr capture)))
                  (should (= (treesit-node-child-count (cdr capture)) 0)))))))))))

;;;; Navigation

(ert-deftest sed-ts-mode-navigates-gnu-commands-and-block-boundaries ()
  (dolist (language '(gsed gsed_ere))
    (pcase-dolist (`(,prefix ,unit ,suffix)
                   '(("p;  " "1~2!d" "\n")
                     ("p\n" "0,/end/{\ne echo;cmd\nQ 4\n}" "\n")
                     ("{\n" "2,+3{\nz\n}" "\n}\n")
                     ("p\n" "a inline\\\nnext" "\nd\n")
                     ("p\n" "e echo;command" "\nd\n")
                     ("p\n" ":again" ";T again\n")))
      (sed-ts-mode-test--with-script language (concat prefix unit suffix)
        (goto-char (1+ (length prefix)))
        (forward-sexp)
        (should (= (point) (1+ (+ (length prefix) (length unit)))))
        (backward-sexp)
        (should (= (point) (1+ (length prefix))))))
    (sed-ts-mode-test--with-script language "  1~2!{\nz\n}\n"
      (goto-char (sed-ts-mode-test--position "{"))
      (forward-sexp)
      (should (= (point) (1+ (sed-ts-mode-test--position "}"))))
      (backward-sexp)
      (should (= (point) 3)))
    (sed-ts-mode-test--with-script language "s/abc/def/\ne echo hi\nR input\n"
      (dolist (fragment '("abc" "def" "echo" "input"))
        (goto-char (sed-ts-mode-test--position fragment))
        (should (equal (treesit-node-type (treesit-thing-at-point 'sexp 'nested))
                       "command"))))))

(ert-deftest sed-ts-mode-navigates-commands-and-blocks ()
  (dolist (syntax '(sed sed_ere))
    (pcase-dolist (`(,prefix ,unit ,suffix)
                   '(("" "p" ";d\n")
                     ("p;" "  1,3!d" "\n")
                     ("p\n" "/start/,/end/!s/a/b/g" "\nd\n")
                     ("p\n" "1,3{\np\n2{\nd\n}\n}" "\nq\n")
                     ("{\np\n" "2{\nd\n}" "\n}\n")
                     ("{\np\n2{\n" "d" "\n}\n}\n")
                     ("p\n" "a\\\ntext" "\nd\n")
                     ("p\n" "y/ab/cd/" "\nd\n")
                     ("p\n" "w output;name" "\nd\n")
                     ("p\n" ":again" "\nb again\n")))
      (ert-info ((format "%S: %S" syntax unit))
        (sed-ts-mode-test--with-script
            syntax (concat prefix unit suffix)
          (should-not (treesit-node-check
                       (treesit-parser-root-node treesit-primary-parser) 'has-error))
          (should (eq forward-sexp-function #'treesit-forward-sexp))
          (let ((start (1+ (length prefix)))
                (end (1+ (+ (length prefix) (length unit)))))
            (goto-char start)
            (forward-sexp)
            (should (= (point) end))
            (backward-sexp)
            (should (= (point) start))))))))

(ert-deftest sed-ts-mode-navigates-label-definitions ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (sed-ts-mode-test--with-script
        syntax "p\n:first\ns/a/b/\nb first\n:last\nt first\n:\nd\n"
      (goto-char (point-max))
      (beginning-of-defun)
      (should (= (point) (sed-ts-mode-test--position ":last")))
      (beginning-of-defun)
      (should (= (point) (sed-ts-mode-test--position ":first")))
      (end-of-defun)
      (should (= (point) (sed-ts-mode-test--position "s/a/b/")))
      (end-of-defun)
      (should (= (point) (sed-ts-mode-test--position "t first")))
      (goto-char (sed-ts-mode-test--position ":first"))
      (let ((node (treesit-thing-at-point 'defun 'top-level)))
        (should (equal (treesit-node-text node t) ":first"))
        (should (equal (treesit-defun-name node) "first"))))))

(ert-deftest sed-ts-mode-updates-navigation-after-edits ()
  (dolist (syntax '(sed sed_ere))
    (sed-ts-mode-test--with-script
        syntax "p;;\n# note\nd\n:old\np\n"
      (goto-char (point-min))
      (forward-sexp 3)
      (should (= (point) (1+ (sed-ts-mode-test--position "d\n"))))
      (backward-sexp 3)
      (should (= (point) (point-min)))
      (goto-char (sed-ts-mode-test--position ":old"))
      (delete-region (point) (+ (point) 4))
      (insert ":new")
      (goto-char (point-max))
      (beginning-of-defun)
      (should (= (point) (sed-ts-mode-test--position ":new")))
      (should (equal (treesit-defun-name (treesit-thing-at-point 'defun 'top-level)) "new")))))

(ert-deftest sed-ts-mode-navigates-shared-block-boundaries ()
  (dolist (syntax '(sed sed_ere))
    (sed-ts-mode-test--with-script syntax "  1,3!{\np\n}\n"
      (goto-char (sed-ts-mode-test--position "{"))
      (forward-sexp)
      (should (= (point) (1+ (sed-ts-mode-test--position "}"))))
      (backward-sexp)
      (should (= (point) (point-min)))))
  (dolist (syntax '(sed sed_ere))
    (sed-ts-mode-test--with-script syntax "s/abc/def/\na\\\ntext\nr file\n"
      (dolist (fragment '("abc" "def" "text" "file"))
        (goto-char (sed-ts-mode-test--position fragment))
        (let ((node (treesit-thing-at-point 'sexp 'nested)))
          (should (equal (treesit-node-type node) "editing_command")))))))

;;;; Imenu

(ert-deftest sed-ts-mode-indexes-gnu-labels-and-refreshes-after-edits ()
  (dolist (language '(gsed gsed_ere))
    (sed-ts-mode-test--with-script language ":長いlabel;T 長いlabel\n:\n:again\n"
      (let ((index (cdr (assoc "Label" (funcall imenu-create-index-function)))))
        (should (equal (mapcar #'car index) '("長いlabel" "again")))
        (should (equal (mapcar (lambda (entry) (marker-position (cdr entry))) index)
                       (list 1 (sed-ts-mode-test--position ":again")))))
      (goto-char (sed-ts-mode-test--position "again"))
      (delete-char 5)
      (insert "last")
      (should (equal (mapcar #'car (cdr (assoc "Label" (funcall imenu-create-index-function))))
                     '("長いlabel" "last"))))))

(ert-deftest sed-ts-mode-indexes-definitions ()
  (pcase-dolist (`(,syntax . ,language)
                 '((sed . sed) (sed_ere . sed_ere)))

    (sed-ts-mode-test--with-script
        syntax "b again\n:again\nt done\n:done\n:\n"
      (let ((index (funcall imenu-create-index-function)))
        (should (equal (mapcar #'car index) '("Label")))
        (setq index (cdr (assoc "Label" index)))
        (should (equal (mapcar #'car index) '("again" "done")))
        (should
         (equal
          (mapcar (lambda (item) (marker-position (cdr item))) index)
          (list (sed-ts-mode-test--position ":" ":again")
                (sed-ts-mode-test--position ":" ":done")))))
      (let* ((root (treesit-buffer-root-node language))
             (capture (car (treesit-query-capture
                            root '((label_function) @label))))
             (definition (cdr capture)))
        (should (equal (treesit-defun-name definition) "again"))
        (should-not (treesit-defun-name root))))))

(ert-deftest sed-ts-mode-keeps-duplicate-labels-in-source-order ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (sed-ts-mode-test--with-script syntax ":same\n:\n:same\nb same\n"
      (let ((entries (cdr (assoc "Label" (funcall imenu-create-index-function)))))
        (should (equal (mapcar #'car entries) '("same" "same")))
        (should (equal (mapcar (lambda (entry) (marker-position (cdr entry))) entries)
                       '(1 9)))))))

;;;; Indentation

(ert-deftest sed-ts-mode-preserves-gnu-operand-indentation ()
  (dolist (language '(gsed gsed_ere))
    (pcase-dolist (`(,source ,expected)
                   '(("{\ne echo one\\\n   two\nz\n}\n"
                      "{\n  e echo one\\\n   two\n  z\n}\n")
                     ("{\na inline\\\n  text\nQ 2\n}\n"
                      "{\n  a inline\\\n  text\n  Q 2\n}\n")))
      (should (equal (sed-ts-mode-test--indent language source) expected)))))

(ert-deftest sed-ts-mode-indents-after-return ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (pcase-dolist (`(,source ,line ,offset ,expected)
                   '(("{\n  p\n}\n" 0 2 "{\n  \n  p\n}\n")
                     ("{\n  p\n}\n" 1 2 "{\n  p\n  \n}\n")
                     ("{\n\n}\n" 0 2 "{\n  \n\n}\n")
                     ("1,2{\n    p\n}\n" 0 4 "1,2{\n    \n    p\n}\n")
                     ("{\n  /x/{\n    p\n  }\n}\n" 1 2 "{\n  /x/{\n    \n    p\n  }\n}\n")
                     ("{\n  /x/{\n    p\n  }\n}\n" 3 2 "{\n  /x/{\n    p\n  }\n  \n}\n")
                     ("{\n  {\n    {\n      p\n    }\n  }\n}\n" 4 2 "{\n  {\n    {\n      p\n    }\n    \n  }\n}\n")
                     ("{\n  {\n    p\n  } # done\n}\n" 3 2 "{\n  {\n    p\n  } # done\n  \n}\n")
                     ("{\n    {\n        p\n    }\n}\n" 3 4 "{\n    {\n        p\n    }\n    \n}\n")
                     ("p\nd\n" 0 2 "p\n\nd\n")
                     ("{\n  p\n}\n" 2 2 "{\n  p\n}\n\n")))
      (ert-info ((format "%s, return on line %s: %S" syntax line source))
        (sed-ts-mode-test--with-script syntax source
          (setq-local indent-tabs-mode nil)
          (setq-local sed-ts-mode-indent-offset offset)
          (electric-indent-local-mode 1)
          (goto-char (point-min))
          (forward-line line)
          (end-of-line)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))))))

(ert-deftest sed-ts-mode-indents-structures ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (pcase-dolist (`(,source ,offset ,expected)
                   '(("{\np\n}\n" 2 "{\n  p\n}\n")
                     ("{\np\n}\n" 4 "{\n    p\n}\n")
                     ("1,2{\np\n        }\n" 2 "1,2{\n  p\n}\n")
                     ("{\n/x/{\np\n}\n}\n" 2 "{\n  /x/{\n    p\n  }\n}\n")))
      (ert-info ((format "Offset %s: %S" offset source))
        (should (equal (sed-ts-mode-test--indent syntax source offset) expected))))))

(ert-deftest sed-ts-mode-preserves-text-and-replacement-indentation ()
  (dolist (syntax '(gsed gsed_ere sed sed_ere))
    (pcase-dolist (`(,source ,expected)
                   '(("{\na\\\n  text\\\n    continued\np\n}\n"
                      "{\n  a\\\n  text\\\n    continued\n  p\n}\n")
                     ("{\ns/a/first\\\n    second/\np\n}\n"
                      "{\n  s/a/first\\\n    second/\n  p\n}\n")))
      (should (equal (sed-ts-mode-test--indent syntax source) expected)))))

;;;; Updates

(ert-deftest sed-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (pcase-dolist (`(,language ,file ,properties ,prefix ,body ,fragment ,face)
                 '((gsed "test.sed" "posix = false\nere = false\n" "p\n" "# note\n" "note" font-lock-comment-face)
                   (gsed_ere "test.sed" "posix = false\nere = true\n" "p\n" "# note\n" "note" font-lock-comment-face)
                   (sed "test.sed" "posix = true\nere = false\n" "p\n" "# note\n" "note" font-lock-comment-face)
                   (sed_ere "test.sed" "posix = true\nere = true\n" "p\n" "# note\n" "note" font-lock-comment-face)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((default-directory
               (file-name-as-directory (make-temp-file "sed-ts-mode-" t))))
          (unwind-protect
              (progn
                (with-temp-file (expand-file-name ".editorconfig")
                  (insert "root = true\n[*]\n" properties))
                (with-temp-buffer
                  (setq buffer-file-name (expand-file-name file))
                  (insert prefix "\ufeff" body)
                  (let ((treesit-font-lock-level 4)) (sed-ts-mode))
                  (sed-ts-mode-test--buffer-state)
                  (when narrow
                    (narrow-to-region 1 (+ 2 (length prefix))))
                  (delete-region 1 (1+ (length prefix)))
                  (widen)
                  (should (eq (treesit-parser-language treesit-primary-parser) language))
                  (should (equal (treesit-parser-list) (list treesit-primary-parser)))
                  (let ((state (sed-ts-mode-test--buffer-state))
                        (tree (treesit-node-string
                               (treesit-parser-root-node treesit-primary-parser)))
                        (source (buffer-substring-no-properties (point-min) (point-max)))
                        (filename buffer-file-name))
                    (should (equal source (concat "\ufeff" body)))
                    (should-not (get-text-property 1 'face))
                    (goto-char (point-min))
                    (search-forward fragment)
                    (should (eq (get-text-property (1- (point)) 'face) face))
                    (with-temp-buffer
                      (setq buffer-file-name filename)
                      (insert source)
                      (let ((treesit-font-lock-level 4)) (sed-ts-mode))
                      (should (equal state (sed-ts-mode-test--buffer-state)))
                      (should (equal tree (treesit-node-string
                                           (treesit-parser-root-node treesit-primary-parser))))))))
            (delete-directory default-directory t)))))))

(ert-deftest sed-ts-mode-refreshes-gnu-classifications-after-edits ()
  (dolist (language '(gsed gsed_ere))
    (let ((treesit-font-lock-level 4))
      (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                     '(("s/a/\\U/\n" "\\U" "&" "&" font-lock-constant-face)
                       ("s/a/&/\n" "&" "\\U" "\\U" font-lock-escape-face)
                       ("1~2p\n" "2" "3" "3" font-lock-number-face)
                       ("s/a/x/\n" "a" "\\x5ba\\x5d" "\\x5b" font-lock-escape-face)
                       ("s/[[:unknown:]]/x/\n" "unknown" "alpha" "alpha" font-lock-constant-face)
                       ("e echo\\n\n" "e " "# " "echo" font-lock-comment-face)
                       ("# note\np\n" "# " "e " "note" font-lock-string-face)))
        (sed-ts-mode-test--with-script language source
          (sed-ts-mode-test--buffer-state)
          (goto-char (sed-ts-mode-test--position old))
          (delete-char (length old))
          (insert new)
          (font-lock-ensure)
          (should (eq (sed-ts-mode-test--face fragment) face))
          (sed-ts-mode-test--should-match-fresh-buffer 4))))))

(ert-deftest sed-ts-mode-updates-like-fresh-buffer ()
  (dolist (syntax '(sed sed_ere))
    (sed-ts-mode-test--with-language syntax
      (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                     '(("s/[[.a." "a." "a.]]/x/\n" "a." font-lock-constant-face)
                       ("1!" "!" "!p\n" "p" font-lock-keyword-face)
                       ("s/a/b" "b" "b/\n" "b" font-lock-string-face)))
        (ert-info ((format "%S: %S -> %S" source old new))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "script.sed"))
            (insert source)
            (let ((treesit-font-lock-level 4)) (sed-ts-mode))
            (sed-ts-mode-test--buffer-state)
            (goto-char (sed-ts-mode-test--position old))
            (delete-char (length old))
            (insert new)
            (font-lock-ensure)
            (should (eq (sed-ts-mode-test--face fragment) face))
            (sed-ts-mode-test--should-match-fresh-buffer 4)))))))

(ert-deftest sed-ts-mode-reclassifies-delimiters-after-edits ()
  (sed-ts-mode-test--with-script 'sed "{\np\n}\n"
    (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))
    (goto-char (point-min))
    (delete-char 1)
    (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 1))
    (goto-char (point-min))
    (insert "{")
    (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))))

(ert-deftest sed-ts-mode-recomputes-comment-syntax-after-edits ()
  (dolist (syntax '(sed sed_ere))

    (sed-ts-mode-test--with-script syntax "p;# note\n"
      (should (sed-ts-mode-test--comment-p "#" 1 "p;# note"))
      (let ((marker (sed-ts-mode-test--position "#" "p;# note")))
        (goto-char marker)
        (insert "s/")
        (should-not (sed-ts-mode-test--comment-p "#" 1 "p;s/# note"))
        (delete-region marker (+ marker 2))
        (should (sed-ts-mode-test--comment-p "#" 1 "p;# note"))))
    (sed-ts-mode-test--with-script syntax "a\\\n# text\np\n"
      (should-not (sed-ts-mode-test--comment-p "#" 1 "# text"))
      (goto-char 2)
      (delete-char 1)
      (should (sed-ts-mode-test--comment-p "#" 1 "# text"))
      (goto-char 2)
      (insert "\\")
      (should-not (sed-ts-mode-test--comment-p "#" 1 "# text")))))

(ert-deftest sed-ts-mode-preserves-syntax-when-narrowed ()
  (pcase-dolist (`(,before ,after ,comment-before ,comment-after)
                 '(("" "s/" t nil) ("s/" "" nil t)))
    (ert-info ((format "%S → %S" before after))
      (sed-ts-mode-test--with-script 'sed (concat before "# head\np\n# tail\n")
        (let ((body (sed-ts-mode-test--position "p" "p")))
          (narrow-to-region body (point-max))
          (syntax-propertize (point-max))
          (widen)
          (should (eq (= (syntax-class (syntax-after (+ 1 (length before)))) 11)
                      comment-before))
          (goto-char (point-min))
          (delete-char (length before))
          (insert after)
          (narrow-to-region (+ body (- (length after) (length before))) (point-max))
          (syntax-propertize (point-max))
          (widen)
          (let ((head (+ 1 (length after))))
            (should (eq (= (syntax-class (syntax-after head)) 11)
                        comment-after))
            (should (eq (nth 4 (syntax-ppss (1+ head))) comment-after))))))))

(ert-deftest sed-ts-mode-reclassifies-line-operands-after-issue-edits ()
  (dolist (syntax '(sed sed_ere))

    (let ((treesit-font-lock-level 4))
      (pcase-dolist (`(,prefix ,face)
                     '((":" font-lock-constant-face)
                       ("b " font-lock-constant-face)
                       ("t " font-lock-constant-face)
                       ("r " font-lock-string-face)
                       ("w " font-lock-string-face)
                       ("s/a/b/w " font-lock-string-face)))
        (sed-ts-mode-test--with-script syntax (concat prefix "ab\n# after\n")
          (font-lock-ensure)
          (dolist (invalid '(t nil t))
            (goto-char (+ (length prefix) 2))
            (if invalid (insert 0) (delete-char 1))
            (font-lock-flush)
            (font-lock-ensure)
            (dotimes (offset (if invalid 3 2))
              (should (eq (get-text-property (+ (length prefix) 1 offset) 'face)
                          (unless (and invalid (= offset 1)) face))))
            (should (eq (sed-ts-mode-test--face "after" nil "# after")
                        'font-lock-comment-face))
            (sed-ts-mode-test--should-match-fresh-buffer 4)
            (should (sed-ts-mode-test--comment-p "after" nil "# after"))))))))

(ert-deftest sed-ts-mode-reclassifies-character-class-names-after-repair ()
  (dolist (syntax '(sed sed_ere))

    (let ((treesit-font-lock-level 4))
      (pcase-dolist (`(,name ,faces)
                     '(("1" (nil))
                       ("a-b" (font-lock-constant-face nil font-lock-constant-face))
                       ("α" (nil))
                       ("\0001" (nil nil))
                       ("a\0001" (font-lock-constant-face nil nil))))
        (sed-ts-mode-test--with-script syntax (concat "s/[[:" name ":]]/x/\n")
          (font-lock-ensure)
          (let ((position 6))
            (dolist (face faces)
              (should (eq (get-text-property position 'face) face))
              (setq position (1+ position))))
          (delete-region 6 (+ 6 (length name)))
          (goto-char 6)
          (insert "alpha")
          (font-lock-flush)
          (font-lock-ensure)
          (dotimes (offset 5)
            (should (eq (get-text-property (+ 6 offset) 'face)
                        'font-lock-constant-face)))
          (should-not
           (treesit-query-capture
            (treesit-buffer-root-node syntax)
            '((syntax_issue) @issue))))))))

(ert-deftest sed-ts-mode-updates-bre-anchors-before-multibyte-delimiters ()
  (let ((treesit-font-lock-level 4))
    (pcase-dolist (`(,delimiter ,neighbor)
                   '(("é" "è") ("あ" "ぃ") ("😀" "😁")))
      (sed-ts-mode-test--with-script 'sed
          (concat "s" delimiter "$" neighbor delimiter "x" delimiter "\n")
        (pcase-dolist (`(,edit ,face)
                       '((nil font-lock-regexp-face)
                         (delete font-lock-operator-face)
                         (insert font-lock-regexp-face)))
          (goto-char 4)
          (pcase edit
            ('delete (delete-char 1))
            ('insert (insert neighbor)))
          (font-lock-flush)
          (font-lock-ensure)
          (should (eq (get-text-property 3 'face) face))
          (should (eq (get-text-property 2 'face) 'font-lock-delimiter-face))
          (let ((source (buffer-substring-no-properties (point-min) (point-max)))
                (faces (sed-ts-mode-test--face-map)))
            (should (equal faces
                           (sed-ts-mode-test--with-script 'sed source
                             (font-lock-ensure)
                             (sed-ts-mode-test--face-map))))))))))

(ert-deftest sed-ts-mode-reclassifies-regexp-escapes-after-edits ()
  (dolist (syntax '(sed sed_ere))

    (let ((treesit-font-lock-level 4))
      (sed-ts-mode-test--with-script syntax "/\\*/p\n"
        (pcase-dolist (`(,character ,face)
                       '((?* font-lock-escape-face) (?\0 nil)
                         (?* font-lock-escape-face)))
          (goto-char 3)
          (delete-char 1)
          (insert character)
          (font-lock-flush)
          (font-lock-ensure)
          (should (equal (sed-ts-mode-test--face-map)
                         (list 'font-lock-delimiter-face face face
                               'font-lock-delimiter-face
                               'font-lock-keyword-face nil))))))))

(ert-deftest sed-ts-mode-rebuilds-imenu-after-edits ()
  (sed-ts-mode-test--with-script 'sed ":first\n"
    (should (equal (mapcar #'car (cdr (assoc "Label"
                                             (funcall imenu-create-index-function))))
                   '("first")))
    (goto-char (point-max))
    (insert ":second\n")
    (should (equal (mapcar #'car (cdr (assoc "Label"
                                             (funcall imenu-create-index-function))))
                   '("first" "second")))))

(provide 'sed-ts-mode-test)

;;; sed-ts-mode-test.el ends here
