;;; sed-ts-mode-test.el --- Tests for sed-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'sed-ts-mode)

(dolist (language '(sed sed_ere))
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

(defun sed-ts-mode-test--language (syntax)
  (pcase syntax ('bre 'sed) ('ere 'sed_ere)))

(defmacro sed-ts-mode-test--with-regexp-syntax (syntax &rest body)
  (declare (indent 1) (debug (form body)))
  `(let ((default-directory
          (file-name-as-directory (make-temp-file "sed-ts-mode-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".editorconfig")
             (insert "root = true\n[*.sed]\nregex_dialect = "
                     (symbol-name ,syntax) "\n"))
           ,@body)
       (delete-directory default-directory t))))

(defmacro sed-ts-mode-test--with-script (syntax source &rest body)
  (declare (indent 2) (debug (form form body)))
  `(sed-ts-mode-test--with-regexp-syntax ,syntax
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
    (should (eq (treesit-parser-language treesit-primary-parser) 'sed))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "script"))
    (sed-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

(ert-deftest sed-ts-mode-selects-editorconfig-dialects-on-file-visits ()
  (dolist (enabled '(nil t))
    (pcase-dolist (`(,value ,language)
                   '(("bre" sed) ("ere" sed_ere) (nil sed) ("unset" sed)))

      (sed-ts-mode-test--with-regexp-syntax 'ere
                                            (with-temp-file (expand-file-name ".editorconfig")
                                              (insert "root = true\n[*.sed]\n")
                                              (when value (insert "regex_dialect = " value "\n")))
                                            (with-temp-file (expand-file-name "script.sed") (insert "/a+/p\n"))
                                            (let ((previous editorconfig-mode))
                                              (unwind-protect
                                                  (progn
                                                    (editorconfig-mode (if enabled 1 -1))
                                                    (with-current-buffer (find-file-noselect
                                                                          (expand-file-name "script.sed"))
                                                      (unwind-protect
                                                          (progn
                                                            (should (eq major-mode 'sed-ts-mode))
                                                            (should (eq (treesit-parser-language
                                                                         treesit-primary-parser) language))
                                                            (should (eq editorconfig-mode enabled)))
                                                        (kill-buffer))))
                                                (editorconfig-mode (if previous 1 -1))))))))

(ert-deftest sed-ts-mode-defaults-to-bre-without-a-file-name ()
  (sed-ts-mode-test--with-regexp-syntax 'ere
                                        (with-temp-buffer
                                          (insert "/a+/p\n")
                                          (sed-ts-mode)
                                          (should-not buffer-file-name)
                                          (should (eq (treesit-parser-language treesit-primary-parser) 'sed)))))

(ert-deftest sed-ts-mode-rejects-invalid-editorconfig-dialects ()
  (dolist (value '(pcre ERE invalid))
    (sed-ts-mode-test--with-regexp-syntax value
                                          (with-temp-buffer
                                            (setq buffer-file-name (expand-file-name "script.sed"))
                                            (let ((error-data (should-error (sed-ts-mode) :type 'user-error)))
                                              (should (equal (error-message-string error-data)
                                                             (format "Invalid EditorConfig regex_dialect: %S"
                                                                     (symbol-name value)))))
                                            (should-not (treesit-parser-list nil nil t))))))

(ert-deftest sed-ts-mode-reloads-editorconfig-only-on-mode-restart ()
  (let ((treesit-font-lock-level 4))
    (sed-ts-mode-test--with-script 'bre "/a+/p\n"
                                   (font-lock-ensure)
                                   (should (eq (sed-ts-mode-test--face "+" nil "/a+/p")
                                               'font-lock-regexp-face))
                                   (pcase-dolist (`(,value ,language ,face)
                                                  '(("ere" sed_ere font-lock-operator-face)
                                                    ("bre" sed font-lock-regexp-face)))
                                     (let ((parser treesit-primary-parser))
                                       (with-temp-file (expand-file-name ".editorconfig")
                                         (insert "root = true\n[*.sed]\nregex_dialect = " value "\n"))
                                       (font-lock-flush)
                                       (font-lock-ensure)
                                       (should (eq treesit-primary-parser parser))
                                       (sed-ts-mode)
                                       (font-lock-ensure)
                                       (should (eq (treesit-parser-language treesit-primary-parser)
                                                   language))
                                       (should (equal (treesit-parser-list) (list treesit-primary-parser)))
                                       (should (eq (sed-ts-mode-test--face "+" nil "/a+/p") face))
                                       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                                      "/a+/p\n")))))))

;;;; Mode Selection

(ert-deftest sed-ts-mode-selects-files ()
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'sed-ts-mode)
        (push (car entry) patterns)))
    (should (equal patterns '("\\.sed\\'"))))
  (sed-ts-mode-test--with-regexp-syntax 'bre
                                        (with-temp-buffer
                                          (setq buffer-file-name (expand-file-name "example.sed"))
                                          (set-auto-mode)
                                          (should (eq major-mode 'sed-ts-mode)))))

(ert-deftest sed-ts-mode-selects-interpreters ()
  (should (equal (alist-get "sed" interpreter-mode-alist nil nil #'equal)
                 'sed-ts-mode))
  (sed-ts-mode-test--with-regexp-syntax 'bre
                                        (dolist (shebang '("#!/usr/bin/sed -f\n"
                                                           "#!/usr/bin/env sed -f\n"
                                                           "#!/usr/bin/env -S sed -f\n"))
                                          (with-temp-buffer
                                            (setq buffer-file-name "/tmp/example")
                                            (insert shebang "p\n")
                                            (set-auto-mode)
                                            (should (eq major-mode 'sed-ts-mode))))))

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

(ert-deftest sed-ts-mode-classifies-delimiters ()
  (dolist (case
           '((bre "s/\\(a[bc]\\)\\{2\\}/x/"
                  ("\\(" "\\)" "[" "]" "\\{" "\\}"))
             (ere "s/(a[bc]){2}/x/"
                  ("(" ")" "[" "]" "{" "}"))))
    (pcase-let ((`(,syntax ,regexp ,delimiters) case))

      (sed-ts-mode-test--with-script syntax
                                     (concat "{\n" regexp "\n}\n")
                                     (dolist (character '(?\( ?\) ?\[ ?\] ?{ ?}))
                                       (should (eq (char-syntax character) ?.)))
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
  (dolist (syntax '(bre ere))

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
                                   (dolist (case '(("#n directive" "#" 1)
                                                   ("#" "#" 1)
                                                   ("# plain" "#" 1)
                                                   ("p;# trailing" "#" 1)
                                                   ("# after-quote" "#" 1)))
                                     (should (sed-ts-mode-test--comment-p (nth 1 case) (nth 2 case) (car case))))
                                   (dolist (case '(("s/#/x/" "#" 1)
                                                   ("s/x/#/" "#" 1)
                                                   ("y/#/x/" "#" 1)
                                                   ("# text" "#" 1)
                                                   ("r #file" "#" 1)))
                                     (should-not (sed-ts-mode-test--comment-p (nth 1 case) (nth 2 case) (car case)))))
    (dolist (source '("# note" "#"))
      (sed-ts-mode-test--with-script syntax source
                                     (syntax-propertize (point-max))
                                     (should (nth 4 (syntax-ppss (point-max))))))))

;;;; Font Lock

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
    (sed-ts-mode-test--with-script 'bre
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
    (sed-ts-mode-test--with-script 'bre
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
  (dolist (entry '((bre . sed) (ere . sed_ere)))

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
    (sed-ts-mode-test--with-script 'bre
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
                                      (,extensions "\\|" font-lock-operator-face)
                                      (,extensions "\\+" font-lock-operator-face)
                                      (,extensions "\\?" font-lock-operator-face))))))

(ert-deftest sed-ts-mode-fontifies-ere-components ()
  (let* ((treesit-font-lock-level 4)
         (expression "/^a\\.(a|[^[:alpha:]x-z]{2,3})+?.*$/p")
         (optional "/a?b/p"))
    (sed-ts-mode-test--with-script 'ere
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

(ert-deftest sed-ts-mode-fontifies-only-written-command-separators ()
  (dolist (syntax '(bre ere))

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
    (sed-ts-mode-test--with-script 'bre
                                   (concat control "\n" replacement "\n" translation "\n")
                                   (font-lock-ensure)
                                   (sed-ts-mode-test--should-have-faces
                                    `((,control "\\n" font-lock-escape-face)
                                      (,control "\\&" font-lock-escape-face)
                                      (,replacement "\\/" font-lock-escape-face)
                                      (,translation "\\/" font-lock-escape-face))))))

(ert-deftest sed-ts-mode-captures-only-leaves ()
  (dolist (syntax '(bre ere))

    (sed-ts-mode-test--with-regexp-syntax syntax
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

;;;; Imenu

(ert-deftest sed-ts-mode-indexes-definitions ()
  (dolist (entry '((bre . sed) (ere . sed_ere)))

    (sed-ts-mode-test--with-script
     (car entry) "b again\n:again\nt done\n:done\n:\n"
     (let ((index (funcall imenu-create-index-function)))
       (should (equal (mapcar #'car index) '("Label")))
       (setq index (cdr (assoc "Label" index)))
       (should (equal (mapcar #'car index) '("again" "done")))
       (should
        (equal
         (mapcar (lambda (item) (marker-position (cdr item))) index)
         (list (sed-ts-mode-test--position ":" ":again")
               (sed-ts-mode-test--position ":" ":done")))))
     (let* ((root (treesit-buffer-root-node (cdr entry)))
            (capture (car (treesit-query-capture
                           root '((label_function) @label))))
            (definition (cdr capture)))
       (should (equal (treesit-defun-name definition) "again"))
       (should-not (treesit-defun-name root))))))

;;;; Indentation

(ert-deftest sed-ts-mode-indents-structures ()
  (dolist (syntax '(bre ere))
    (pcase-dolist (`(,source ,offset ,expected)
                   '(("{\np\n}\n" 2 "{\n  p\n}\n")
                     ("{\np\n}\n" 4 "{\n    p\n}\n")
                     ("1,2{\np\n        }\n" 2 "1,2{\n  p\n}\n")
                     ("{\n/x/{\np\n}\n}\n" 2 "{\n  /x/{\n    p\n  }\n}\n")))
      (ert-info ((format "Offset %s: %S" offset source))
        (should (equal (sed-ts-mode-test--indent syntax source offset) expected))))))

;;;; Updates

(ert-deftest sed-ts-mode-updates-like-fresh-buffer ()
  (dolist (syntax '(bre ere))
    (sed-ts-mode-test--with-regexp-syntax syntax
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
  (sed-ts-mode-test--with-script 'bre "{\np\n}\n"
                                 (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))
                                 (goto-char (point-min))
                                 (delete-char 1)
                                 (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 1))
                                 (goto-char (point-min))
                                 (insert "{")
                                 (should (= (sed-ts-mode-test--syntax-class "}" nil "}") 5))))

(ert-deftest sed-ts-mode-recomputes-comment-syntax-after-edits ()
  (dolist (syntax '(bre ere))

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
      (sed-ts-mode-test--with-script 'bre (concat before "# head\np\n# tail\n")
                                     (let ((body (sed-ts-mode-test--position "p" "p")))
                                       (narrow-to-region body (point-max))
                                       (syntax-propertize (point-max))
                                       (widen)
                                       (should (eq (not (null (get-text-property (+ 1 (length before))
                                                                                 'syntax-table)))
                                                   comment-before))
                                       (goto-char (point-min))
                                       (delete-char (length before))
                                       (insert after)
                                       (narrow-to-region (+ body (- (length after) (length before))) (point-max))
                                       (syntax-propertize (point-max))
                                       (widen)
                                       (let ((head (+ 1 (length after))))
                                         (should (eq (not (null (get-text-property head 'syntax-table)))
                                                     comment-after))
                                         (should (eq (nth 4 (syntax-ppss (1+ head))) comment-after))))))))

(ert-deftest sed-ts-mode-reclassifies-line-operands-after-issue-edits ()
  (dolist (syntax '(bre ere))

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
  (dolist (syntax '(bre ere))

    (let ((treesit-font-lock-level 4))
      (pcase-dolist (`(,name ,faces)
                     '(("1" (nil))
                       ("a-b" (nil nil nil))
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
                                         (treesit-buffer-root-node (sed-ts-mode-test--language syntax))
                                         '((syntax_issue) @issue))))))))

(ert-deftest sed-ts-mode-updates-bre-anchors-before-multibyte-delimiters ()
  (let ((treesit-font-lock-level 4))
    (pcase-dolist (`(,delimiter ,neighbor)
                   '(("é" "è") ("あ" "ぃ") ("😀" "😁")))
      (sed-ts-mode-test--with-script 'bre
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
                                                        (sed-ts-mode-test--with-script 'bre source
                                                                                       (font-lock-ensure)
                                                                                       (sed-ts-mode-test--face-map))))))))))

(ert-deftest sed-ts-mode-reclassifies-regexp-escapes-after-edits ()
  (dolist (syntax '(bre ere))

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
  (sed-ts-mode-test--with-script 'bre ":first\n"
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
