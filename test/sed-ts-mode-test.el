;;; sed-ts-mode-test.el --- Tests for sed-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sed-ts-mode)

(defun sed-ts-mode-test--require-grammar (language)
  (unless (treesit-ready-p language t)
    (ert-skip (format "The %s grammar is unavailable" language))))

(defun sed-ts-mode-test--grammar-source (language configured)
  (let ((treesit-language-source-alist configured)
        (ensure (symbol-function 'treesit-ensure-installed))
        source)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (installed-language)
                  (setq source
                        (assq installed-language
                              treesit-language-source-alist))
                  t))
          (should (sed-ts-mode--ensure-grammar language))
          source)
      (fset 'treesit-ensure-installed ensure))))

(defmacro sed-ts-mode-test--with-regexp-syntax (syntax &rest body)
  (declare (indent 1) (debug (form body)))
  `(let ((default (default-value 'sed-ts-mode-regexp-syntax)))
     (unwind-protect
         (progn
           (set-default 'sed-ts-mode-regexp-syntax ,syntax)
           ,@body)
       (set-default 'sed-ts-mode-regexp-syntax default))))

(defmacro sed-ts-mode-test--with-script (syntax source &rest body)
  (declare (indent 2) (debug (form form body)))
  `(sed-ts-mode-test--with-regexp-syntax ,syntax
     (with-temp-buffer
       (insert ,source)
       (sed-ts-mode)
       ,@body)))

(defun sed-ts-mode-test--position-in-line (line fragment)
  (let (line-start)
    (save-excursion
      (goto-char (point-min))
      (while (and (not line-start) (not (eobp)))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (if (equal line (buffer-substring-no-properties start end))
              (setq line-start start)
            (forward-line 1)))))
    (unless line-start
      (ert-fail (format "Test line not found: %s" line)))
    (let ((offset (string-search fragment line)))
      (unless offset
        (ert-fail (format "Fragment %s not found in test line: %s"
                          fragment line)))
      (+ line-start offset))))

(defun sed-ts-mode-test--face-in-line (line fragment &optional offset)
  (get-text-property
   (+ (sed-ts-mode-test--position-in-line line fragment)
      (or offset 0))
   'face))

(defun sed-ts-mode-test--comment-in-line-p (line fragment &optional offset)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss
          (+ (sed-ts-mode-test--position-in-line line fragment)
             (or offset 0)))))

(defun sed-ts-mode-test--syntax-class-in-line
    (line fragment &optional offset)
  (syntax-propertize (point-max))
  (syntax-class
   (syntax-after
    (+ (sed-ts-mode-test--position-in-line line fragment)
       (or offset 0)))))

(defun sed-ts-mode-test--should-fontify (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (should (equal (list line fragment
                         (sed-ts-mode-test--face-in-line line fragment))
                   (list line fragment face)))))

(defun sed-ts-mode-test--face-map ()
  (let ((position (point-min)) faces)
    (while (< position (point-max))
      (push (get-text-property position 'face) faces)
      (setq position (1+ position)))
    (nreverse faces)))

(defun sed-ts-mode-test--block-delimiter-faces ()
  (let (faces)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[{}]" nil t)
        (push (get-text-property (match-beginning 0) 'face) faces)))
    (nreverse faces)))

(defun sed-ts-mode-test--should-fontify-block-delimiters
    (syntax source faces)
  (let ((treesit-font-lock-level 4))
    (sed-ts-mode-test--with-script syntax source
      (font-lock-ensure)
      (should
       (equal (sed-ts-mode-test--block-delimiter-faces) faces)))))

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

(ert-deftest sed-ts-mode-starts-with-a-bre-parser ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "/a\\{2\\}/p\n"
    (should (eq major-mode 'sed-ts-mode))
    (let ((root (treesit-buffer-root-node 'sed)))
      (should (equal (treesit-node-type root) "script"))
      (should (treesit-query-capture root '((basic_reg_exp) @expression))))))

(ert-deftest sed-ts-mode-provides-default-grammar-sources ()
  (should
   (equal
    (sed-ts-mode-test--grammar-source 'sed nil)
    '(sed "https://github.com/konomanoasa/tree-sitter-sed"
          :revision "v0.7.0")))
  (should
   (equal
    (sed-ts-mode-test--grammar-source 'sed_ere nil)
    '(sed_ere "https://github.com/konomanoasa/tree-sitter-sed"
              :revision "v0.7.0"
              :source-dir "sed_ere/src"))))

(ert-deftest sed-ts-mode-preserves-a-user-grammar-source ()
  (let ((custom '(sed . ("custom-source"))))
    (should (equal (sed-ts-mode-test--grammar-source
                    'sed (list custom))
                   custom))))

(ert-deftest sed-ts-mode-uses-an-ere-parser-when-configured ()
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (sed-ts-mode-test--with-script 'ere "/a+/p\n"
    (should (eq major-mode 'sed-ts-mode))
    (let ((root (treesit-buffer-root-node 'sed_ere)))
      (should (equal (treesit-node-type root) "script"))
      (should (treesit-query-capture root '((extended_reg_exp) @expression))))))

(ert-deftest sed-ts-mode-applies-a-file-local-regexp-syntax ()
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (let ((file (make-temp-file
               "sed-ts-mode" nil ".sed"
               "#n -*- sed-ts-mode-regexp-syntax: ere -*-\n/a+/p\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (unwind-protect
              (progn
                (should (eq major-mode 'sed-ts-mode))
                (should (eq sed-ts-mode-regexp-syntax 'ere))
                (should (eq (treesit-parser-language
                             treesit-primary-parser)
                            'sed_ere))
                (should (equal (treesit-parser-list)
                               (list treesit-primary-parser)))
                (should (treesit-query-capture
                         (treesit-buffer-root-node 'sed_ere)
                         '((one_or_more_operator) @operator))))
            (kill-buffer)))
      (delete-file file))))

(ert-deftest sed-ts-mode-switches-the-current-buffer-between-bre-and-ere ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (let ((treesit-font-lock-level 4))
    (sed-ts-mode-test--with-script 'bre "/a+/p\n"
      (goto-char 3)
      (set-buffer-modified-p nil)
      (font-lock-ensure)
      (let ((default (default-value 'sed-ts-mode-regexp-syntax))
            (position (point))
            (source (buffer-string)))
        (should (local-variable-p 'sed-ts-mode-regexp-syntax))
        (should (eq (sed-ts-mode-test--face-in-line "/a+/p" "+")
                    'font-lock-regexp-face))
        (sed-ts-toggle-regexp)
        (font-lock-ensure)
        (should (eq sed-ts-mode-regexp-syntax 'ere))
        (should (eq (treesit-parser-language treesit-primary-parser)
                    'sed_ere))
        (should (equal (treesit-parser-list)
                       (list treesit-primary-parser)))
        (should (treesit-query-capture
                 (treesit-buffer-root-node 'sed_ere)
                 '((one_or_more_operator) @operator)))
        (should (eq (sed-ts-mode-test--face-in-line "/a+/p" "+")
                    'font-lock-operator-face))
        (should (= (point) position))
        (should (equal (buffer-string) source))
        (should-not (buffer-modified-p))
        (should (eq (default-value 'sed-ts-mode-regexp-syntax) default))
        (let ((parser treesit-primary-parser))
          (sed-ts-mode--set-regexp-syntax 'ere)
          (should (eq treesit-primary-parser parser)))
        (sed-ts-toggle-regexp)
        (font-lock-ensure)
        (should (eq sed-ts-mode-regexp-syntax 'bre))
        (should (eq (treesit-parser-language treesit-primary-parser) 'sed))
        (should (equal (treesit-parser-list)
                       (list treesit-primary-parser)))
        (should (treesit-query-capture
                 (treesit-buffer-root-node 'sed)
                 '((basic_reg_exp) @expression)))
        (should (eq (sed-ts-mode-test--face-in-line "/a+/p" "+")
                    'font-lock-regexp-face))))))

(ert-deftest sed-ts-mode-keeps-open-buffers-on-their-selected-syntax ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "/a/p\n"
    (setopt sed-ts-mode-regexp-syntax 'ere)
    (should (eq sed-ts-mode-regexp-syntax 'bre))
    (should (eq (default-value 'sed-ts-mode-regexp-syntax) 'ere))
    (should (eq (treesit-parser-language treesit-primary-parser) 'sed))))

(ert-deftest sed-ts-mode-keeps-the-current-parser-when-switching-fails ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "/a/p\n"
    (let ((ensure (symbol-function 'treesit-ensure-installed))
          (parser treesit-primary-parser))
      (unwind-protect
          (progn
            (fset 'treesit-ensure-installed (lambda (_) nil))
            (should-error (sed-ts-toggle-regexp)
                          :type 'user-error)
            (should (eq sed-ts-mode-regexp-syntax 'bre))
            (should (eq treesit-primary-parser parser))
            (should (equal (treesit-parser-list) (list parser))))
        (fset 'treesit-ensure-installed ensure)))))

(ert-deftest sed-ts-mode-rejects-regexp-switches-outside-the-mode ()
  (with-temp-buffer
    (should-error (sed-ts-toggle-regexp)
                  :type 'user-error)))

(ert-deftest sed-ts-mode-rejects-an-unsupported-regexp-syntax ()
  (sed-ts-mode-test--with-regexp-syntax 'pcre
    (with-temp-buffer
      (let ((error-data (should-error (sed-ts-mode) :type 'user-error)))
        (should (string-match-p
                 "pcre" (error-message-string error-data)))))))

(ert-deftest sed-ts-mode-uses-only-block-delimiters-as-parens ()
  (dolist (case
           '((bre "s/\\(a[bc]\\)\\{2\\}/x/"
                  ("\\(" "\\)" "[" "]" "\\{" "\\}"))
             (ere "s/(a[bc]){2}/x/"
                  ("(" ")" "[" "]" "{" "}"))))
    (pcase-let ((`(,syntax ,regexp ,delimiters) case))
      (sed-ts-mode-test--require-grammar
       (sed-ts-mode--language syntax))
      (sed-ts-mode-test--with-script syntax
          (concat "{\n" regexp "\n}\n")
        (dolist (character '(?\( ?\) ?\[ ?\] ?{ ?}))
          (should (eq (char-syntax character) ?.)))
        (should (= (sed-ts-mode-test--syntax-class-in-line "{" "{") 4))
        (should (= (sed-ts-mode-test--syntax-class-in-line "}" "}") 5))
        (should (= (scan-sexps
                    (sed-ts-mode-test--position-in-line "{" "{") 1)
                   (1+ (sed-ts-mode-test--position-in-line "}" "}"))))
        (dolist (delimiter delimiters)
          (should
           (= (sed-ts-mode-test--syntax-class-in-line regexp delimiter)
              1)))))))

(ert-deftest sed-ts-mode-reclassifies-block-delimiters-after-edits ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "{\np\n}\n"
    (should (= (sed-ts-mode-test--syntax-class-in-line "}" "}") 5))
    (goto-char (point-min))
    (delete-char 1)
    (should (= (sed-ts-mode-test--syntax-class-in-line "}" "}") 1))
    (goto-char (point-min))
    (insert "{")
    (should (= (sed-ts-mode-test--syntax-class-in-line "}" "}") 5))))

(ert-deftest sed-ts-mode-configures-posix-sed-line-comments ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre ""
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (equal comment-start-skip "#[[:blank:]]*"))
    (should comment-use-syntax)
    (should (eq (char-syntax ?#) ?.))
    (should (eq (char-syntax ?\n) ?>))))

(defun sed-ts-mode-test--check-contextual-comments (syntax)
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
      (should (apply #'sed-ts-mode-test--comment-in-line-p case)))
    (dolist (case '(("s/#/x/" "#" 1)
                    ("s/x/#/" "#" 1)
                    ("y/#/x/" "#" 1)
                    ("# text" "#" 1)
                    ("r #file" "#" 1)))
      (should-not (apply #'sed-ts-mode-test--comment-in-line-p case))))
  (dolist (source '("# note" "#"))
    (sed-ts-mode-test--with-script syntax source
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (point-max)))))))

(ert-deftest sed-ts-mode-recognizes-only-cst-comments-with-bre ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--check-contextual-comments 'bre))

(ert-deftest sed-ts-mode-recognizes-only-cst-comments-with-ere ()
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (sed-ts-mode-test--check-contextual-comments 'ere))

(defun sed-ts-mode-test--check-comment-reclassification (syntax)
  (sed-ts-mode-test--with-script syntax "p;# note\n"
    (should (sed-ts-mode-test--comment-in-line-p
             "p;# note" "#" 1))
    (let ((marker (sed-ts-mode-test--position-in-line "p;# note" "#")))
      (goto-char marker)
      (insert "s/")
      (should-not (sed-ts-mode-test--comment-in-line-p
                   "p;s/# note" "#" 1))
      (delete-region marker (+ marker 2))
      (should (sed-ts-mode-test--comment-in-line-p
               "p;# note" "#" 1))))
  (sed-ts-mode-test--with-script syntax "a\\\n# text\np\n"
    (should-not (sed-ts-mode-test--comment-in-line-p
                 "# text" "#" 1))
    (goto-char 2)
    (delete-char 1)
    (should (sed-ts-mode-test--comment-in-line-p
             "# text" "#" 1))
    (goto-char 2)
    (insert "\\")
    (should-not (sed-ts-mode-test--comment-in-line-p
                 "# text" "#" 1))))

(ert-deftest sed-ts-mode-recomputes-bre-comment-syntax-after-edits ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--check-comment-reclassification 'bre))

(ert-deftest sed-ts-mode-recomputes-ere-comment-syntax-after-edits ()
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (sed-ts-mode-test--check-comment-reclassification 'ere))

(ert-deftest sed-ts-mode-propertizes-comments-before-narrowed-regions ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "# head\np\n# tail\n"
    (let ((head (sed-ts-mode-test--position-in-line "# head" "#"))
          (body (sed-ts-mode-test--position-in-line "p" "p"))
          (tail (sed-ts-mode-test--position-in-line "# tail" "#")))
      (narrow-to-region body (point-max))
      (should (nth 4 (syntax-ppss (1+ tail))))
      (widen)
      (should (nth 4 (syntax-ppss (1+ head)))))))

(ert-deftest sed-ts-mode-removes-stale-comments-before-narrowed-regions ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "# head\np\n# tail\n"
    (let ((body (sed-ts-mode-test--position-in-line "p" "p")))
      (narrow-to-region body (point-max))
      (syntax-propertize (point-max))
      (widen)
      (should (get-text-property
               (sed-ts-mode-test--position-in-line "# head" "#")
               'syntax-table))
      (goto-char (point-min))
      (insert "s/")
      (narrow-to-region (+ body 2) (point-max))
      (syntax-propertize (point-max))
      (widen)
      (let ((head (sed-ts-mode-test--position-in-line "s/# head" "#")))
        (should-not (get-text-property head 'syntax-table))
        (should-not (nth 4 (syntax-ppss (1+ head))))))))

(ert-deftest sed-ts-mode-restores-comments-revealed-before-narrowed-regions ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre "s/# head\np\n# tail\n"
    (let ((body (sed-ts-mode-test--position-in-line "p" "p")))
      (narrow-to-region body (point-max))
      (syntax-propertize (point-max))
      (widen)
      (should-not (get-text-property
                   (sed-ts-mode-test--position-in-line "s/# head" "#")
                   'syntax-table))
      (goto-char (point-min))
      (delete-char 2)
      (narrow-to-region (- body 2) (point-max))
      (syntax-propertize (point-max))
      (widen)
      (let ((head (sed-ts-mode-test--position-in-line "# head" "#")))
        (should (get-text-property head 'syntax-table))
        (should (nth 4 (syntax-ppss (1+ head))))))))

(ert-deftest sed-ts-mode-uses-standard-tree-sitter-imenu ()
  (dolist (entry '((bre . sed) (ere . sed_ere)))
    (sed-ts-mode-test--require-grammar (cdr entry))
    (sed-ts-mode-test--with-script (car entry) ""
      (should (equal treesit-simple-imenu-settings
                     sed-ts-mode-imenu-settings))
      (should (eq treesit-defun-name-function
                  #'sed-ts-mode--defun-name))
      (should (eq imenu-create-index-function #'treesit-simple-imenu)))))

(ert-deftest sed-ts-mode-indexes-only-label-definitions ()
  (dolist (entry '((bre . sed) (ere . sed_ere)))
    (sed-ts-mode-test--require-grammar (cdr entry))
    (sed-ts-mode-test--with-script
        (car entry) "b again\n:again\nt done\n:done\n:\n"
      (let ((index (funcall imenu-create-index-function)))
        (should (equal (mapcar #'car index) '("again" "done")))
        (should
         (equal
          (mapcar (lambda (item) (marker-position (cdr item))) index)
          (list (sed-ts-mode-test--position-in-line ":again" ":")
                (sed-ts-mode-test--position-in-line ":done" ":")))))
      (let* ((root (treesit-buffer-root-node (cdr entry)))
             (capture (car (treesit-query-capture
                            root '((label_function) @label))))
             (definition (cdr capture)))
        (should (equal (treesit-defun-name definition) "again"))
        (should-not (treesit-defun-name root))))))

(ert-deftest sed-ts-mode-rebuilds-imenu-after-edits ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre ":first\n"
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first")))
    (goto-char (point-max))
    (insert ":second\n")
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first" "second")))))

(ert-deftest sed-ts-mode-uses-standard-tree-sitter-indentation ()
  (sed-ts-mode-test--require-grammar 'sed)
  (sed-ts-mode-test--with-script 'bre ""
    (should (eq indent-line-function #'treesit-indent))
    (should (eq indent-region-function #'treesit-indent-region))
    (should (equal (alist-get 'sed treesit-simple-indent-rules)
                   (alist-get 'sed sed-ts-mode-indent-rules)))))

(ert-deftest sed-ts-mode-indents-block-rules ()
  (dolist (entry '((bre . sed) (ere . sed_ere)))
    (sed-ts-mode-test--require-grammar (cdr entry))
    (should (equal (sed-ts-mode-test--indent (car entry) "{\np\n}\n")
                   "{\n  p\n}\n"))))

(ert-deftest sed-ts-mode-indent-offset-controls-rules ()
  (sed-ts-mode-test--require-grammar 'sed)
  (should (equal (sed-ts-mode-test--indent 'bre "{\np\n}\n" 4)
                 "{\n    p\n}\n")))

(ert-deftest sed-ts-mode-indents-addressed-and-nested-blocks ()
  (sed-ts-mode-test--require-grammar 'sed)
  (should (equal (sed-ts-mode-test--indent 'bre "1,2{\np\n        }\n")
                 "1,2{\n  p\n}\n"))
  (should (equal (sed-ts-mode-test--indent 'bre "{\n/x/{\np\n}\n}\n")
                 "{\n  /x/{\n    p\n  }\n}\n")))

(ert-deftest sed-ts-mode-selects-sed-files ()
  (sed-ts-mode-test--require-grammar 'sed)
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'sed-ts-mode)
        (push (car entry) patterns)))
    (should (equal patterns '("\\.sed\\'"))))
  (sed-ts-mode-test--with-regexp-syntax 'bre
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example.sed")
      (set-auto-mode)
      (should (eq major-mode 'sed-ts-mode)))))

(ert-deftest sed-ts-mode-generates-mode-and-file-association-autoloads ()
  (require 'loaddefs-gen)
  (let ((output (make-temp-file "sed-ts-mode-loaddefs-"))
        (directory
         (file-name-directory (locate-library "sed-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'sed-ts-mode"
                            "(add-to-list 'auto-mode-alist"
                            "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

(ert-deftest sed-ts-mode-selects-sed-interpreters ()
  (sed-ts-mode-test--require-grammar 'sed)
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

(ert-deftest sed-ts-mode-fontifies-each-command-verb ()
  (sed-ts-mode-test--require-grammar 'sed)
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
      (sed-ts-mode-test--should-fontify
       (append
        (mapcar (lambda (command)
                  (list command (substring command 0 1)
                        'font-lock-keyword-face))
                commands)
        '(("{" "{" font-lock-bracket-face)
          ("}" "}" font-lock-bracket-face)))))))

(ert-deftest sed-ts-mode-fontifies-block-delimiters ()
  (dolist (syntax '(bre ere))
    (sed-ts-mode-test--require-grammar (sed-ts-mode--language syntax))
    (sed-ts-mode-test--should-fontify-block-delimiters
     syntax "{\n{\np\n}\n}\n"
     '(font-lock-bracket-face font-lock-bracket-face
       font-lock-bracket-face font-lock-bracket-face))))

(ert-deftest sed-ts-mode-keeps-regexp-delimiter-faces ()
  (dolist (case '((bre "s/\\([a]\\)\\{2\\}/x/"
                       "\\(" "[" "\\{")
                  (ere "s/([a]){2}/x/" "(" "[" "{")))
    (pcase-let ((`(,syntax ,regexp ,group ,bracket ,interval) case))
      (sed-ts-mode-test--require-grammar (sed-ts-mode--language syntax))
      (let ((treesit-font-lock-level 4))
        (sed-ts-mode-test--with-script syntax
            (concat "{\n" regexp "\n}\n")
          (font-lock-ensure)
          (sed-ts-mode-test--should-fontify
           `((,regexp ,group font-lock-bracket-face)
             (,regexp ,bracket font-lock-bracket-face)
             (,regexp ,interval font-lock-bracket-face))))))))

(ert-deftest sed-ts-mode-fontifies-compound-bracket-delimiters ()
  (dolist (entry '((bre . sed) (ere . sed_ere)))
    (sed-ts-mode-test--require-grammar (cdr entry))
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
           (eq (sed-ts-mode-test--face-in-line
                line (car expectation) (nth 1 expectation))
               (nth 2 expectation))))
        (dolist (payload '("x" "y" "alpha"))
          (should (eq (sed-ts-mode-test--face-in-line line payload)
                      'font-lock-constant-face)))))))

(ert-deftest sed-ts-mode-fontifies-addresses-arguments-and-data ()
  (sed-ts-mode-test--require-grammar 'sed)
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
      (sed-ts-mode-test--should-fontify
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
        (let ((backslash (sed-ts-mode-test--position-in-line line "\\")))
          (should (eq (get-text-property (1+ backslash) 'face)
                      'font-lock-punctuation-face)))))))

(ert-deftest sed-ts-mode-fontifies-only-written-command-separators ()
  (sed-ts-mode-test--require-grammar 'sed)
  (let ((treesit-font-lock-level 4))
    (sed-ts-mode-test--with-script 'bre "2p;3d\n"
      (font-lock-ensure)
      (sed-ts-mode-test--should-fontify
       '(("2p;3d" ";" font-lock-punctuation-face)))
      (goto-char (point-min))
      (should-not (get-text-property (line-end-position) 'face)))))

(ert-deftest sed-ts-mode-fontifies-bre-components ()
  (sed-ts-mode-test--require-grammar 'sed)
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
      (sed-ts-mode-test--should-fontify
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
  (sed-ts-mode-test--require-grammar 'sed_ere)
  (let* ((treesit-font-lock-level 4)
         (expression "/^a\\.(a|[^[:alpha:]x-z]{2,3})+?.*$/p")
         (optional "/a?b/p"))
    (sed-ts-mode-test--with-script 'ere
        (concat expression "\n" optional "\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-fontify
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
      (should (eq (sed-ts-mode-test--face-in-line expression "[^" 1)
                  'font-lock-negation-char-face)))))

(ert-deftest sed-ts-mode-distinguishes-regexp-control-and-general-escapes ()
  (sed-ts-mode-test--require-grammar 'sed)
  (let ((treesit-font-lock-level 4)
        (control "s/\\n/\\&/")
        (replacement "s/a/x\\/y/")
        (translation "y/abc/x\\/z/"))
    (sed-ts-mode-test--with-script 'bre
        (concat control "\n" replacement "\n" translation "\n")
      (font-lock-ensure)
      (sed-ts-mode-test--should-fontify
       `((,control "\\n" font-lock-escape-face)
         (,control "\\&" font-lock-escape-face)
         (,replacement "\\/" font-lock-escape-face)
         (,translation "\\/" font-lock-escape-face))))))

(ert-deftest sed-ts-mode-fontifies-by-font-lock-level ()
  (sed-ts-mode-test--require-grammar 'sed)
  (dolist (case '((1 font-lock-comment-face nil nil nil nil)
                  (2 font-lock-keyword-face font-lock-keyword-face
                     font-lock-constant-face nil nil)
                  (3 font-lock-keyword-face font-lock-keyword-face
                     font-lock-constant-face font-lock-constant-face nil)
                  (4 font-lock-keyword-face font-lock-keyword-face
                     font-lock-constant-face font-lock-constant-face
                     font-lock-regexp-face)))
    (pcase-let ((`(,level ,directive ,command ,label ,flag ,expression) case))
      (let ((treesit-font-lock-level level))
        (sed-ts-mode-test--with-script 'bre
            "#n note\n# plain\np\n:again\ns/x/y/g\n"
          (font-lock-ensure)
          (sed-ts-mode-test--should-fontify
           `(("#n note" "#" ,directive)
             ("#n note" "n" ,directive)
             ("#n note" "note" font-lock-comment-face)
             ("# plain" "plain" font-lock-comment-face)
             ("p" "p" ,command)
             (":again" "again" ,label)
             ("s/x/y/g" "g" ,flag)
             ("s/x/y/g" "x" ,expression))))))))

(ert-deftest sed-ts-mode-fontifies-chunks-like-the-whole-script ()
  (sed-ts-mode-test--require-grammar 'sed)
  (let ((source "1,2s/a/b/g\n#n\n{\n/x/{\np\n}\ny/a/b/\n}\na\\\ntext\n"))
    (dolist (chunk '(7 97))
      (should
       (equal
        (sed-ts-mode-test--with-regexp-syntax 'bre
          (with-temp-buffer
            (let ((treesit-font-lock-level 4))
              (insert source)
              (sed-ts-mode)
              (let ((position (point-min)))
                (while (< position (point-max))
                  (let ((end (min (point-max) (+ position chunk))))
                    (font-lock-fontify-region position end)
                    (setq position end))))
              (sed-ts-mode-test--face-map))))
        (sed-ts-mode-test--with-regexp-syntax 'bre
          (with-temp-buffer
            (let ((treesit-font-lock-level 4))
              (insert source)
              (sed-ts-mode)
              (font-lock-ensure)
              (sed-ts-mode-test--face-map)))))))))

(ert-deftest sed-ts-mode-reports-an-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_) nil))
          (sed-ts-mode-test--with-regexp-syntax 'bre
            (with-temp-buffer
              (let ((error-data
                     (should-error (sed-ts-mode) :type 'user-error)))
                (should (string-match-p
                         "sed" (error-message-string error-data)))
                (should-not (treesit-parser-list nil nil t))))))
      (fset 'treesit-ensure-installed ensure))))

(provide 'sed-ts-mode-test)

;;; sed-ts-mode-test.el ends here
