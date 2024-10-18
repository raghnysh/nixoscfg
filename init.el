;;; Emacs init file

;; ===================================================================
;; Appearance
;; ===================================================================

(setq inhibit-startup-screen t)
(setq frame-title-format '("Emacs: " (:eval (or buffer-file-name dired-directory (buffer-name)))))
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode -1)
(load-theme 'modus-operandi)

;; ===================================================================
;; Default fonts
;; ===================================================================

(defvar my-small-font "DejaVu Sans Mono 14"
  "Small font that is mainly for my external monitor.")

(defvar my-large-font "DejaVu Sans Mono 18"
  "Large font that is mainly for my laptop monitor.")

(modify-all-frames-parameters `((font . ,my-small-font)))

(defun my-frame-set-small-font (&optional frame)
  "Set the font of the frame FRAME to `my-small-font'.
FRAME defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'font my-small-font))

(defun my-frame-set-large-font (&optional frame)
  "Set the font of the frame FRAME to `my-large-font'.
FRAME defaults to the selected frame."
  (interactive)
  (set-frame-parameter frame 'font my-large-font))

(keymap-global-set "s--" #'my-frame-set-small-font)
(keymap-global-set "s-=" #'my-frame-set-large-font)

;; ===================================================================
;; Fonts for special characters
;; ===================================================================

(set-fontset-font t '(#x1d7d8 . #x1d7e1) "DejaVu Sans")
(set-fontset-font t '(#x1d538 . #x1d56b) "DejaVu Sans")

;; ===================================================================
;; `custom' library settings
;; ===================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; ===================================================================
;; Settings to all modes
;; ===================================================================

(setq-default indent-tabs-mode nil)

;; ===================================================================
;; Interface settings
;; ===================================================================

(setopt auto-save-visited-interval 2)
(auto-save-visited-mode 1)

(setopt auto-revert-interval 2)
(global-auto-revert-mode 1)

(global-display-fill-column-indicator-mode 1)
(setq make-backup-files nil)
(tab-bar-mode 1)
(repeat-mode 1)

;; ===================================================================
;; Text mode
;; ===================================================================

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'text-mode-hook 'turn-on-flyspell)
(setq ispell-alternate-dictionary (expand-file-name "~/.nix-profile/share/dict/wbritish.txt"))
(setq ispell-silently-savep t)

;; ===================================================================
;; Input methods
;; ===================================================================

(set-language-environment "UTF-8")
(setq default-input-method "Agda")

;; ===================================================================
;; Compilation
;; ===================================================================

(setq-default compilation-scroll-output 'first-error)

(defvar my-compilation-frame-name "compilation"
  "The name of a dedicated frame for compilation buffers.")

(add-to-list 'display-buffer-alist
             `(,(rx (and string-start "*compilation*" string-end))
               (display-buffer-reuse-window display-buffer-pop-up-frame)
               (reusable-frames . t)
               (pop-up-frame-parameters . ((name . ,my-compilation-frame-name)
                                           (height . 20)
                                           (width . 80)
                                           (user-position . t)
                                           (top . 0)
                                           (left . 0)))))

(defun my-delete-compilation-frame (_buffer message)
  "Delete the compilation frame if MESSAGE declares success."
  (let* ((successful (string-match "\\bfinished\\b" message))
         (compilation-frame
          (car (filtered-frame-list
                #'(lambda (frame)
                    (string-equal (frame-parameter frame 'name)
                                  my-compilation-frame-name)))))
         (alive (and compilation-frame
                     (framep compilation-frame)
                     (frame-live-p compilation-frame))))
    (when (and successful alive)
      (delete-frame compilation-frame))))

(add-hook 'compilation-finish-functions #'my-delete-compilation-frame)

;; ===================================================================
;; Company mode
;; ===================================================================

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (keymap-set company-active-map "<down>" #'company-complete-common-or-cycle))

(setq company-show-quick-access t)

;; ===================================================================
;; Yasnippet templates
;; ===================================================================

(yas-global-mode 1)

;; ===================================================================
;; Makefile commands
;; ===================================================================

(defun my-make (target)
  "Run `make TARGET' in the current directory."
  (save-some-buffers t)
  (let* ((compile-command (format "make %s" target)))
    (recompile)))

(defun my-make-keymap-set (keymap)
  "Set key bindings for `make' commands in keymap KEYMAP."
  (keymap-set keymap "<kp-left>"
              #'(lambda ()
                  "Run `make all' in the current directory."
                  (interactive)
                  (my-make "all")))
  (keymap-set keymap "<kp-right>"
              #'(lambda ()
                  "Run `make clean' in the current directory."
                  (interactive)
                  (my-make "clean")))
  (keymap-set keymap "<kp-up>"
              #'(lambda ()
                  "Run `make upload' in the current directory."
                  (interactive)
                  (my-make "upload"))))

(add-hook 'LaTeX-mode-hook #'(lambda () (my-make-keymap-set LaTeX-mode-map)))
(add-hook 'org-mode-hook #'(lambda () (my-make-keymap-set org-mode-map)))

;; ===================================================================
;; AUCTeX
;; ===================================================================

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master t)
(add-to-list 'auto-mode-alist '("\\.nw\\'" . LaTeX-mode))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (LaTeX-math-mode 1)
              (keymap-set LaTeX-math-mode-map "\140 c"
                          #'(lambda ()
                              (interactive "*c\nP")
                              (when dollar (insert "$"))
                              (insert "\\mathscr{" (char-to-string char) "}")
                              (when dollar (insert "$"))))))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (add-to-list 'TeX-file-extensions "nw")
              (LaTeX-add-environments '("codechunk" LaTeX-env-label)
                                      '("theorem" LaTeX-env-label)
                                      '("exercise" LaTeX-env-label)
                                      '("solution" LaTeX-env-label))
              (dolist (label '(("codechunk" . "chk:")
                               ("theorem" . "thm:")
                               ("exercise" . "exe:")
                               ("solution" . "sol:")))
                (add-to-list 'LaTeX-label-alist label))
              (add-to-list 'LaTeX-verbatim-environments "codechunk")
              (add-to-list 'LaTeX-indent-environment-list
                           '("codechunk" current-indentation))
              (add-to-list 'LaTeX-verbatim-macros-with-delims "nwverb")
              (font-latex-setup)))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (font-latex-add-keywords '(("firstterm" "\{")) 'italic-command)
              (font-latex-add-keywords '("maketitle" "tableofcontents") 'function)
              (font-latex-add-keywords '(("title" "\{") ("author" "\{") ("date" "\{"))
                                       'textual)
              (font-latex-setup)))

(yas-define-snippets 'latex-mode
                     '(;; Analogue of DocBook `firstterm'
                       ("zft" "\\firstterm{$1}$0")
                       ;; Noweb identifier
                       ("zv" "\\nwverb|$1|$0")
                       ;; Inline math mode
                       ("zm" "\\\\($1\\\\)$0")
                       ;; Noweb code chunk
                       ("zcc" "\<\<$1\>\>=\n$0\n@")
                       ;; Set builder notation
                       ("zsetb" "\\\\{ $1 \\,\\vert\\, $2 \\\\}$0")))

;; ===================================================================
;; RefTeX
;; ===================================================================

(add-hook 'LaTeX-mode-hook #'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-bibliography-commands '(".*addbibresource"))
(setq reftex-cite-format 'biblatex)
(setq reftex-insert-label-flags '(nil nil))

(setq reftex-label-alist
      '(("codechunk" ?k "chk:" "\\subpageref{%s}" nil ("code chunk"))
        ("equation" ?e "eqn:" "\\ref{%s}" nil ("equation"))
        ("theorem" ?h "thm:" "\\ref{%s}" nil ("theorem"))
        ("exercise" ?x "exe:" "\\ref{%s}" nil ("exercise"))
        ("solution" ?n "sol:" "\\ref{%s}" nil ("solution"))))

(add-hook 'reftex-mode-hook
          #'(lambda ()
              (add-to-list 'reftex-label-regexps "\\\\nextchunklabel{\\(?1:[^}]*\\)}")))

(require 'calc-bin)

(defun my-base-36 (number)
  "Return the base 36 representation of NUMBER."
  (let ((calc-number-radix 36))
    (downcase (math-format-radix number))))

(defun my-label (maxlength &optional padded padright)
  "Return a lower-case alphanumeric label of length at most MAXLENGTH.
      If PADDED is non-nil, the label is padded with `0' characters so
      that its length equals MAXLENGTH.  If PADRIGHT is also non-nil,
      the padding is inserted on the right rather than the left."
  (let* ((limit (expt 36 maxlength))
         (template (concat "%"
                           (if padded
                               (concat (if padright "-" "") "0")
                             "")
                           (int-to-string maxlength)
                           "a"))
         (number (random limit))
         (spec-alist (list (cons ?a (my-base-36 number)))))
    (format-spec template spec-alist)))

(setq reftex-format-label-function
      #'(lambda (label format)
          (let ((label-prefix (car (split-string label ":")))
                (new-label (my-label 8 t t)))
            (if (string= label-prefix "chk")
                (format "\\nextchunklabel{chk:%s}" new-label)
              (format format (concat label-prefix ":" new-label))))))

(add-hook 'reftex-mode-hook
          #'(lambda ()
              (add-to-list 'reftex-ref-style-alist '("Personal" "personal"
                                                     (("\\ref" ?\C-m)
                                                      ("\\Cref" ?C)
                                                      ("\\cref" ?c)
                                                      ("\\cpageref" ?d)
                                                      ("\\pageref" ?p)
                                                      ("\\Cpageref" ?D)
                                                      ("\\Ref" ?R))))
              (setq reftex-ref-style-default-list '("Personal"))))

(add-hook 'reftex-mode-hook
          #'(lambda ()
              (keymap-set reftex-mode-map
                          "C-c )"
                          #'(lambda ()
                              (interactive)
                              (let ((current-prefix-arg '(4)))
                                (reftex-reference))))))

(setq reftex-find-label-regexp-format "\\(label[[:space:]]*=[[:space:]]*\\|\\\\label\\|\\\\nextchunklabel\\)\\([[{][^]}]*[]}]\\)*[[{]\\(%s\\)[]}]")

;; ===================================================================
;; PDF viewer
;; ===================================================================

(pdf-tools-install)
(add-hook 'pdf-view-mode-hook
          #'(lambda ()
              (pdf-view-fit-page-to-window)))
(setq pdf-view-midnight-colors '("black" . "gray98"))
(add-to-list 'pdf-tools-enabled-modes 'pdf-view-midnight-minor-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

;; ===================================================================
;; BibTeX
;; ===================================================================

(setq bibtex-dialect 'biblatex)
(setq bibtex-user-optional-fields nil)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-autokey-edit-before-use nil)
(setq bibtex-maintain-sorted-entries t)
(setq fill-column 700)

(setq bibtex-autokey-before-presentation-function
      #'(lambda (_key)
          (format "bib:%s" (my-label 8 t t))))

(add-hook 'bibtex-mode-hook
          #'(lambda ()
              (dolist (spec '(whitespace realign last-comma delimiters unify-case braces sort-fields))
                (add-to-list 'bibtex-entry-format spec))))

;; ===================================================================
;; Org mode
;; ===================================================================

(setq org-src-preserve-indentation t)

;;; End of file
