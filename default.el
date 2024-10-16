(setq frame-title-format
      '("Emacs: " (:eval (or buffer-file-name
                             dired-directory
                             (buffer-name)))))

(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(load-theme 'modus-operandi)
(defvar my-laptop-monitor-font "DejaVu Sans Mono 18")
(defvar my-external-monitor-font "DejaVu Sans Mono 14")
(modify-all-frames-parameters `((font . ,my-laptop-monitor-font)))
(defvar my-laptop-monitor-name "0x08c6")
(defvar my-external-monitor-name "DELL U2415")

(defun my-frame-monitor-name (&optional frame)
  (frame-monitor-attribute 'name frame))

(defun my-monitor-external-p (monitor-name)
  (string-equal monitor-name my-external-monitor-name))

(defun my-set-frame-font-for-monitor (&optional frame monitor-name)
  (interactive)
  (set-frame-parameter frame 'font
                       (if (my-monitor-external-p (or monitor-name
                                                      (my-frame-monitor-name frame)))
                           my-external-monitor-font
                         my-laptop-monitor-font)))

(defun my-set-frame-font-for-laptop-monitor (&optional frame)
  (interactive)
  (my-set-frame-font-for-monitor frame my-laptop-monitor-name))

(defun my-set-frame-font-for-external-monitor (&optional frame)
  (interactive)
  (my-set-frame-font-for-monitor frame my-external-monitor-name))

(keymap-global-set "s-0" #'my-set-frame-font-for-monitor)
(keymap-global-set "s-=" #'my-set-frame-font-for-laptop-monitor)
(keymap-global-set "s--" #'my-set-frame-font-for-external-monitor)

(defun my-set-focused-frames-font-for-monitor ()
  (dolist (frame (frame-list))
    (when (eq (frame-focus-state frame) t)
      (my-set-frame-font-for-monitor frame))))

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2016-05/msg00172.html
;; The hook after-make-frame-functions used there is now
;; deprecated, and replaced with after-focus-change-function.
(add-function :after after-focus-change-function
              #'my-set-focused-frames-font-for-monitor)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default indent-tabs-mode nil)
(set-fontset-font t '(#x1d7d8 . #x1d7e1) "DejaVu Sans")
(set-fontset-font t '(#x1d538 . #x1d56b) "DejaVu Sans")
(setopt auto-save-visited-interval 2)
(auto-save-visited-mode 1)
(setopt auto-revert-interval 2)
(global-auto-revert-mode 1)
(global-display-fill-column-indicator-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(setq make-backup-files nil)
(setq ispell-alternate-dictionary "/etc/profiles/per-user/raghnysh/share/dict/wbritish.txt")
(setq ispell-silently-savep t)
(tab-bar-mode 1)
(repeat-mode 1)

(require 'agda-input)
(set-language-environment "UTF-8")
(setq default-input-method "Agda")

(setq-default compilation-scroll-output 'first-error)
(setq my-compilation-frame-name "compilation")

(add-to-list 'display-buffer-alist
             `(,(rx (and string-start "*compilation*" string-end))
               (display-buffer-reuse-window
                display-buffer-pop-up-frame)
               (reusable-frames . t)
               (pop-up-frame-parameters
                .
                ((name . ,my-compilation-frame-name)
                 (height . 20)
                 (width . 80)
                 (user-position . t)
                 (top . 0)
                 (left . 0)))))

(defun my-delete-compilation-frame (buffer message)
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

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (keymap-set company-active-map "<down>"
              #'company-complete-common-or-cycle))
(setq company-show-numbers t)

(yas-global-mode 1)

(setq org-src-preserve-indentation t)

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master t)

(add-to-list 'auto-mode-alist '("\\.nw\\'" . LaTeX-mode))

(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode
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

(defun my-make (target)
  "Compile the current LaTeX document."
  (save-some-buffers t)
  (let* ((compile-command (format "make %s" target)))
    (recompile)))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (keymap-set LaTeX-mode-map "<kp-left>"
                          #'(lambda ()
                              (interactive)
                              (my-make "all")))
              (keymap-set LaTeX-mode-map "<kp-right>"
                          #'(lambda ()
                              (interactive)
                              (my-make "clean")))))

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

(pdf-tools-install)
(add-hook 'pdf-view-mode-hook
          #'(lambda ()
              (pdf-view-fit-page-to-window)))
(setq pdf-view-midnight-colors '("black" . "gray98"))
(add-to-list 'pdf-tools-enabled-modes 'pdf-view-midnight-minor-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

(setq bibtex-dialect 'biblatex)
(setq bibtex-user-optional-fields nil)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-autokey-edit-before-use nil)
(setq bibtex-maintain-sorted-entries t)
(setq fill-column 700)

(setq bibtex-autokey-before-presentation-function
      #'(lambda (_key)
          (format "bib:%s" (my-label 8 t t))))

(setq bibtex-autokey-titleword-ignore
      '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
        "[^[:upper:][:lower:]].*" ".*[^[:upper:][:lower:]0-9].*"))

(add-hook 'bibtex-mode-hook
          #'(lambda ()
              (dolist (spec '(whitespace realign last-comma delimiters unify-case braces sort-fields))
                (add-to-list 'bibtex-entry-format spec))))
