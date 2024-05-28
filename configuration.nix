### NixOS configuration file

{
  ## =================================================================
  ## Base version of NixOS
  ## =================================================================

  system.stateVersion = "23.05";

  ## =================================================================
  ## Copy of this file at /run/current-system/configuration.nix
  ## =================================================================

  system.copySystemConfiguration = true;

  ## =================================================================
  ## Other configuration files
  ## =================================================================

  imports = [
    ./hardware-configuration.nix
    "${builtins.fetchTarball {
      url = "https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz";
      sha256 = "0rwzab51hnr6cmm1w5zmfh29gbkg6byv8jnr7frcv5kd6m8kna41";
    }}/nixos"
  ];

  ## =================================================================
  ## Bootloader
  ## =================================================================

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  ## =================================================================
  ## Networking
  ## =================================================================

  networking.hostName = "bastet";
  networking.networkmanager.enable = true;

  ## =================================================================
  ## Date and time
  ## =================================================================

  time.timeZone = "Asia/Kolkata";

  ## =================================================================
  ## Internationalisation
  ## =================================================================

  i18n.defaultLocale = "en_GB.UTF-8";
  i18n.extraLocaleSettings.LC_COLLATE = "C.UTF-8";

  ## =================================================================
  ## Graphical environment
  ## =================================================================

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "";

  ## =================================================================
  ## Printing
  ## =================================================================

  services.printing.enable = true;

  ## =================================================================
  ## Sound
  ## =================================================================

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire.enable = true;
  services.pipewire.alsa.enable = true;
  services.pipewire.alsa.support32Bit = true;
  services.pipewire.pulse.enable = true;

  ## =================================================================
  ## Non-free packages
  ## =================================================================

  nixpkgs.config.allowUnfree = true;

  ## =================================================================
  ## Suspending when lid closed even with external monitor attached
  ## =================================================================

  ## https://apiraino.github.io/ubuntu-gnome-power/

  services.logind.lidSwitchDocked = "suspend";
  services.upower.ignoreLid = true;

  ## =================================================================
  ## Limits
  ## =================================================================

  ## Avoid "too many open files" error when installing TeX Live with
  ## documentation.  nixos-rebuild has to be run with this setting
  ## before it is run for the TeX Live installation.
  ## https://github.com/NixOS/nixpkgs/issues/171218#issuecomment-1114708931

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "300000";
    }
  ];

  ## =================================================================
  ## Overlays
  ## =================================================================

  nixpkgs.overlays = [
    (final: prev: {
      ## Copy noweb's LaTeX style files to $out/tex/latex/noweb so
      ## that they are put in the correct directory (with respect to
      ## the TeX Directory Strucuture (TDS)) under TeX Live's
      ## TEXMFDIST when noweb is used as an extra package of TeX Live.
      ## This is necessary for TeX Live's kpsewhich to locate these
      ## files.
      noweb = prev.noweb.overrideAttrs (oldAttrs : {
        postInstall = oldAttrs.postInstall + "cp -R $tex/tex $out";
      });
    })
  ];

  ## =================================================================
  ## Users
  ## =================================================================

  users.users.raghnysh.isNormalUser = true;
  users.users.raghnysh.description = "Raghavendra Nyshadham";
  users.users.raghnysh.extraGroups = [ "networkmanager" "wheel" ];

  ## =================================================================
  ## General settings of Home Manager
  ## =================================================================

  home-manager.useGlobalPkgs = true;

  ## =================================================================
  ## User settings of Home Manager
  ## =================================================================

  home-manager.users.raghnysh = { pkgs, lib, ... }: {
    ## ===============================================================
    ## Base version of Home Manager
    ## ===============================================================

    home.stateVersion = "23.05";

    ## ===============================================================
    ## Custom keybindings
    ## ===============================================================

    dconf.settings."org/gnome/settings-daemon/plugins/media-keys".custom-keybindings = [
      "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/suspend/"
    ];

    ## After I wrote this, I found from the Thinkpad p15v Gen 1 User
    ## Guide that "Fn 4" suspends the computer, but I am keeping this
    ## as a reminder of how to set custom key bindings in this file.
    dconf.settings."org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/suspend".name = "Suspend the computer";
    ## Pause = "Fn P" (Thinkpad p15v Gen 1 User Guide, page 17)
    dconf.settings."org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/suspend".binding = "Pause";
    dconf.settings."org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/suspend".command = "systemctl suspend";

    ## ===============================================================
    ## Input methods
    ## ===============================================================

    ## https://discourse.nixos.org/t/keyboard-layout-with-gnome/21996/9
    dconf.settings."org/gnome/desktop/input-sources".show-all-sources = true;

    dconf.settings."org/gnome/desktop/input-sources".sources = [
      (lib.hm.gvariant.mkTuple [ "xkb" "us" ])
      (lib.hm.gvariant.mkTuple [ "xkb" "in+tel-sarala" ])
    ];

    dconf.settings."org/gnome/desktop/input-sources".xkb-options = [ "terminate:ctrl_alt_bksp" ];

    ## ===============================================================
    ## Display of notifications on locked screen
    ## ===============================================================

    dconf.settings."org/gnome/desktop/notifications".show-in-lock-screen = false;

    ## ===============================================================
    ## Cursor theme
    ## ===============================================================

    gtk.enable = true;
    gtk.cursorTheme.package = pkgs.bibata-cursors;
    gtk.cursorTheme.name = "Bibata-Modern-Amber";
    gtk.cursorTheme.size = 50;

    ## ===============================================================
    ## Bash
    ## ===============================================================

    programs.bash.enable = true;

    programs.bash.enableCompletion = true;

    programs.bash.initExtra = ''
      prompt_bold_blue="\[\033[01;34m\]"
      prompt_normal="\[\033[00m\]"
      if test $TERM = "dumb" ; then
        PS1="\$ "
      else
        PS1="[$prompt_bold_blue\h:\w$prompt_normal]\$ "
      fi
      PROMPT_COMMAND="history -a; history -n; $PROMPT_COMMAND"
    '';

    programs.bash.historyControl = [
      "ignorespace" "ignoredups" "erasedups"
    ];

    programs.bash.historyIgnore = [
      "history" "ls" "cd" "bg" "fg" "ps" "top" "exit"
    ];

    programs.bash.historySize = 20000;
    programs.bash.historyFileSize = 200000;

    programs.bash.sessionVariables = {
      EDITOR = "nano";
      IGNOREOF = 0;
      LESS = "-R";
      PAGER = "less";
    };

    programs.bash.shellOptions = [
      "checkjobs"
      "checkwinsize"
      "cmdhist"
      "extglob"
      "globstar"
      "histappend"
    ];

    programs.bash.shellAliases = {
      cp = "cp -i";
      mv = "mv -i";
      rm = "rm -i";
      ls = "ls --classify --color";
      rsync = "rsync -a -e ssh --progress";
    };

    ## ===============================================================
    ## Readline
    ## ===============================================================

    programs.readline.enable = true;

    programs.readline.bindings = {
      "\\e[A" = "history-search-backward";
      "\\e[B" = "history-search-forward";
      "\\e/" = "dabbrev-expand";
    };

    programs.readline.variables = {
      history-size = 10000;
      show-all-if-ambiguous = true;
    };

    ## ===============================================================
    ## Direnv
    ## ===============================================================

    programs.direnv.enable = true;

    ## ===============================================================
    ## Git
    ## ===============================================================

    programs.git.enable = true;
    programs.git.userName = "Raghavendra Nyshadham";
    programs.git.userEmail = "rn@raghnysh.com";
    programs.git.extraConfig.status.showUntrackedFiles = "all";

    ## ===============================================================
    ## GNU Info
    ## ===============================================================

    programs.info.enable = true;

    ## ===============================================================
    ## Emacs
    ## ===============================================================

    programs.emacs.enable = true;
    programs.emacs.package = pkgs.emacs29-pgtk;

    programs.emacs.extraPackages = epkgs: with epkgs; [
      agda-input
      auctex
      magit
      nix-mode
      pdf-tools
      yasnippet
    ];

    programs.emacs.extraConfig = ''
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

      (yas-global-mode 1)

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
                                            '("coderemark" LaTeX-env-label)
                                            '("definition" LaTeX-env-label)
                                            '("interjection")
                                            '("question" ["Number"])
                                            '("answer")
                                            '("notation" LaTeX-env-label)
                                            '("remark" LaTeX-env-label))
                    (dolist (label '(("codechunk" . "chk:")
                                     ("coderemark" . "crk:")
                                     ("definition" . "def:")
                                     ("notation" . "not:")
                                     ("remark" . "rem:")))
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
                           '(("zft" "\\firstterm{$1}$0")
                             ("zv" "\\nwverb|$1|$0")
                             ("zm" "\\\\($1\\\\)$0")
                             ("zcc" "\<\<$1\>\>=\n$0\n@")))

      (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
      (setq reftex-plug-into-AUCTeX t)
      (setq reftex-bibliography-commands '(".*addbibresource"))
      (setq reftex-cite-format 'biblatex)
      (setq reftex-insert-label-flags '(nil nil))

      (setq reftex-label-alist
            '(("codechunk" ?k "chk:" "\\subpageref{%s}" nil ("code chunk"))
              ("coderemark" ?c "crk:" "\\ref{%s}" nil ("code remark"))
              ("definition" ?d "def:" "\\ref{%s}" nil ("definition"))
              ("notation" ?n "not:" "\\ref{%s}" nil ("notation"))
              ("remark" ?r "rem:" "\\ref{%s}" nil ("remark"))))

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
    '';

    home.file."init.el".target = ".emacs.d/init.el";
    home.file."init.el".text = ''
      (setq inhibit-startup-screen t)
    '';

    ## ===============================================================
    ## Firefox
    ## ===============================================================

    programs.firefox.enable = true;

    ## ===============================================================
    ## Fonts
    ## ===============================================================

    fonts.fontconfig.enable = true;

    ## ===============================================================
    ## Packages that are not Home Manager modules
    ## ===============================================================

    home.packages = let
      pkgs2211Path = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz";
        sha256 = "1xi53rlslcprybsvrmipm69ypd3g3hr7wkxvzc73ag8296yclyll";
      };
      pkgs2211 = import pkgs2211Path { config = pkgs.config; };
      texlivePackage = pkgs2211.texlive.combine {
        inherit (pkgs2211.texlive) scheme-full;
        pkgFilter = pkg:
          pkgs2211.lib.elem pkg.tlType [ "run" "bin" "doc" ];
        noweb = { pkgs = [ pkgs.noweb ]; };
      };
      texlivePackageNoCollisions = texlivePackage.override {
        ignoreCollisions = true;
      };
      aspellPackage = pkgs.aspellWithDicts (dicts:
        with dicts; [
          en
          en-computers
          en-science
        ]
      );
    in
      [
        aspellPackage
        pkgs.gnumake
        pkgs.lohit-fonts.telugu
        pkgs.noweb
        pkgs.scowl
        texlivePackageNoCollisions
      ];
  };
}

### End of file
