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
    <home-manager/nixos>
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
  services.upower.enable = true;
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
  home-manager.useUserPackages = true;

  ## =================================================================
  ## User settings of Home Manager
  ## =================================================================

  home-manager.users.raghnysh = { pkgs, ... }: {
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
    ## Git
    ## ===============================================================

    programs.git.enable = true;
    programs.git.userName = "Raghavendra Nyshadham";
    programs.git.userEmail = "rn@raghnysh.com";
    programs.git.extraConfig.status.showUntrackedFiles = "all";

    ## ===============================================================
    ## TeX Live
    ## ===============================================================

    programs.texlive.enable = true;

    programs.texlive.extraPackages = tpkgs: {
      inherit (tpkgs) scheme-full;

      pkgFilter = pkg:
        pkgs.lib.elem pkg.tlType [ "run" "bin" "doc" ];

      noweb = { pkgs = [ pkgs.noweb ]; };
    };

    ## ===============================================================
    ## Emacs
    ## ===============================================================

    programs.emacs.enable = true;
    programs.emacs.package = pkgs.emacs29-pgtk;

    programs.emacs.extraPackages = epkgs: with epkgs; [
      magit
      nix-mode
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
      (modify-all-frames-parameters '((fullscreen . maximized)))
      (defvar my-laptop-monitor-font "DejaVu Sans Mono 16")
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
      (customize-set-variable 'auto-save-visited-interval 2)
      (auto-save-visited-mode 1)
      (customize-set-variable 'auto-revert-interval 2)
      (global-auto-revert-mode 1)
      (global-display-fill-column-indicator-mode 1)
    '';
    '';

    ## ===============================================================
    ## Firefox
    ## ===============================================================

    programs.firefox.enable = true;

    ## ===============================================================
    ## Packages that are not Home Manager modules
    ## ===============================================================

    home.packages = [
      pkgs.noweb
    ];
  };
}

### End of file
