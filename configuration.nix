### NixOS configuration file

{
  ## =================================================================
  ## Base version of NixOS
  ## =================================================================

  system.stateVersion = "24.05";

  ## =================================================================
  ## Automatic upgrades
  ## =================================================================

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  system.autoUpgrade.channel = "https://channels.nixos.org/nixos-unstable";

  ## =================================================================
  ## Copy of this file at /run/current-system/configuration.nix
  ## =================================================================

  system.copySystemConfiguration = true;

  ## =================================================================
  ## Other configuration files
  ## =================================================================

  imports = [
    ./hardware-configuration.nix
    "${builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz"}/nixos"
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
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.variant = "";

  ## =================================================================
  ## Printing
  ## =================================================================

  services.printing.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns4 = true;
  services.avahi.openFirewall = true;

  ## =================================================================
  ## Sound
  ## =================================================================

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

    home.stateVersion = "24.05";

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
    ## Java
    ## ===============================================================

    programs.java.enable = true;

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
      agda2-mode
      auctex
      bnfc
      company
      ess
      haskell-mode
      htmlize
      magit
      nix-mode
      pdf-tools
      sml-mode
      yasnippet
    ];

    programs.emacs.extraConfig = builtins.readFile ./default.el;

    home.file."init.el".target = ".emacs.d/init.el";
    home.file."init.el".source = ./init.el;

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
      texlivePackage = pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-full;
        noweb = { pkgs = [ pkgs.noweb ]; };
      };
      aspellPackage = pkgs.aspellWithDicts (dicts:
        with dicts; [
          en
          en-computers
          en-science
        ]
      );
      jlex = pkgs.stdenv.mkDerivation rec {
        pname = "jlex";
        version = "1.2.6";
        src = pkgs.fetchurl {
          url = "https://www.cs.princeton.edu/~appel/modern/java/JLex/Archive/1.2.6/Main.java";
          sha256 = "1msblmsgzij3z9pwm7gff1q2cv1q802q23xsn0mrflrs7g7axsxf";
        };
        dontUnpack = true;
        buildInputs = [ pkgs.jdk ];
        buildPhase = ''
          cp ${src} Main.java
          javac -d . Main.java
          jar cf ${pname}-${version}.jar JLex/*.class
        '';
        installPhase = ''
          mkdir -p $out/share/java
          cp ${pname}-${version}.jar $out/share/java
        '';
      };
      rExtraPackages = [
        pkgs.rPackages.ISwR
      ];
      rPackage = pkgs.rWrapper.override {
        packages = rExtraPackages;
      };
      rstudioPackage = pkgs.rstudioWrapper.override {
        packages = rExtraPackages;
      };
    in
      [
        aspellPackage
        jlex
        pkgs.bashmount
        pkgs.binutils
        pkgs.cloc
        pkgs.ffmpeg-full
        pkgs.fpc
        pkgs.gnumake
        pkgs.gparted
        pkgs.haskellPackages.alex
        pkgs.haskellPackages.BNFC
        pkgs.haskellPackages.ghc
        pkgs.haskellPackages.happy
        pkgs.haskellPackages.hoogle
        pkgs.id3lib
        pkgs.id3v2
        pkgs.jasmin
        pkgs.javaCup
        pkgs.jmtpfs
        pkgs.lohit-fonts.telugu
        pkgs.libreoffice
        pkgs.mlton
        pkgs.mmixware
        pkgs.mplayer
        pkgs.noweb
        pkgs.pdftk
        pkgs.perlPackages.MP3Tag
        pkgs.polyml
        pkgs.python3Packages.cram
        pkgs.qpdf
        pkgs.rclone
        pkgs.recutils
        pkgs.scowl
        pkgs.smlnj
        pkgs.speedtest-cli
        pkgs.stremio
        pkgs.thunderbird
        pkgs.tor-browser-bundle-bin
        pkgs.unzip
        pkgs.vim
        pkgs.vlc
        pkgs.wget
        pkgs.yt-dlp
        rPackage
        rstudioPackage
        texlivePackage
      ];
  };
}

### End of file
