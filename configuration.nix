### NixOS configuration file

{ config, pkgs, ... }:

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

    ## ===============================================================
    ## Git
    ## ===============================================================

    programs.git.enable = true;

    ## ===============================================================
    ## Emacs
    ## ===============================================================

    programs.emacs.enable = true;
    programs.emacs.extraPackages = epkgs: with epkgs; [
      magit
      nix-mode
    ];

    ## ===============================================================
    ## Firefox
    ## ===============================================================

    programs.firefox.enable = true;
  };
}

### End of file
