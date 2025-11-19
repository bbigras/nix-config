{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cosmic-ext-applet-caffeine
  ];

  environment.cosmic.excludePackages = with pkgs; [
    cosmic-edit
    cosmic-ext-calculator
    cosmic-player
    cosmic-reader
    cosmic-store
    cosmic-term
  ];

  services = {
    desktopManager.cosmic.enable = true;
    displayManager.cosmic-greeter.enable = true;
  };
  environment.sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;
}
