{ pkgs, ... }:

{
  environment = {
    cosmic.excludePackages = with pkgs; [
      cosmic-edit
      cosmic-ext-calculator
      cosmic-player
      cosmic-reader
      cosmic-store
      cosmic-term
    ];

    sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;

    systemPackages = with pkgs; [
      cosmic-ext-applet-caffeine
    ];
  };

  services = {
    desktopManager.cosmic.enable = true;
    displayManager.cosmic-greeter.enable = true;
  };
}
