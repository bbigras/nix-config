{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cosmic-ext-applet-caffeine
  ];

  services = {
    desktopManager.cosmic.enable = true;
    displayManager.cosmic-greeter.enable = true;
  };
  environment.sessionVariables.COSMIC_DATA_CONTROL_ENABLED = 1;
}
