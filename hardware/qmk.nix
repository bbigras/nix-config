{ pkgs, ... }:

{
  hardware.keyboard.qmk.enable = true;
  environment.systemPackages = with pkgs; [
    via
  ];
  services.udev.packages = [ pkgs.via ];
}
