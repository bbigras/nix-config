{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wayvr
  ];

  services.wivrn = {
    enable = true;
    openFirewall = true;
    autoStart = false;
  };
}
