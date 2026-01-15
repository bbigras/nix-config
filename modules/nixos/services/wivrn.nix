{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wayvr
  ];

  services.wivrn = {
    enable = true;
    openFirewall = true;
    defaultRuntime = true;
    autoStart = false;
  };
}
