{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wlx-overlay-s
  ];

  services.wivrn = {
    enable = true;
    openFirewall = true;
    defaultRuntime = true;
    autoStart = false;
  };
}
