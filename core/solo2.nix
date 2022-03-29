{ pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      solo2-cli
    ];
  };

  services.udev.packages = [
    pkgs.solo2-cli
  ];
}
