{ pkgs, ... }:

{
  programs = {
    steam = {
      remotePlay.openFirewall = true;
      extraCompatPackages = [ pkgs.proton-ge-bin ];
    };
    gamemode.enable = true;
    gamescope.enable = true;
  };
}
