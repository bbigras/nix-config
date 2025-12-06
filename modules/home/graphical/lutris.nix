{ pkgs, osConfig, ... }:

{
  programs.lutris = {
    enable = true;
    defaultWinePackage = pkgs.proton-ge-bin;
    extraPackages = with pkgs; [
      mangohud
      winetricks
      gamescope
      gamemode
      umu-launcher
    ];
    protonPackages = [ pkgs.proton-ge-bin ];
    steamPackage = osConfig.programs.steam.package;
    winePackages = [
      # pkgs.wineWow64Packages.full
      pkgs.proton-ge-bin
    ];
  };
}
