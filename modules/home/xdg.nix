{ pkgs, ... }:
{
  xdg = {
    enable = true;
    mimeApps.enable = pkgs.stdenv.isLinux;
    userDirs = {
      enable = pkgs.stdenv.isLinux;
      desktop = "$HOME/Bureau";
      documents = "$HOME/Documents";
      download = "$HOME/Téléchargements";
      music = "$HOME/Musique";
      pictures = "$HOME/Photos";
      publicShare = "$HOME/Public";
      templates = "$HOME/Templates";
      videos = "$HOME/Vidéos";
    };
  };
}
