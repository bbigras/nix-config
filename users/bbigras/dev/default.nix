{ pkgs, ... }: {
  imports = [
    # pkgs.nur.repos.rycee.hmModules.emacs-init
    ./emacs.nix
  ];

  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];

  home.file.gdbinit = {
    target = ".gdbinit";
    text = ''
      set auto-load safe-path /
    '';
  };

  programs.emacs = {
    enable = true;
    init.enable = true;
    package = pkgs.emacsPgtkGcc;
  };
}
