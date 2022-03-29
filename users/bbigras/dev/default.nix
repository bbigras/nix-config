{ pkgs, ... }:

let
  direnv_psql_cfg = ../core/direnv-psql.cfg;
in
{
  home = {
    extraOutputsToInstall = [ "doc" "devdoc" ];
    file.gdbinit = {
      target = ".gdbinit";
      text = ''
        set auto-load safe-path /
      '';
    };
    packages = with pkgs; [ git-extras git-lfs ];
  };

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      stdlib = ''
        : ''${XDG_CACHE_HOME:=$HOME/.cache}
        declare -A direnv_layout_dirs
        direnv_layout_dir() {
            echo "''${direnv_layout_dirs[$PWD]:=$(
                echo -n "$XDG_CACHE_HOME"/direnv/layouts/
                echo -n "$PWD" | shasum | cut -d ' ' -f 1
            )}"
        }

        source ${direnv_psql_cfg}
      '';
    };

    gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };

    nix-index.enable = true;

    # zsh.shellAliases.gco = lib.mkForce "git cz";
  };
}
