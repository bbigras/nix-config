{ lib, pkgs, ... }:

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
    packages = with pkgs; [
      git-lfs
      (lib.hiPrio nixpkgs-review)
      nix-update
      tmate
      upterm
      watchman
    ];
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
    jujutsu = {
      enable = true;
      settings = {
        core.fsmonitor = "watchman";
        ui.allow-filesets = true;

        # https://github.com/martinvonz/jj/pull/3657
        ui.pager = {
          command = ["less" "-FRX"];
          env = { LESSCHARSET = "utf-8"; };
        };

        user = {
          name = "Bruno Bigras";
          email = "bigras.bruno@gmail.com";
        };
        template-aliases = {
          "format_short_signature(signature)" = "signature.username()";
        };
      };
    };
  };
}
