{ pkgs, ... }:
let
  helloWorld = pkgs.writeScriptBin "helloWorld" ''
        #!${pkgs.stdenv.shell}

    set -euxo pipefail

    mkdir -p /home/bbigras/
    touch /tmp/bleh.rs

    mkdir -p /home/bbigras/Dropbox/emacs
    touch /home/bbigras/Dropbox/emacs/custom-server.el

    sudo -i -u bbigras /home/bbigras/.nix-profile/bin/emacs -l /home/bbigras/.emacs.d/early-init.el --batch -l /home/bbigras/.emacs.d/init.el --eval "(progn (setq lsp-restart 'ignore) (find-file \"/tmp/bleh.rs\") )"

    sudo -i -u bbigras /home/bbigras/.nix-profile/bin/emacs -l /home/bbigras/.emacs.d/early-init.el --batch -l /home/bbigras/.emacs.d/init.el --eval "(progn (setq lsp-restart 'ignore) (find-file \"/tmp/bleh.rs\") )" 2>&1 | (! grep -q Error)

  '';
in
{
  name = "nix-matrix-yggdrasil-test";
  nodes.server = {
    imports = [
      (import (import ./nix).home-manager)
    ];

    nixpkgs = {
      overlays = [
        (import (import ./nix).emacs-overlay)
      ];
    };

    environment.systemPackages = with pkgs; [ rust-analyzer helloWorld ];

    users.users.bbigras = {
      createHome = true;
      isNormalUser = true;
    };

    home-manager.useGlobalPkgs = true;
    home-manager.users.bbigras = { pkgs, ... }: {
      # imports = [ ./users/bbigras/dev/emacs.nix ];

      programs.emacs = {
        enable = true;
        init.enable = true;
        package = pkgs.emacsGit;
      };
    };
  };

  testScript = ''
    server.start()
    server.wait_for_unit("multi-user.target")
    server.succeed("helloWorld")
  '';
}
