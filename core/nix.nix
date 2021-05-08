{ pkgs, ... }: {
  nix = {
    allowedUsers = [ "@wheel" ];
    daemonIONiceLevel = 5;
    daemonNiceLevel = 10;
    # nrBuildUsers = config.nix.maxJobs * 2;
    # optimise = {
    #   automatic = true;
    #   dates = [ "01:10" "12:10" ];
    # };
    trustedUsers = [ "root" "@wheel" ];

    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations ca-references
      keep-outputs = true
      keep-derivations = true
    '';

    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://nix-matrix-pinecone.cachix.org"
      "https://eigenvalue.cachix.org" # for crate2nix
      "https://bbigras-nix-config.cachix.org"
      "https://nixiosk.cachix.org"
      "https://mjlbach.cachix.org"
      "https://pre-commit-hooks.cachix.org"
      "https://bbigras-dev.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-matrix-pinecone.cachix.org-1:GR+/Y4n1H2x7VaA+Q982EeZsRXPfdEV21Lp7/+YdrjI="
      "eigenvalue.cachix.org-1:ykerQDDa55PGxU25CETy9wF6uVDpadGGXYrFNJA3TUs="
      "bbigras-nix-config.cachix.org-1:aXL6Q9Oi0jyF79YAKRu17iVNk9HY0p23OZX7FA8ulhU="
      "nixiosk.cachix.org-1:pyzRzjCUhw0r+moXnSklZwwI/gFk+Z+A2ofmEhOf7Sc="
      "mjlbach.cachix.org-1:dR0V90mvaPbXuYria5mXvnDtFibKYqYc2gtl9MWSkqI="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "bbigras-dev.cachix.org-1:7JrfmXK/0cXRCzguretr0wlWtY2zp/mkp/HwMwIJzCw="
    ];
  };
}
