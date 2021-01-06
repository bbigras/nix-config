{
  description = "bbigras's NixOS config";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixos-hardware.url = "nixos-hardware";
    nur.url = "nur";

    nixpkgs-chrony = {
      type = "github";
      owner = "ctem";
      repo = "nixpkgs";
      ref = "feature/chrony";
    };

    flake-utils.url = "github:numtide/flake-utils";

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    impermanence.url = "github:nix-community/impermanence";

    nixpkgs-cdda-mods = {
      url = "github:mnacamura/nixpkgs-cdda-mods";
      flake = false;
    };

    explain-pause-mode = {
      url = "github:lastquestion/explain-pause-mode";
      flake = false;
    };
  };

  # FIXME: I can't η-reduce this for some reason
  outputs = args: import ./nix/outputs.nix args;
}
