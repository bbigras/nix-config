# NixOS modules aggregator
# This is the main entry point for NixOS configurations
{ flake, pkgs, ... }:
let
  inherit (flake) inputs self;
  nurNoPkgs = import inputs.nur {
    pkgs = null;
    nurpkgs = pkgs;
  };
in
{
  imports = [
    # External input modules (shared across all NixOS hosts)
    inputs.agenix.nixosModules.default
    inputs.catppuccin.nixosModules.catppuccin
    inputs.disko.nixosModules.default
    inputs.ncro.nixosModules.ncro
    inputs.home-manager.nixosModules.home-manager
    inputs.impermanence.nixosModules.impermanence
    inputs.lanzaboote.nixosModules.lanzaboote
    inputs.niks3.nixosModules.niks3-auto-upload
    inputs.nix-index-database.nixosModules.nix-index
    inputs.nixos-facter-modules.nixosModules.facter
  ]
  ++ (with self.nixosModules; [
    agenix-rekey
    core
    # tailscale-address
  ]);

  catppuccin = {
    enable = true;
    autoEnable = true;
    flavor = "mocha";
    accent = "blue";
    cursors.enable = true;
  };

  programs.nix-ld.enable = true;

  # Home-manager integration
  # Note: stylix.homeModules.stylix is auto-imported via stylix.homeManagerIntegration
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = { inherit flake; };
    sharedModules = [
      inputs.nix-index-database.homeModules.nix-index
      inputs.catppuccin.homeModules.catppuccin
      inputs.niri.homeModules.niri
      inputs.noctalia.homeModules.default
      nurNoPkgs.repos.rycee.hmModules.emacs-init
      self.homeModules.default
    ];
  };
}
