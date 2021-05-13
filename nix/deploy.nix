{ deploy-rs
, ...
}@inputs:
let
  inherit (builtins) elemAt mapAttrs;

  mkHost = name: system: import ./mk-host.nix { inherit inputs name system; };

  mkPath = name: system: deploy-rs.lib.${system}.activate.nixos (mkHost name system);
  pixel2 = (inputs.nix-on-droid.lib.aarch64-linux.nix-on-droid { config = ../hosts/pixel2; }).activationPackage;
in
{
  deploy = {
    autoRollback = true;
    magicRollback = true;
    user = "root";

    nodes = (mapAttrs
      (n: v: {
        inherit (v) hostname;
        profiles.system.path = mkPath n v.system;
      })
      (import ./hosts.nix)) //
    {
      pixel2 = {
        hostname = "pixel2";

        # to prevent using sudo
        sshUser = "nix-on-droid";
        user = "nix-on-droid";

        profiles.nix-on-droid.path = deploy-rs.lib.aarch64-linux.activate.custom
          pixel2
          (pixel2 + "/activate");
      };
    };
  };
}
