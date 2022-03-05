{ self, ... }:

system:

with self.nixpkgs.${system};

let
  inherit (lib) concatMapStrings escapeShellArg mapAttrs mapAttrsToList;

  hostDrvs = mapAttrs (_: host: if builtins.hasAttr "system" host.profiles then host.profiles.system.path else host.profiles.nix-on-droid.path) self.deploy.nodes;

  hostEntries = mapAttrsToList
    (name: path: { inherit name path; })
    hostDrvs;

  mkHostLinks = concatMapStrings
    (x:
      ''
        mkdir -p "$(dirname ${escapeShellArg x.name})"
        ln -s ${escapeShellArg x.path} ${escapeShellArg x.name}
      '')
    hostEntries;
in
runCommand "hosts"
  ({ preferLocalBuild = true; } // hostDrvs)
  ''
    mkdir -p $out
    cd $out
    ${mkHostLinks}
  ''
