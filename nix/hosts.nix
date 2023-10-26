let
  hasSuffix = suffix: content:
    let
      inherit (builtins) stringLength substring;
      lenContent = stringLength content;
      lenSuffix = stringLength suffix;
    in
    lenContent >= lenSuffix
    && substring (lenContent - lenSuffix) lenContent content == suffix
  ;

  mkHost =
    { type
    , hostPlatform
    , address ? null
    , pubkey ? null
    , homeDirectory ? null
    , remoteBuild ? true
    , large ? false
    }:
    if type == "nixos" then
      assert address != null && pubkey != null;
      assert (hasSuffix "linux" hostPlatform);
      {
        inherit type hostPlatform address pubkey remoteBuild large;
      }
    else if type == "darwin" then
      assert pubkey != null;
      assert (hasSuffix "darwin" hostPlatform);
      {
        inherit type hostPlatform pubkey large;
      }
    else if type == "home-manager" then
      assert homeDirectory != null;
      {
        inherit type hostPlatform homeDirectory large;
      }
    else if type == "nix-on-droid" then
      assert address != null && pubkey != null;
      assert (hasSuffix "linux" hostPlatform);
      {
        inherit type hostPlatform address pubkey remoteBuild large;
      }
    else throw "unknown host type '${type}'";
in
{
  desktop = mkHost {
    type = "nixos";
    hostPlatform = "x86_64-linux";
    address = "desktop";
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGUlX5oPlf1HHr7TLaOTeN0NldHzgeWGHZF1ntVpWwIm";
  };
  laptop = mkHost {
    type = "nixos";
    hostPlatform = "x86_64-linux";
    address = "laptop";
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEIs8Ait2j8oYQB0cWPLZIw9vObzuWxo4E00awCNw2rZ root@nixos";
  };
  work = mkHost {
    type = "nixos";
    hostPlatform = "x86_64-linux";
    address = "bbigras-work";
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKEfhY66gBDU0xgjaQgm9V991wuxI/R3bm3Yt6Kdv9Au root@nixos";
  };
  # pixel6 = mkHost {
  #   type = "nix-on-droid";
  #   hostPlatform = "aarch64-linux";
  #   address = "pixel6";
  #   pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHGTEcpBZpaWTaPBXsBCvO2sNE6UU9Z7In98htaEofYa nix-on-droid@localhost";
  #   remoteBuild = false;
  # };
}
