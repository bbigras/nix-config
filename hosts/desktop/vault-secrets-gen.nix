{ pkgs }:

with pkgs;
buildGoModule rec {
  pname = "vault-secrets-gen";
  version = "0.1.1";


  src = pkgs.fetchFromGitHub {
    owner = "sethvargo";
    repo = pname;
    rev = "v${version}";
    sha256 = "0hhwhnk637ynwkf1gakhpssljrbfabaw7xgz03rlmjhjam4n31hr";
  };

  vendorSha256 = "0ii3daw629wcma66q4x3ys3y8xmm70bnxign6s48fw9gshjjyk1a";

  # subPackages = [ "vault-secrets-gen" ];
  subPackages = [ "." ];

  # preBuild = ''
  #   export buildFlagsArray=(
  #     -ldflags="$(make env-LDFLAGS)"
  #   )
  # '';

  # src = nix-gitignore.gitignoreSource [ ] ./.;
  nativeBuildInputs = [ which git ];

  # preBuild = ''
    # export buildFlagsArray=(
      # -ldflags="$(make env-LDFLAGS)"
    # )
  # '';
  dontStrip = true;
  # vendorSha256 = "16sdaqj02173xcksh8ysmj2s0lz1jz53ddaqn0nxq7qmpcyajfnc";
}
