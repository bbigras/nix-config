final: prev:
{
  veilid = final.rustPlatform.buildRustPackage rec {
    pname = "veilid";
    version = "0.3.3";

    src = final.fetchFromGitLab {
      owner = "veilid";
      repo = "veilid";
      rev = "v${version}";
      sha256 = "sha256-Gm65fvLImbsAU8kMYQv5VFEjkBQnhBFDqwheddRbtU8=";
    };

    cargoLock = {
      lockFile = ../veilid-Cargo.lock;
      outputHashes = {
        "ansi-parser-0.9.1" = "sha256-Vdjt8QDstrfxYfklZ5vYPGhVNG1BVh4cpKGwvvsHlS4=";
        "cursive-0.20.0" = "sha256-EGKO7JVN9hIqADKKC3mUHHOCSxMjPoXzYBZujzdgk3E=";
        "cursive_buffered_backend-0.6.1" = "sha256-+sTJnp570HupwaJxV2x+oKyLwNmqQ4HqOH2P1s9Hhw8=";
        "cursive_table_view-0.14.0" = "sha256-haos82qtobMsFCP3sNRu5u1mki4bsjrV+eqFxUGIHqk=";
      };
    };

    nativeBuildInputs = with final; [
      capnproto
      protobuf
    ];

    # buildInputs = lib.optionals stdenv.isDarwin [ AppKit Security ];

    cargoBuildFlags = [
      "--workspace"
    ];

    doCheck = false;

    outputs = [ "out" "lib" "dev" ];

    postInstall = ''
      moveToOutput "lib" "$lib"
    '';
  };
}
