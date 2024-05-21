final: prev:
{
  veilid = final.rustPlatform.buildRustPackage rec {
    pname = "veilid";
    version = "unstable-2024-05-21";

    src = final.fetchFromGitLab {
      owner = "veilid";
      repo = "veilid";
      rev = "8e8ee06fe9103674e8fb2ce8464e4a296d742cbb";
      hash = "sha256-ABity7TBEg2tyiUwGJFTVW9MeQRdCWYE3JQR//Mwwq0=";
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
