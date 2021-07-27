{ lib
, buildGoModule
, fetchFromGitHub
}:
buildGoModule {
  pname = "yggmail";
  version = "unstable-2021-07-10";

  src = fetchFromGitHub {
    owner = "neilalexander";
    repo = "yggmail";
    rev = "e0b3f6089d61415b1fca505864562f9b5dee4bc1";
    sha256 = "sha256:0xb9596xa9356f1xv18ly4p2a436ls86rp6cx2nf46j04zzchhf7";
  };

  vendorSha256 = "sha256:1q8s9ixanxsik32qdyzlqhs8il98755bw1zj8qa0z47i339bfnad";

  meta = with lib; {
    description = "Yggdrasil based end to end email";
    homepage = "https://github.com/neilalexander/yggmail";
    license = licenses.lgpl3;
    #maintainers = teams.determinatesystems.members;
  };
}
