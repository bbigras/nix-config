{ pkgs, ... }:

let
  myLatex = with pkgs; (texlive.combine { inherit (texlive) scheme-small wrapfig ulem capt-of; });
in
{
  home.packages = with pkgs; [
  ] ++ lib.optionals (stdenv.hostPlatform.system == "x86_64-linux") [
    myLatex
  ];
}
