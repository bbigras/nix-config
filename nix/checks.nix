{ self
, pre-commit-hooks
, ...
}:

system:

with self.legacyPackages.${system};

{
  pre-commit-check = pre-commit-hooks.lib.${system}.run
    {
      src = gitignoreSource ../.;
      hooks = {
        nixpkgs-fmt = {
          enable = true;
          excludes = [ "hardware/*" ];
        };
        nix-linter = {
          enable = true;
          excludes = [ "hardware/*" ];
        };
        actionlint = {
          enable = true;
          files = "^.github/workflows/";
          types = [ "yaml" ];
          entry =
            let
              actionlintBin = "${actionlint}/bin/actionlint";
              pyflakesBin = "${python3Packages.pyflakes}/bin/pyflakes";
              shellcheckBin = "${shellcheck}/bin/shellcheck";
            in
            "${actionlintBin} -pyflakes ${pyflakesBin} -shellcheck ${shellcheckBin}";
        };
      };
      settings.nix-linter.checks = [
        "DIYInherit"
        "EmptyInherit"
        "EmptyLet"
        "EtaReduce"
        "LetInInheritRecset"
        "ListLiteralConcat"
        "NegateAtom"
        "SequentialLet"
        "SetLiteralUpdate"
        "UnfortunateArgName"
        "UnneededRec"
        "UnusedArg"
        "UnusedLetBind"
        "UpdateEmptySet"
        "BetaReduction"
        "EmptyVariadicParamSet"
        "UnneededAntiquote"
        "no-FreeLetInFunc"
        "no-AlphabeticalArgs"
        "no-AlphabeticalBindings"
      ];
    };
} // (deploy-rs.lib.deployChecks self.deploy)
