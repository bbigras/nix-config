{ self
, pre-commit-hooks
, ...
}:

system:

with self.nixpkgs.${system};

{
  pre-commit-check = pre-commit-hooks.lib.${system}.run
    {
      src = lib.cleanSource ../.;
      hooks = {
        nix-linter = {
          enable = true;
          excludes = [ "hardware-configuration.*.nix" ];
        };
        nixpkgs-fmt.enable = true;
        statix.enable = true;
        stylua = {
          enable = false;
          types = [ "file" "lua" ];
          entry = "${stylua}/bin/stylua";
        };
        luacheck = {
          enable = false;
          types = [ "file" "lua" ];
          entry = "${luajitPackages.luacheck}/bin/luacheck --std luajit --globals vim -- ";
        };
        actionlint = {
          enable = true;
          files = "^.github/workflows/";
          types = [ "yaml" ];
          entry = "${actionlint}/bin/actionlint";
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
