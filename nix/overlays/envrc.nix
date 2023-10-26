self: super: {
  emacsPackages = super.emacsPackages // {
    envrc = super.emacsPackages.trivialBuild {
      pname = "envrc";
      version = "git";
      src = self.__inputs.envrc;
      packageRequires = with super.emacsPackages; [
        inheritenv
      ];
    };
  };
}
