self: super: {
  emacsPackages = super.emacsPackages // {
    combobulate = super.emacsPackages.trivialBuild {
      pname = "combobulate";
      version = "git";
      src = self.__inputs.combobulate;
    };
  };
}
