self: super: {
  emacsPackages = super.emacsPackages // {
    combobulate = (super.emacsPackagesFor super.emacs-git).trivialBuild {
      pname = "combobulate";
      version = "git";
      src = self.__inputs.combobulate;
    };
  };
}
