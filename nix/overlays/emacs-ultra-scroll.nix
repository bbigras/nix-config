self: super: {
  emacsPackages = super.emacsPackages // {
    ultra-scroll = super.emacsPackages.trivialBuild {
      pname = "ultra-scroll";
      version = "git";
      src = self.__inputs.emacs-ultra-scroll;
    };
  };
}
