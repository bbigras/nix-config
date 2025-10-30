self: super: {
  emacs.pkgs = super.emacs.pkgs // {
    ultra-scroll = super.emacs.pkgs.trivialBuild {
      pname = "ultra-scroll";
      version = "git";
      src = self.__inputs.emacs-ultra-scroll;
    };
  };
}
