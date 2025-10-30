self: super: {
  emacs.pkgs = super.emacs.pkgs // {
    gptel = super.emacs.pkgs.trivialBuild {
      pname = "gptel";
      version = "git";
      src = self.__inputs.gptel;
      packageRequires = super.emacs.pkgs.gptel.packageRequires;
    };
  };
}
