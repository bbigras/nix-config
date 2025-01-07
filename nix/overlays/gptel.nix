self: super: {
  emacsPackages = super.emacsPackages // {
    gptel = super.emacsPackages.trivialBuild {
      pname = "gptel";
      version = "git";
      src = self.__inputs.gptel;
      packageRequires = super.emacsPackages.gptel.packageRequires;
    };
  };
}
