self: super: {
  emacsPackages = super.emacsPackages // {
    org-tufte = super.emacsPackages.trivialBuild {
      pname = "org-tufte";
      version = "git";
      src = self.__inputs.org-tufte;
      packageRequires = with super.emacsPackages; [
        s
      ];
    };
  };
}
