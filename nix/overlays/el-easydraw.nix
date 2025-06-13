self: super: {
  emacsPackages = super.emacsPackages // {
    el-easydraw = super.emacsPackages.trivialBuild {
      pname = "el-easydraw";
      version = "git";
      src = self.__inputs.el-easydraw;
    };
  };
}
