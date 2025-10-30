self: super: {
  emacs.pkgs = super.emacs.pkgs // {
    el-easydraw = super.emacs.pkgs.trivialBuild {
      pname = "el-easydraw";
      version = "git";
      src = self.__inputs.el-easydraw;
    };
  };
}
