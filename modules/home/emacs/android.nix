{
  programs.emacs = {
    init = {
      usePackage = {
        kotlin-ts-mode = {
          enable = true;
          defer = true;
        };
        lsp-dart = {
          enable = true;
          defer = true;
          command = [ "lsp-dart-setup" ];
        };
        flutter = {
          enable = true;
          defer = true;
          command = [
            "flutter-run"
            "flutter-hot-reload"
          ];
        };
      };
    };
  };
}
