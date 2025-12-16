{
  programs.emacs = {
    init = {
      usePackage = {
        magit = {
          enable = true;
          bind = {
            "C-x g" = "magit-status";
          };
          config = ''
            (add-to-list 'git-commit-style-convention-checks
                         'overlong-summary-line)
            (setq magit-repolist-columns
                  '(("Name"    25 magit-repolist-column-ident ())
                    ("Version" 25 magit-repolist-column-version ())
                    ("D"        1 magit-repolist-column-dirty ())
                    ("⇣"      3 magit-repolist-column-unpulled-from-upstream
                     ((:right-align t)
                      (:help-echo "Upstream changes not in branch")))
                    ("⇡"        3 magit-repolist-column-unpushed-to-upstream
                     ((:right-align t)
                      (:help-echo "Local changes not in upstream")))
                    ("Path"    99 magit-repolist-column-path ())))
          '';
        };

        magit-todos = {
          enable = true;
          defer = 1;
        };

        forge = {
          enable = true;
          after = [ "magit" ];
        };

        git-messenger = {
          enable = true;
          bind = {
            "C-x v p" = "git-messenger:popup-message";
          };
        };
      };
    };
  };
}
