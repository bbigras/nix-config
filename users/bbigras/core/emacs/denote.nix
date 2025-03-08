{
  ...
}:

{
  programs.emacs.init = {
    usePackage = {
      denote = {
        enable = true;
        bind = {
          "C-c n l" = "denote-link-or-create";
          "C-c n o" = "denote-open-or-create";
          "C-c n r" = "denote-rename-file-using-front-matter";

        };
        custom = {
          "denote-directory" = ''"~/Documents/notes"'';
          "denote-rename-buffer-format" = ''"Denote: %t (%k)"'';
          # "denote-infer-keywords" = "nil";
          # "denote-known-keywords" = ''
          #   '("pra" "prb" "prc"
          #     "ply" "plm" "plw"
          #     "kt" "ke" "kp" "kl" "ka" "kap"
          #     "kcp" "kca" "kcc"
          #     "kra" "krb" "krv"
          #     "rn")
          # '';
        };

        config = ''
          (require 'denote-rename-buffer)
           (require 'denote-org-extras)

           ;; Rename buffers with the note name
           (denote-rename-buffer-mode 1)

           ;; Buttonize all denote links in text buffers
           (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
        '';

        # ;; Automatically rename Denote buffers when opening them so that
        #  ;; instead of their long file name they have, for example, a literal
        #  ;; "[D]" followed by the file's title.  Read the doc string of
        #  ;; `denote-rename-buffer-format' for how to modify this.
        #  (denote-rename-buffer-mode 1)

      };
      denote-explore.enable = true;
      consult-denote = {
        enable = true;
        after = [
          "consult"
          "denote"
        ];
        config = ''
          (consult-denote-mode 1)
        '';
      };
    };
  };
}
