{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fd
  ];

  programs.emacs.init = {
    usePackage = {
      vulpea = {
        enable = true;
        config = ''
          ;; Parse and write in a background process: saving a note - even a
          ;; 100MB one - blocks Emacs for about a millisecond.
          (setq vulpea-db-async-extraction 'full)

          ;; Skip org-mode-hook during indexing (fine unless you rely on
          ;; hook-based per-file setup).
          (setq vulpea-db-parse-method 'single-temp-buffer)

          ;; Index only [[bracketed]] links. id: links are always bracketed,
          ;; so the note graph is unaffected; skips the expensive scan for
          ;; plain https://... links in prose.
          (setq vulpea-db-index-plain-links nil)

          (setq vulpea-db-sync-directories (list "~/dev/org-mode/"))

          (vulpea-db-autosync-mode +1)
        '';
        extraPackages = [
          pkgs.fswatch
        ];
      };

      vulpea-ui = {
        enable = true;
        bind = {
          "C-c v s" = "vulpea-ui-sidebar-toggle";
        };
        config = ''
          (setq vulpea-ui-link-types '("id" "denote"))
        '';
      };

      vulpea-journal = {
        enable = true;
        after = [
          "vulpea"
          "vulpea-ui"
        ];
        bind = {
          "C-c j" = "vulpea-journal";
        };
        config = ''
          (vulpea-journal-setup)
        '';
        #   (setq vulpea-journal-directory "~/dev/org-mode/notes/journal/")
        #   (setq vulpea-journal-file-format "%Y-%m-%d.org")
        # '';
      };

      consult-vulpea = {
        enable = true;
        after = [
          "consult"
          "vulpea"
        ];
        config = ''
          (consult-vulpea-mode 1)
        '';
        # bind = {
        #   "C-c v f" = "consult-vulpea-find-note";
        #   "C-c v g" = "consult-vulpea-grep-notes";
        #   "C-c v i" = "consult-vulpea-insert-link";
        # };
      };

      vulpea-para = {
        enable = true;
        package = _epkgs: pkgs.emacsPackages.vulpea-para;
        config = ''
          (vulpea-para-setup-defaults)

          (setq org-capture-templates
                '(("t" "Task" entry
                   (file+headline "inbox.org" "Tasks")
                   (function vulpea-para-capture-task-template))
          ("p" "PARA project" entry #'vulpea-para-capture-project-target #'vulpea-para-capture-project-template)
          ("m" "PARA meeting" entry #'vulpea-para-capture-meeting-target #'vulpea-para-capture-meeting-template :clock-in t :clock-resume t)
          ))
        '';
      };
    };
  };
}
