{ pkgs, config, ... }:

let
  pcfg = config.programs.emacs.init.usePackage;
in
{
  programs.emacs = {
    init = {
      prelude = ''
        (setq org-directory "~/dev/org-mode")
      '';

      usePackage = {
        org-transclusion = {
          enable = true;
          #           init = ''
          # (define-key global-map (kbd "<f12>") #'org-transclusion-add)
          # (define-key global-map (kbd "C-n t") #'org-transclusion-mode)
          #           '';
        };

        org-rich-yank = {
          enable = true;
          bindLocal = {
            org-mode-map = {
              "C-M-y" = "org-rich-yank";
            };
          };
          config = ''
            (defun my-org-rich-yank-format-paste (language contents link)
              "Based on `org-rich-yank--format-paste-default'."
              (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
                      language
                      (org-rich-yank--trim-nl contents)
                      link))
            (customize-set-variable 'org-rich-yank-format-paste #'my-org-rich-yank-format-paste)
          '';
        };

        consult-org-roam = {
          enable = true;
          after = [ "org-roam" ];
          # init = ''
          #   (consult-org-roam-mode 1)
          # '';
          config = ''
            (consult-customize
             consult-org-roam-forward-links
             :preview-key (kbd "M-."))

            (setq consult-org-roam-grep-func #'consult-ripgrep)
            (setq consult-org-roam-buffer-narrow-key ?r)
            (setq consult-org-roam-buffer-after-buffers t)
          '';
          bind = {
            "C-c n e" = "consult-org-roam-file-find";
            "C-c n b" = "consult-org-roam-backlinks";
            "C-c n l" = "consult-org-roam-forward-links";
            "C-c n r" = "consult-org-roam-search";
          };
        };

        org-roam = {
          enable = true;
          hook = [ "(after-init . org-roam-db-autosync-mode)" ];
          config = ''
            (setq org-roam-directory "~/dev/org-mode/notes")
          '';
          bind = {
            "C-c n f" = "org-roam-node-find";
            "C-c n r" = "org-roam-node-random";
            "C-c n t" = "org-roam-dailies-find-today";
          };

          bindLocal = {
            org-mode-map = {
              "C-c n i" = "org-roam-node-insert";
              "C-c n o" = "org-id-get-create";
              "C-c n t" = "org-roam-tag-add";
              "C-c n a" = "org-roam-alias-add";
              "C-c n l" = "org-roam-buffer-toggle";
            };
          };
        };

        org-roam-ui = {
          enable = true;
          config = ''
            (setq org-roam-ui-sync-theme t
                      org-roam-ui-follow t
                      org-roam-ui-update-on-save t
                      org-roam-ui-open-on-start t)
          '';
        };

        org-roam-dailies = {
          enable = true;
          after = [ "org-roam" ];
          config = ''
            (setq org-roam-dailies-directory "journal/")

            (setq org-roam-dailies-capture-templates
                  '(("d" "default" entry
                     "* %?"
                     :target (file+head "%<%Y-%m-%d>.org"
                                        "#+title: %<%Y-%m-%d>\n"))))
          '';
        };

        org-download = {
          enable = true;
          extraPackages = [ pkgs.xclip ];
        };


        # Highlight and annotate text file and websites
        org-remark.enable = true;

        org-tempo = {
          enable = true;
          package = "org";
        };

        org-mime.enable = true;
        org-web-tools.enable = true;
        ox-clip.enable = true;
        ox-tufte.enable = true;

        ox-hugo = {
          enable = true;
          after = [ "ox" ];
        };

        org-modern = {
          enable = true;
          config = ''
            (global-org-modern-mode)
          '';
        };

        org-hyperscheduler = {
          enable = true;
        };


        # for org-mode
        htmlize.enable = true;


        org-sticky-header = {
          enable = true;
          hook = [
            "(org-mode . org-sticky-header-mode)"
          ];
        };

        org = {
          enable = true;
          bind = {
            "C-c c" = "org-capture";
            "C-c a" = "org-agenda";
            "C-c l" = "org-store-link";
            "C-c b" = "org-switchb";
          };
          bindLocal = {
            org-mode-map = {
              "C-x n s" = "org-toggle-narrow-to-subtree";
            };
          };
          hook = [
            ''
              (org-mode
               . (lambda ()
                   (add-hook 'completion-at-point-functions
                             'pcomplete-completions-at-point nil t)))
            ''
          ];
          config = ''
            ;; Some general stuff.
            (setq org-reverse-note-order t
                  org-use-fast-todo-selection t
                  org-adapt-indentation nil
                  org-hide-emphasis-markers t)

            ;;(setq org-tag-alist rah-org-tag-alist)

            ;; Add some todo keywords.
            (setq org-todo-keywords
                  '((sequence "TODO(t)"
                              "NEXT"
                              "STARTED(s!)"
                              "WAITING(w@/!)"
                              "DELEGATED(@!)"
                              "|"
                              "DONE(d!)"
                              "CANCELED(c@!)")))

            ;; Setup org capture.
            ;; (setq org-default-notes-file (rah-org-file "capture"))
            (setq org-default-notes-file (concat org-directory "/inbox.org"))

            ;; Active Org-babel languages
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((plantuml . t)
                                           (http . t)
                                           (dot . t)
                                           (restclient . t)
                                           (R . t)
                                           (sql . t)
                                           (shell . t)))

            ;; Unfortunately org-mode tends to take over keybindings that
            ;; start with C-c.
            (unbind-key "C-c SPC" org-mode-map)
            (unbind-key "C-c w" org-mode-map)

            (setq org-image-actual-width 400)
            (setq org-extend-today-until 4)
            (setq org-export-backends (quote (ascii html icalendar latex md odt)))

            (defun add-newline-at-end-if-none ()
              "Add a newline at the end of the buffer if there isn't any."
              (save-excursion
                (save-restriction
                  (goto-char (1- (point-max)))
                  (if (not (looking-at "\n"))
                  (progn
                    (goto-char (point-max))
                    (insert "\n"))))))

            ;; https://old.reddit.com/r/orgmode/comments/hg8qik/weird_joined_lines_bug/fw73kml/
            (add-hook 'org-capture-prepare-finalize-hook 'add-newline-at-end-if-none)
          '';
        };

        # org-super-agenda = {
        #   enable = true;
        #   after = [ "org" ];
        #   config = ''
        #     (org-super-agenda-mode t)
        #     (setq org-super-agenda-groups '((:auto-parent t)))
        #   '';
        # };

        org-habit = {
          enable = true;
          after = [ "org" ];
          # defer = true;
          config = ''
            ;; for using with Orgzly
            (setq org-log-into-drawer "LOGBOOK")
          '';
        };

        org-agenda = {
          enable = true;
          after = [ "org" ];
          defer = true;
          config = ''
            ;; Set up agenda view.
            ;; org-agenda-files (rah-all-org-files)
            (setq org-agenda-files
                  (list
                   (concat org-directory "/inbox.org")
                   (concat org-directory "/luxor.org")
                   (concat org-directory "/gtd.org")))
            (setq org-agenda-span 5
                  org-deadline-warning-days 14
                  org-agenda-show-all-dates t
                  org-agenda-skip-deadline-if-done t
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-start-on-weekday nil
                  org-agenda-todo-ignore-scheduled 'future
                  org-agenda-todo-ignore-deadlines 7)

            (setq org-agenda-custom-commands
                  '(("p" tags "PROJECT-MAYBE-DONE/-DONE-CANCELED" nil)
                    ("m" tags "PROJECT&MAYBE" nil)
                    ("a" "My agenda"
                     ((org-agenda-list)
                      (org-agenda-list-stuck-projects)          ;; (1)
                      (tags "PROJECT-MAYBE-DONE/-DONE-CANCELED")))
                    ;; ... put your other custom commands here
                   ))

            (setq org-stuck-projects
                  '("+PROJECT-MAYBE-DONE/-DONE-CANCELED" ("TODO" "STARTED") nil "\\<IGNORE\\>"))
          '';
          # (setq org-tags-exclude-from-inheritance (list "PROJECT"))
        };

        ol-notmuch = {
          enable = pcfg.org.enable && pcfg.notmuch.enable;
          after = [ "notmuch" "org" ];
        };

        ob-restclient = {
          enable = true;
          mode = [ ''("\\.http\\'" . restclient-mode)'' ];
        };

        ob-http = {
          enable = true;
          after = [ "org" ];
          defer = true;
        };

        ob-plantuml = {
          enable = true;
          after = [ "org" ];
          defer = true;
        };


        org-table = {
          enable = true;
          after = [ "org" ];
          command = [ "orgtbl-to-generic" ];
          hook = [
            # For orgtbl mode, add a radio table translator function for
            # taking a table to a psql internal variable.
            ''
              (orgtbl-mode
               . (lambda ()
                   (defun rah-orgtbl-to-psqlvar (table params)
                     "Converts an org table to an SQL list inside a psql internal variable"
                     (let* ((params2
                             (list
                              :tstart (concat "\\set " (plist-get params :var-name) " '(")
                              :tend ")'"
                              :lstart "("
                              :lend "),"
                              :sep ","
                              :hline ""))
                            (res (orgtbl-to-generic table (org-combine-plists params2 params))))
                       (replace-regexp-in-string ",)'$"
                                                 ")'"
                                                 (replace-regexp-in-string "\n" "" res))))))
            ''
          ];
          config = ''
            (unbind-key "C-c SPC" orgtbl-mode-map)
            (unbind-key "C-c w" orgtbl-mode-map)
          '';
          extraConfig = ''
            :functions org-combine-plists
          '';
        };

        org-capture = {
          enable = true;
          after = [ "org" ];
          config = ''
            (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                           (file+headline ,(concat org-directory "/inbox.org") "Tasks")
                                           "* TODO %i%?")
                                          ("T" "Tickler" entry
                                           (file+headline ,(concat org-directory "/tickler.org") "Tickler")
                                           "* %i%? \n %U")))
          '';
        };

        org-clock = {
          enable = true;
          after = [ "org" ];
          config = ''
            (setq org-clock-rounding-minutes 5
                  org-clock-out-remove-zero-time-clocks t)
          '';
        };

        org-duration = {
          enable = true;
          after = [ "org" ];
          config = ''
            ;; I always want clock tables and such to be in hours, not days.
            (setq org-duration-format (quote h:mm))
          '';
        };

        org-refile = {
          enable = true;
          after = [ "org" ];
          config = ''
            ;; Refiling should include not only the current org buffer but
            ;; also the standard org files. Further, set up the refiling to
            ;; be convenient with IDO. Follows norang's setup quite closely.
            (setq org-refile-targets '((nil :maxlevel . 2)
                                       (org-agenda-files :maxlevel . 2))
                  org-refile-use-outline-path t
                  org-outline-path-complete-in-steps nil
                  org-refile-allow-creating-parent-nodes 'confirm)
          '';
        };

        org-superstar = {
          enable = true;
          hook = [ "(org-mode . org-superstar-mode)" ];
        };

        org-edna = {
          enable = true;
          defer = 1;
          config = "(org-edna-mode)";
        };

        # custom
        # (org-image-actual-width nil)
        org-tree-slide = {
          enable = true;
          command = [ "org-tree-slide-mode" ];
          # hook = [
          #   "(org-tree-slide-play . efs/presentation-setup)"
          #   "(org-tree-slide-stop . efs/presentation-end)"
          # ];
          init = ''
            (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
            (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)

            (with-eval-after-load "org-tree-slide"
              (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
              (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
              )
          '';
        };
      };
    };
  };
}
