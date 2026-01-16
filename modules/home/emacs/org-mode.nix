{ pkgs, ... }:

{
  programs.emacs = {
    init = {
      prelude = ''
        (setq org-directory "~/dev/org-mode")
      '';

      usePackage = {
        # org-transclusion = {
        #   enable = true;
        #   #           init = ''
        #   # (define-key global-map (kbd "<f12>") #'org-transclusion-add)
        #   # (define-key global-map (kbd "C-n t") #'org-transclusion-mode)
        #   #           '';
        # };

        # org-rich-yank = {
        #   enable = true;
        #   bindLocal = {
        #     org-mode-map = {
        #       "C-M-y" = "org-rich-yank";
        #     };
        #   };
        #   config = ''
        #     (defun my-org-rich-yank-format-paste (language contents link)
        #       "Based on `org-rich-yank--format-paste-default'."
        #       (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n#+comment: %s"
        #               language
        #               (org-rich-yank--trim-nl contents)
        #               link))
        #     (customize-set-variable 'org-rich-yank-format-paste #'my-org-rich-yank-format-paste)
        #   '';
        # };

        org-download = {
          enable = true;
          extraPackages = [ pkgs.wl-clipboard ];
        };

        # Highlight and annotate text file and websites
        # org-remark.enable = true;

        org-ql.enable = true;

        consult-notes = {
          enable = true;
          bind = {
            "C-c n f" = "consult-notes";
          };
          custom = {
            # "org-default-notes-file" = ''"~/dev/org-mode/inbox.org"'';
            "consult-notes-denote-display-id" = "nil";
          };
          config = ''
            (consult-notes-denote-mode)
          '';
        };

        org-tempo = {
          enable = true;
          # :demand t
          package = "org";
          config = ''
            (dolist (item '(("sh" . "src sh")
                              ("el" . "src emacs-lisp")
                              ("li" . "src lisp")
                              ("sc" . "src scheme")
                              ("ts" . "src typescript")
                              ("py" . "src python")
                              ("yaml" . "src yaml")
                              ("json" . "src json")
                              ("einit" . "src emacs-lisp :tangle emacs/init.el")
                              ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
                (add-to-list 'org-structure-template-alist item))
          '';
        };

        org-mime.enable = true;
        org-web-tools.enable = true;
        # ox-clip.enable = true;
        # ox-tufte.enable = true;

        # ox-hugo = {
        #   enable = true;
        #   after = [ "ox" ];
        # };

        org-modern = {
          enable = true;
          config = ''
            (global-org-modern-mode)
          '';
        };

        # org-hyperscheduler = {
        #   enable = true;
        # };

        # # for org-mode
        # htmlize.enable = true;

        org-sticky-header = {
          enable = true;
          hook = [
            "(org-mode . org-sticky-header-mode)"
          ];
        };

        edraw-org = {
          enable = true;
          package = _epkgs: pkgs.emacs.pkgs.el-easydraw;
        };

        org = {
          enable = true;
          command = [
            "org-mode"
            "org-version"
          ];
          bind = {
            "C-c c" = "org-capture";
            "C-c a" = "org-agenda";
            "C-c l" = "org-store-link";
            "C-c b" = "org-switchb";
          };
          custom = {
            "org-default-notes-file" = ''"~/dev/org-mode/inbox.org"'';
            "org-directory" = ''"~/dev/org-mode"'';
            # "org-log-done" = "time";
            # "org-agenda-start-with-log-mode" = true;
            # (setq org-extend-today-until 4)
            "org-extend-today-until" = 4;

            org-hide-leading-stars = "t";
            org-startup-indented = "t";
            org-adapt-indentation = "nil";
            org-edit-src-content-indentation = 0;
            org-startup-truncated = "t";
          };
          hook = [
            "(org-mode . dw/org-mode-setup)"
          ];
          config = ''
                        ;; Turn on indentation and auto-fill mode for Org files
                        (defun dw/org-mode-setup ()
                          ;; (variable-pitch-mode 1)
                          (org-indent-mode 1)
                          (auto-fill-mode 0)
                          (visual-line-mode 1)
                          (setq corfu-auto nil)
                          (setq evil-auto-indent nil))

                        (defun dw/org-move-done-tasks-to-bottom ()
                          "Sort all tasks in the topmost heading by TODO state."
                          (interactive)
                          (save-excursion
                            (while (org-up-heading-safe))
                            (org-sort-entries nil ?o))

                          ;; Reset the view of TODO items
                          (org-overview)
                          (org-show-entry)
                          (org-show-children))

                        (defun dw/org-todo-state-change-hook ()
                          (when (string= org-state "DONE")
                            (dw/org-move-done-tasks-to-bottom)))

                            (setq org-ellipsis " ▾"
                                    org-hide-emphasis-markers t
                                    org-src-fontify-natively t
                                    org-fontify-quote-and-verse-blocks t
                                    org-src-tab-acts-natively t
                                    org-edit-src-content-indentation 2
                                    org-hide-block-startup nil
                                    org-src-preserve-indentation nil
                                    org-startup-folded 'content
                                    org-cycle-separator-lines 2
                                    org-capture-bookmark nil)

                                    (setq org-refile-targets '((nil :maxlevel . 1)
                                                                (org-agenda-files :maxlevel . 1))
                                           ;; Refile items to the top of parent heading
                                           org-reverse-note-order t)

                                     (setq org-outline-path-complete-in-steps nil)
                                     (setq org-refile-use-outline-path t)

                                    (setq org-log-done 'time)
                                    (setq org-agenda-start-with-log-mode t)

                                    (setq org-priority-highest ?A
                                          org-priority-lowest  ?D
                                          org-priority-default ?C)

                                          (setq org-todo-keywords
                                                '((sequence "TODO(t)" "NEXT(n)" "STRT(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANX(c!)")
                                                  (sequence "GOAL(G)" "PROJ(P)" "|" "DONE(d!)")
                                                  (sequence  "PLAN(p)" "REVIEW(r)" "|" "WAIT(w)" "BACK(b)")))

                                          ;; TODO: org-todo-keyword-faces
                                          (setq org-todo-keyword-faces
                                                '(("GOAL" . (:foreground "orange red" :weight bold))
                                                  ("WAIT" . (:foreground "HotPink2" :weight bold))
                                                  ("BACK" . (:foreground "MediumPurple3" :weight bold))))

                                          (setq org-modern-todo-faces
                                                '(("GOAL"
                                                   :background "orange red"
                                                   :foreground "white")
                                                  ("PROJ"
                                                   :background "gold"
                                                   :foreground "black")))

                                                   ;; Make done tasks show up in the agenda log
                                                   (setq org-log-done 'time)
                                                   (setq org-log-into-drawer t)

                                                   ;; Only make context tags inheritable (what about noexport?)
                                                   (setq org-use-tag-inheritance "^@")

                                                    (setq-default org-tag-alist
                                                                  '((:startgroup)
                                                                    ("Areas")
                                                                    (:grouptags)
                                                                    ("@home" . ?H)
                                                                    ("@work" . ?W)
                                                                    (:endgroup)

                                                                    (:startgrouptag . nil)
                                                                    ("Contexts")
                                                                    (:grouptags)
                                                                    ("@computer" . ?C)
                                                                    ("@mobile" . ?M)
                                                                    ("@calls" . ?A)
                                                                    ("@errands" . ?E)
                                                                    (:endgrouptag)

                                                                    ;; Task Types
                                                                    (:startgrouptag . nil)
                                                                    ("Types")
                                                                    (:grouptags)
                                                                    ("@easy" . ?e)
                                                                    ("@hacking" . ?h)
                                                                    ("@writing" . ?w)
                                                                    ("@creative" . ?v)
                                                                    ("@accounting" . ?a)
                                                                    ("@email" . ?m)
                                                                    ("@system" . ?s)
                                                                    (:endgrouptag)

                                                                    ;; Workflow states
                                                                    (:startgroup . nil)
                                                                    ("States")
                                                                    (:grouptags)
                                                                    ("@plan" . ?p)
                                                                    ("@review" . ?r)
                                                                    ("@followup" . ?f)
                                                                    (:endgroup)))

                        (setq org-use-tag-inheritance "^@")

            ;; Active Org-babel languages
            (org-babel-do-load-languages 'org-babel-load-languages
              '((plantuml . t)
                (dot . t)
                (mermaid . t)
                (sql . t)
                (shell . t)))
          '';
          # bindLocal = {
          #   org-mode-map = {
          #     "C-x n s" = "org-toggle-narrow-to-subtree";
          #   };
          # };
          #   hook = [
          #     ''
          #       (org-mode
          #        . (lambda ()
          #            (add-hook 'completion-at-point-functions
          #                      'pcomplete-completions-at-point nil t)))
          #     ''
          #   ];
          #   config = ''
          #     ;; Some general stuff.
          #     (setq org-reverse-note-order t
          #           org-use-fast-todo-selection t
          #           org-adapt-indentation nil
          #           org-hide-emphasis-markers t)

          #     ;;(setq org-tag-alist rah-org-tag-alist)

          #     ;; Add some todo keywords.

          #     ;; Setup org capture.
          #     ;; (setq org-default-notes-file (rah-org-file "capture"))
          #     (setq org-default-notes-file (concat org-directory "/inbox.org"))

          #     ;; Unfortunately org-mode tends to take over keybindings that
          #     ;; start with C-c.
          #     (unbind-key "C-c SPC" org-mode-map)
          #     (unbind-key "C-c w" org-mode-map)

          #     (setq org-image-actual-width 400)
          #     (setq org-extend-today-until 4)
          #     (setq org-export-backends (quote (ascii html icalendar latex md odt)))

          #     (defun add-newline-at-end-if-none ()
          #       "Add a newline at the end of the buffer if there isn't any."
          #       (save-excursion
          #         (save-restriction
          #           (goto-char (1- (point-max)))
          #           (if (not (looking-at "\n"))
          #           (progn
          #             (goto-char (point-max))
          #             (insert "\n"))))))

          #     ;; https://old.reddit.com/r/orgmode/comments/hg8qik/weird_joined_lines_bug/fw73kml/
          #     (add-hook 'org-capture-prepare-finalize-hook 'add-newline-at-end-if-none)
          #   '';
        };

        org-super-agenda = {
          enable = true;
          after = [ "org" ];
          config = ''
            (org-super-agenda-mode t)
            (setq org-super-agenda-groups '((:auto-parent t)))
          '';
        };

        # org-habit = {
        #   enable = true;
        #   after = [ "org" ];
        #   # defer = true;
        #   config = ''
        #     ;; for using with Orgzly
        #     (setq org-log-into-drawer "LOGBOOK")
        #   '';
        # };

        org-agenda = {
          enable = true;
          after = [ "org" ];
          defer = true;
          custom = {
            "org-agenda-files" = ''
              '(
              "gtd.org"
              "inbox.org"
              "schedule.org"
              )
            '';
            "org-agenda-show-all-dates" = "t";
            "org-agenda-skip-deadline-if-done" = "t";
            "org-agenda-skip-scheduled-if-done" = "t";
            "org-agenda-span" = 5;
            "org-agenda-start-on-weekday" = "nil";
            "org-agenda-todo-ignore-deadlines" = 7;
            "org-agenda-todo-ignore-scheduled" = "'future";
            "org-deadline-warning-days" = 14;
            "org-stuck-projects" = ''
              '(
                "+PROJECT-MAYBE-DONE/-DONE-CANCELED" ("NEXT" "STARTED") nil "\\<IGNORE\\>"
              )
            '';
          };
          config = ''
                        (setq org-agenda-custom-commands
                              `(("d" "Dashboard"
                                 ((agenda "" ((org-deadline-warning-days 7)))
                                  (tags-todo "+PRIORITY=\"A\""
                                             ((org-agenda-overriding-header "High Priority")))
                                  (todo "*" ((org-agenda-files '("~/dev/org-mode/inbox.org"))
                                             (org-agenda-overriding-header "Unfiled Inbox Tasks")))
                                  (tags-todo "+@followup" ((org-agenda-overriding-header "Needs Follow Up")))))

                                ("u" tags-todo "+ALLTAGS=\"\""
                                 ((org-agenda-overriding-header "Untagged Tasks")))

                                ("n" "Next Tasks"
                                 ((agenda "" ((org-deadline-warning-days 7)))
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next Tasks")))))

                                ;; Low-effort next actions
                                ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                                 ((org-agenda-overriding-header "Low Effort Tasks")
                                  (org-agenda-max-todos 20)
                                  (org-agenda-files org-agenda-files)))))

            (setq org-agenda-window-setup 'current-window)
            (setq org-agenda-span 'day)
            (setq org-agenda-start-with-log-mode t)
          '';
        };

        # ol-notmuch = {
        #   enable = pcfg.org.enable && pcfg.notmuch.enable;
        #   after = [
        #     "notmuch"
        #     "org"
        #   ];
        # };

        ob-plantuml = {
          enable = true;
          after = [ "org" ];
          defer = true;
        };

        ob-mermaid = {
          enable = true;
          after = [ "org" ];
          defer = true;
        };
        # (setq ob-mermaid-cli-path "mmdc")

        # org-table = {
        #   enable = true;
        #   after = [ "org" ];
        #   command = [ "orgtbl-to-generic" ];
        #   hook = [
        #     # For orgtbl mode, add a radio table translator function for
        #     # taking a table to a psql internal variable.
        #     ''
        #       (orgtbl-mode
        #        . (lambda ()
        #            (defun rah-orgtbl-to-psqlvar (table params)
        #              "Converts an org table to an SQL list inside a psql internal variable"
        #              (let* ((params2
        #                      (list
        #                       :tstart (concat "\\set " (plist-get params :var-name) " '(")
        #                       :tend ")'"
        #                       :lstart "("
        #                       :lend "),"
        #                       :sep ","
        #                       :hline ""))
        #                     (res (orgtbl-to-generic table (org-combine-plists params2 params))))
        #                (replace-regexp-in-string ",)'$"
        #                                          ")'"
        #                                          (replace-regexp-in-string "\n" "" res))))))
        #     ''
        #   ];
        #   config = ''
        #     (unbind-key "C-c SPC" orgtbl-mode-map)
        #     (unbind-key "C-c w" orgtbl-mode-map)
        #   '';
        #   extraConfig = ''
        #     :functions org-combine-plists
        #   '';
        # };

        org-capture = {
          enable = true;
          after = [ "org" ];
          config = ''
            (defun dw/org-path (path)
              (expand-file-name path org-directory))

            (setq org-capture-templates
                    `(("t" "Tasks")
                    ("tt" "Task" entry (file ,(dw/org-path "inbox.org"))
                    "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
                    ("ts" "Clocked Entry Subtask" entry (clock)
                    "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

                    ("j" "Journal Entries")
                    ("je" "General Entry" entry
                    (file+olp+datetree ,(dw/org-path "journal.org"))
                    "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
                    :tree-type week
                    :clock-in :clock-resume
                    :empty-lines 1)
                    ("jt" "Task Entry" entry
                    (file+olp+datetree ,(dw/org-path "journal.org"))
                    "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
                    :tree-type week
                    :clock-in :clock-resume
                    :empty-lines 1)
                    ("jj" "Journal" entry
                    (file+olp+datetree ,(dw/org-path "journal.org"))
                    "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                    :tree-type week
                    :clock-in :clock-resume
                    :empty-lines 1)

                    ("m" "Metrics Capture")
                    ("mw" "Weight" table-line (file+headline "~/Notes/Metrics.org" "Weight")
                    "| %U | %^{Weight} | %^{Notes} |"
                    :immediate-finish t
                    :jump-to-captured t)
                    ("mp" "Blood Pressure" table-line (file+headline "~/Notes/Metrics.org" "Blood Pressure")
                    "| %U | %^{Systolic} | %^{Diastolic} | %^{BPM} | %^{Stress 1-5}"
                    :immediate-finish t
                    :jump-to-captured t)))
          '';
        };

        # org-clock = {
        #   enable = true;
        #   after = [ "org" ];
        #   config = ''
        #     (setq org-clock-rounding-minutes 5
        #           org-clock-out-remove-zero-time-clocks t)
        #   '';
        # };

        # org-duration = {
        #   enable = true;
        #   after = [ "org" ];
        #   config = ''
        #     ;; I always want clock tables and such to be in hours, not days.
        #     (setq org-duration-format (quote h:mm))
        #   '';
        # };

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

        # org-superstar = {
        #   enable = true;
        #   hook = [ "(org-mode . org-superstar-mode)" ];
        # };

        # org-edna = {
        #   enable = true;
        #   defer = 1;
        #   config = "(org-edna-mode)";
        # };

        # # custom
        # # (org-image-actual-width nil)
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
