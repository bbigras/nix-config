{ pkgs, config, ... }:

let
  pcfg = config.programs.emacs.init.usePackage;
in
{
  home.packages = with pkgs; [
    # samba # for tramp
  ];
  programs.emacs = {
    enable = true;

    package = pkgs.emacs-git;

    init = {
      enable = true;

      packageQuickstart = false;
      recommendedGcSettings = true;
      usePackageVerbose = false;

      earlyInit = ''
        ;; Disable some GUI distractions. We set these manually to avoid starting
        ;; the corresponding minor modes.
        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . nil) default-frame-alist)
        (push '(vertical-scroll-bars . nil) default-frame-alist)

        ;; Set up fonts early.
        ;;(set-face-attribute 'default
        ;;                    nil
        ;;                    :height 105
        ;;                    :family "Source Code Pro")
        ;;(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 100)
        ;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

        ;; Configure color theme and modeline in early init to avoid flashing
        ;; during start.
        (require 'nimbus-theme)
        (load-theme 'nimbus t)

        (require 'doom-modeline)
        (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
        (doom-modeline-mode)
      '';

      prelude = ''
        ;; Disable startup message.
        (setq inhibit-startup-screen t
              inhibit-startup-echo-area-message (user-login-name))

        (setq initial-major-mode 'fundamental-mode
              initial-scratch-message nil)

        ;; Don't blink the cursor.
        (setq blink-cursor-mode nil)

        ;; Set frame title.
        (setq frame-title-format
              '("" invocation-name ": "(:eval
                                        (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

        ;; Make sure the mouse cursor is visible at all times.
        (set-face-background 'mouse "#ffffff")

        ;; Accept 'y' and 'n' rather than 'yes' and 'no'.
        (defalias 'yes-or-no-p 'y-or-n-p)

        ;; Don't want to move based on visual line.
        (setq line-move-visual nil)

        ;; Stop creating backup and autosave files.
        (setq make-backup-files nil
              auto-save-default nil)

        ;; Default is 4k, which is too low for LSP.
        (setq read-process-output-max (* 1024 1024))

        ;; Always show line and column number in the mode line.
        (line-number-mode)
        (column-number-mode)

        ;; Enable some features that are disabled by default.
        (put 'narrow-to-region 'disabled nil)

        ;; Typically, I only want spaces when pressing the TAB key. I also
        ;; want 4 of them.
        (setq-default indent-tabs-mode nil
                      tab-width 4
                      c-basic-offset 4)

        ;; Trailing white space are banned!
        (setq-default show-trailing-whitespace t)

        ;; Use one space to end sentences.
        (setq sentence-end-double-space nil)

        ;; I typically want to use UTF-8.
        (prefer-coding-system 'utf-8)

        ;; Nicer handling of regions.
        (transient-mark-mode 1)

        ;; Make moving cursor past bottom only scroll a single line rather
        ;; than half a page.
        (setq scroll-step 1
              scroll-conservatively 5)

        ;; Enable highlighting of current line.
        (global-hl-line-mode 1)

        ;; Improved handling of clipboard in GNU/Linux and otherwise.
        (setq select-enable-clipboard t
              select-enable-primary t
              save-interprogram-paste-before-kill t)

        ;; Pasting with middle click should insert at point, not where the
        ;; click happened.
        (setq mouse-yank-at-point t)

        ;; Enable a few useful commands that are initially disabled.
        (put 'upcase-region 'disabled nil)
        (put 'downcase-region 'disabled nil)

        ;; When finding file in non-existing directory, offer to create the
        ;; parent directory.
        (defun with-buffer-name-prompt-and-make-subdirs ()
          (let ((parent-directory (file-name-directory buffer-file-name)))
            (when (and (not (file-exists-p parent-directory))
                       (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
              (make-directory parent-directory t))))

        (add-to-list 'find-file-not-found-functions #'with-buffer-name-prompt-and-make-subdirs)

        ;; Don't want to complete .hi files.
        (add-to-list 'completion-ignored-extensions ".hi")

        (defun rah-disable-trailing-whitespace-mode ()
          (setq show-trailing-whitespace nil))

        ;; Shouldn't highlight trailing spaces in terminal mode.
        (add-hook 'term-mode #'rah-disable-trailing-whitespace-mode)
        (add-hook 'term-mode-hook #'rah-disable-trailing-whitespace-mode)

        ;; Ignore trailing white space in compilation mode.
        (add-hook 'compilation-mode-hook #'rah-disable-trailing-whitespace-mode)

        (defun rah-prog-mode-setup ()
          ;; Use a bit wider fill column width in programming modes
          ;; since we often work with indentation to start with.
          (setq fill-column 80))

        (add-hook 'prog-mode-hook #'rah-prog-mode-setup)

        ;(defun rah-sort-lines-ignore-case ()
        ;  (interactive)
        ;  (let ((sort-fold-case t))
        ;    (call-interactively 'sort-lines)))

        ;; MY STUFF
        (setq auth-sources '((:source "~/.authinfo.gpg")))
        ;;(setq max-lisp-eval-depth 10000)
        ;;(setq max-specpdl-size 13000)
        (delete-selection-mode 1)
        (setq compilation-scroll-output t)
        ;;(setq compilation-scroll-output 'first-error)

        (setq org-directory "~/dev/org-mode")
        (setq custom-file (expand-file-name (concat "custom-" (system-name) ".el") "~/dev/emacs"))
        (when (file-exists-p custom-file)
          (load custom-file 'noerror))

        (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
        (with-eval-after-load 'tramp
        (add-to-list 'tramp-backup-directory-alist
                     (cons tramp-file-name-regexp nil)))

        (setq delete-old-versions -1)
        (setq version-control t)
        (setq vc-make-backup-files t)
        (setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

        (setq savehist-file "~/.config/emacs/savehist")
        (savehist-mode 1)
        (setq history-length t)
        (setq history-delete-duplicates t)
        (setq savehist-save-minibuffer-history 1)
        (setq savehist-additional-variables
              '(kill-ring
                search-ring
                regexp-search-ring))

        (setq native-comp-async-report-warnings-errors nil)

        (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
        (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
        (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

        ; (server-start)

        (add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)

        ; Enable mouse in terminal/TTY
        (xterm-mouse-mode 1)

        (if (version< emacs-version "29.0")
            (pixel-scroll-mode)
          (pixel-scroll-precision-mode 1)
          (setq pixel-scroll-precision-large-scroll-height 35.0))
      '';

      usePackage = {
        nimbus-theme = {
          enable = true;
          extraConfig = ":disabled";
        };

        doom-modeline = {
          enable = true;
          extraConfig = ":disabled";
        };

        jinx = {
          enable = true;
          config = ''
            (add-hook 'emacs-startup-hook #'global-jinx-mode)
            (setq jinx-languages "fr_CA en_CA")
          '';
        };

        marginalia = {
          enable = true;
          command = [ "marginalia-mode" ];
          # after = [ "vertico" ];
          defer = 1;
          config = "(marginalia-mode)";
        };

        undo-tree = {
          enable = true;
          defer = 1;
          command = [ "global-undo-tree-mode" ];
          config = ''
            (setq undo-tree-visualizer-relative-timestamps t
                  undo-tree-visualizer-timestamps t
                  undo-tree-enable-undo-in-region t
                  undo-tree-visualizer-diff t)
            (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
            (global-undo-tree-mode)
          '';
        };

        org-modern = {
          enable = true;
          config = ''
            (global-org-modern-mode)
          '';
        };

        org-sticky-header = {
          enable = true;
          hook = [
            "(org-mode . org-sticky-header-mode)"
          ];
        };

        org-gtd = {
          enable = true;
          init = ''
            (setq org-gtd-update-ack "3.0.0")
          '';

          config = ''
            (org-edna-mode)

            (setq org-edna-use-inheritance t)
            (setq org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
            (setq org-gtd-areas-of-focus '("Maison" "Santé" "Famille" "Carrière" "Finances" "Productivité" "Dev" "Loisirs" "Performance"))
          '';

        };

        org = {
          enable = true;
          bind = {
            "C-c c" = "org-capture";
            "C-c a" = "org-agenda";
            "C-c l" = "org-store-link";
            "C-c b" = "org-switchb";
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
            ;;(org-babel-do-load-languages 'org-babel-load-languages
            ;;                             '((plantuml . t)
            ;;                               (http . t)
            ;;                               (dot . t)
            ;;                               (restclient . t)
            ;;                               (R . t)
            ;;                               (sql . t)
            ;;                               (shell . t)))

            ;; Unfortunately org-mode tends to take over keybindings that
            ;; start with C-c.
            (unbind-key "C-c SPC" org-mode-map)
            (unbind-key "C-c w" org-mode-map)

            (setq org-image-actual-width 400)
            (setq org-extend-today-until 4)
            (setq org-export-backends (quote (ascii html icalendar latex md odt)))

            ;; https://old.reddit.com/r/orgmode/comments/hg8qik/weird_joined_lines_bug/fw73kml/
            ;;(add-hook 'org-capture-prepare-finalize-hook 'add-newline-at-end-if-none)
          '';
        };

        ob-http = {
          enable = true;
          after = [ "org" ];
          defer = true;
        };

        vertico = {
          enable = true;
          command = [ "vertico-mode" ];
          init = "(vertico-mode)";
        };

        orderless = {
          enable = true;
          init = ''
            (setq completion-styles '(orderless)
                  read-file-name-completion-ignore-case t)
          '';
        };

        ob-restclient = {
          enable = true;
          mode = [ ''("\\.http\\'" . restclient-mode)'' ];
        };
      };
    };
  };
}
