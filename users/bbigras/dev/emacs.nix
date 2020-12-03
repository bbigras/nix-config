{ pkgs, lib, ... }:
let
  nurpkgs = (import (import ../../../nix).nixpkgs { });
  nurNoPkgs = (import (import ../../../nix).NUR) { inherit pkgs nurpkgs; };

  explain-pause-mode = epkgs: epkgs.trivialBuild {
    pname = "explain-pause-mode";
    version = "git";
    # dontBuild = true;
    src = (import ../../../nix/sources.nix).explain-pause-mode;
    # packageRequires = [ epkgs.async ];
  };
in
{
  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-notmuch
    nurNoPkgs.repos.rycee.hmModules.emacs-init
  ];

  home.packages = [
    pkgs.samba # pour tramp

    # emacs
  ];

  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;

    earlyInit = ''
      ;; Disable some GUI distractions. We set these manually to avoid starting
      ;; the corresponding minor modes.
      (push '(menu-bar-lines . 0) default-frame-alist)
      (push '(tool-bar-lines . nil) default-frame-alist)
      (push '(vertical-scroll-bars . nil) default-frame-alist)
      (blink-cursor-mode 0)

      ;; Set up fonts early.
      (set-face-attribute 'default
                          nil
                          :height 140
                          :family "Fira Code Retina")
      (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 100)
      (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)
    '';

    prelude = ''
                (require 'cl)

                (setq custom-file (expand-file-name (concat "custom-" (system-name) ".el") "~/Dropbox/emacs"))
                (load custom-file)

                (setq auth-sources '((:source "~/.authinfo.gpg")))
                (setq max-lisp-eval-depth 10000)
                (setq max-specpdl-size 13000)
                (delete-selection-mode 1)
                (setq org-directory "~/Dropbox/org-mode")

                ;; Keep transient cruft out of ~/.emacs.d/
                (setq user-emacs-directory "~/.cache/emacs/"
                      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
                      url-history-file (expand-file-name "url/history" user-emacs-directory)
                      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
                      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

                ;; Keep customization settings in a temporary file (thanks Ambrevar!)
                (setq custom-file
                      (if (boundp 'server-socket-dir)
                          (expand-file-name "custom.el" server-socket-dir)
                        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
                (load custom-file t)

                ;; Set up the visible bell
                (setq visible-bell t)

                (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
                (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
                (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
                (setq scroll-step 1) ;; keyboard scroll one line at a timesetq use-dialog-box nil) ; Disable dialog boxes since they weren't working in Mac OSX

                (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
                (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
                (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
                (add-to-list 'default-frame-alist '(fullscreen . maximized))

                      ;; Disable startup message.
                      (setq inhibit-startup-message t
                            inhibit-startup-echo-area-message (user-login-name))

                      (setq initial-major-mode 'fundamental-mode
                            initial-scratch-message nil)

                      ;; Set frame title.
                      (setq frame-title-format
                            '("" invocation-name ": "(:eval
                                                      (if (buffer-file-name)
                                                          (abbreviate-file-name (buffer-file-name))
                                                        "%b"))))

                      ;; Accept 'y' and 'n' rather than 'yes' and 'no'.
                      (defalias 'yes-or-no-p 'y-or-n-p)

                      ;; Don't want to move based on visual line.
                      (setq line-move-visual nil)

                      ;; Stop creating backup and autosave files.
                      (setq make-backup-files nil
                            auto-save-default nil)

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

                      ;; Make a reasonable attempt at using one space sentence separation.
                      (setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*"
                            sentence-end-double-space nil)

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

                ;;      (setq custom-file (locate-user-emacs-file "custom.el"))
                ;;      (load custom-file)

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

                      ;; Shouldn't highlight trailing spaces in terminal mode.
                      (add-hook 'term-mode (lambda () (setq show-trailing-whitespace nil)))
                      (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))

                      ;; https://github.com/emacs-lsp/lsp-mode#performance
                      (setq read-process-output-max (* 1024 1024)) ;; 1mb
                      (setq lsp-prefer-capf t)

                (defun indent-between-pair (&rest _ignored)
                  (newline)
                  (indent-according-to-mode)
                  (forward-line -1)
                  (indent-according-to-mode))

                (defun add-newline-at-end-if-none ()
            "Add a newline at the end of the buffer if there isn't any."
            (save-excursion
              (save-restriction
                (goto-char (1- (point-max)))
                (if (not (looking-at "\n"))
                (progn
                  (goto-char (point-max))
                  (insert "\n"))))))

      (defun rah-prog-mode-setup ()
        ;; Use a bit wider fill column width in programming modes
        ;; since we often work with indentation to start with.
        (setq fill-column 80))

      (add-hook 'prog-mode-hook #'rah-prog-mode-setup)

      (defun rah-sort-lines-ignore-case ()
        (interactive)
        (let ((sort-fold-case t))
          (call-interactively 'sort-lines)))

      (defun efs/presentation-setup ()
        ;; Hide the mode line
        (hide-mode-line-mode 1)

        ;; Display images inline
        (org-display-inline-images) ;; Can also use org-startup-with-inline-images

        ;; Scale the text.  The next line is for basic scaling:
        (setq text-scale-mode-amount 3)
        (text-scale-mode 1))

      (defun efs/presentation-end ()
        ;; Show the mode line again
        (hide-mode-line-mode 0)

        ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
        (text-scale-mode 0))
    '';

    usePackage = {
      explain-pause-mode = {
        enable = true;
        package = explain-pause-mode;
        config = "(explain-pause-mode t)";
      };

      abbrev = {
        enable = true;
        diminish = [ "abbrev-mode" ];
        command = [ "abbrev-mode" ];
      };

      ace-link = {
        enable = true;
        after = [ "org" ];
        config = ''
          (ace-link-setup-default)
          (define-key org-mode-map (kbd "M-o") 'ace-link-org)
        '';
      };

      adoc-mode = {
        enable = true;
        mode = [
          ''"\\.txt\\'"''
          ''"\\.adoc\\'"''
        ];
        hook = [
          ''
            (adoc-mode . (lambda ()
                           (visual-line-mode)
                           (visual-fill-column-mode)
                           (variable-pitch-mode)))
          ''
        ];
        config = ''
          (set-face-background 'markup-verbatim-face nil)
        '';
      };

      # Save and restore frames and windows with their buffers in Emacs
      burly.enable = true;

      mark-thing-at = {
        enable = true;
        #         config = ''
        # (mark-thing-at-make-keybindings "\C-xm")
        #         '';
      };

      ansi-color = {
        enable = true;
        command = [ "ansi-color-apply-on-region" ];
      };

      autorevert = {
        enable = true;
        diminish = [ "auto-revert-mode" ];
        command = [ "auto-revert-mode" ];
      };

      # back-button = {
      #   enable = true;
      #   defer = 1;
      #   diminish = [ "back-button-mode" ];
      #   command = [ "back-button-mode" ];
      #   config = ''
      #     (back-button-mode 1)

      #     ;; Make mark ring larger.
      #     (setq global-mark-ring-max 50)
      #   '';
      # };

      base16-theme = {
        enable = true;
        config = "(load-theme 'base16-tomorrow-night t)";
      };

      browse-at-remote = {
        enable = true;
        bind = {
          "C-c g g" = "browse-at-remote";
        };
      };

      # From https://github.com/mlb-/emacs.d/blob/a818e80f7790dffa4f6a775987c88691c4113d11/init.el#L472-L482
      compile = {
        enable = true;
        defer = true;
        after = [ "ansi-color" ];
        hook = [
          ''
            (compilation-filter . (lambda ()
                                    (when (eq major-mode 'compilation-mode)
                                      (ansi-color-apply-on-region compilation-filter-start (point-max)))))
          ''
        ];
      };

      prettier-js = {
        enable = true;
        after = [ "web-mode" ];
        config = ''
          (add-hook 'web-mode-hook #'prettier-js-mode)

          (setq prettier-js-command "yarn"
                prettier-js-args (list "prettier"))
        '';
      };

      ws-butler = {
        enable = true;
        hook = [
          "(text-mode . ws-butler-mode)"
          "(prog-mode . ws-butler-mode)"
        ];
      };

      centaur-tabs = {
        enable = true;
        config = ''
          (centaur-tabs-mode t)
          (centaur-tabs-headline-match)
          (setq centaur-tabs-set-icons t)
          (setq centaur-tabs-gray-out-icons 'buffer)
          (setq centaur-tabs-set-bar 'left)
          (setq centaur-tabs-set-modified-marker t)
          (centaur-tabs-group-by-projectile-project)
        '';
        bind = {
          "C-<prior>" = "centaur-tabs-backward";
          "C-<next>" = "centaur-tabs-forward";
        };
      };

      calc = {
        enable = true;
        command = [ "calc" ];
        config = ''
          (setq calc-date-format '(YYYY "-" MM "-" DD " " Www " " hh ":" mm ":" ss))
        '';
      };

      cc-mode = {
        enable = true;
        defer = true;
        hook = [
          ''
            (c-mode-common . (lambda ()
                               (subword-mode)

                               (c-set-offset 'arglist-intro '++)))
          ''
        ];
      };

      deadgrep = {
        enable = true;
        bind = {
          "C-x f" = "deadgrep";
        };
      };

      emojify = {
        enable = true;
        command = [ "emojify-mode" ];
        hook = [ "(after-init . global-emojify-mode)" ];
      };

      envrc = {
        enable = true;
        after = [ "lsp-mode" "lsp-rust" ];
        config = ''
          (envrc-global-mode)
          (with-eval-after-load 'envrc
            (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

          (push "[/\\\\]vendor$" lsp-file-watch-ignored)
          (push "[/\\\\]\\.yarn$" lsp-file-watch-ignored)
          (push "[/\\\\]\\.direnv$" lsp-file-watch-ignored)
        '';
      };

      docker-tramp.enable = true;

      dockerfile-mode.enable = true;

      alert = {
        enable = true;
        config = ''
          (setq alert-default-style 'notifications)
        '';
      };

      doom-modeline = {
        enable = true;
        hook = [ "(after-init . doom-modeline-mode)" ];
        config = ''
          (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
        '';
      };

      drag-stuff = {
        enable = true;
        bind = {
          "M-<up>" = "drag-stuff-up";
          "M-<down>" = "drag-stuff-down";
        };
      };

      ediff = {
        enable = true;
        defer = true;
        config = ''
          (setq ediff-window-setup-function 'ediff-setup-windows-plain)
        '';
      };

      diff-hl = {
        enable = true;
        config = "(global-diff-hl-mode)";
      };

      eldoc = {
        enable = true;
        diminish = [ "eldoc-mode" ];
        command = [ "eldoc-mode" ];
      };

      eldoc-box = {
        enable = true;
      };

      # Enable Electric Indent mode to do automatic indentation on RET.
      electric = {
        enable = true;
        command = [ "electric-indent-local-mode" ];
        hook = [
          "(prog-mode . electric-indent-mode)"

          # Disable for some modes.
          "(nix-mode . (lambda () (electric-indent-local-mode -1)))"
        ];
      };

      etags = {
        enable = true;
        defer = true;
        # Avoid spamming reload requests of TAGS files.
        config = "(setq tags-revert-without-query t)";
      };

      fira-code-mode = {
        enable = true;
        hook = [ "prog-mode" ];
      };

      gcmh = {
        enable = true;
        defer = 1;
        command = [ "gcmh-mode" ];
        config = ''
          (setq gcmh-verbose t)
          (gcmh-mode)
        '';
      };

      ggtags = {
        enable = true;
        defer = true;
        diminish = [ "ggtags-mode" ];
        command = [ "ggtags-mode" ];
      };

      groovy-mode = {
        enable = true;
        mode = [
          ''"\\.gradle\\'"''
          ''"\\.groovy\\'"''
          ''"Jenkinsfile\\'"''
        ];
      };

      org-download = {
        enable = true;
        after = [ "org" ];
        hook = [ "('dired-mode-hook 'org-download-enable)" ];
        config = ''
          (setq org-download-screenshot-method "xclip")
        '';
      };

      emacsql = {
        enable = true;
      };

      emacsql-sqlite = {
        enable = true;
      };

      #     org-journal = {
      #       enable = true;
      #       bind = {
      #         "C-c n j" = "org-journal-new-entry";
      #       };
      #       config = ''
      # (setq org-journal-date-prefix "#+TITLE: ")
      # (setq org-journal-file-format "%Y-%m-%d.org")
      # (setq org-journal-dir "~/Dropbox/org-mode/notes/")
      # (setq org-journal-date-format "%A, %d %B %Y")
      #       '';
      #     };

      deft = {
        enable = true;
        after = [ "org" ];
        bind = {
          "C-c n d" = "deft";
        };
        config = ''
          (setq deft-recursive t)
          (setq deft-use-filter-string-for-filename t)
          (setq deft-default-extension "org")
          (setq deft-directory "~/Dropbox/org-mode/notes/")
        '';
      };

      org-roam = {
        enable = true;
        after = [ "org" "org-element" "emacsql" "emacsql-sqlite" ];
        hook = [ "(after-init . org-roam-mode)" ];
        config = ''
          (setq org-roam-directory "~/Dropbox/org-mode/notes")
          (setq org-roam-graph-executable "${pkgs.graphviz}/bin/dot")
        '';
        bind = {
          "C-c n l" = "org-roam";
          "C-c n t" = "org-roam-dailies-find-today";
          "C-c n f" = "org-roam-find-file";
          "C-c n i" = "org-roam-insert";
          "C-c n g" = "org-roam-graph";
        };
        extraPackages = [ pkgs.sqlite ];
      };

      rainbow-mode = {
        enable = true;
        defer = 1;
        hook = [
          ''(org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode)''
        ];
      };

      elcord = {
        enable = true;
        config = ''
          (setq elcord-display-buffer-details nil)
          (elcord-mode)
        '';
      };

      rainbow-delimiters = {
        enable = true;
        hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
      };

      org-tempo = {
        enable = true;
        package = "org";
      };

      hydra = {
        enable = true;
        # after = [ "org-tempo" ];
        after = [ "org" "org-tempo" ];
        config = ''
          (progn
                               (defhydra hydra-org-template (:color blue :hint nil)
                                 "
           _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
           _l_atex   _E_xample   _p_erl          _i_ndex:
           _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
           _s_rc     _n_ote      plant_u_ml      _H_TML:
           _h_tml    ^ ^         ^ ^             _A_SCII:
          "
                                 ("s" (hot-expand "<s"))
                                 ("E" (hot-expand "<e"))
                                 ("q" (hot-expand "<q"))
                                 ("v" (hot-expand "<v"))
                                 ("n" (hot-expand "<not"))
                                 ("c" (hot-expand "<c"))
                                 ("l" (hot-expand "<l"))
                                 ("h" (hot-expand "<h"))
                                 ("a" (hot-expand "<a"))
                                 ("L" (hot-expand "<L"))
                                 ("i" (hot-expand "<i"))
                                 ("e" (hot-expand "<s" "emacs-lisp"))
                                 ("p" (hot-expand "<s" "perl"))
                                 ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
                                 ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
                                 ("I" (hot-expand "<I"))
                                 ("H" (hot-expand "<H"))
                                 ("A" (hot-expand "<A"))
                                 ("<" self-insert-command "ins")
                                 ("o" nil "quit"))

                               ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
                               (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
                               (defun hot-expand (str &optional mod header)
                                 "Expand org template.

          STR is a structure template string recognised by org like <s. MOD is a
          string with additional parameters to add the begin line of the
          structure element. HEADER string includes more parameters that are
          prepended to the element after the #+HEADER: tag."
                                 (let (text)
                                   (when (region-active-p)
                                     (setq text (buffer-substring (region-beginning) (region-end)))
                                     (delete-region (region-beginning) (region-end))
                                     (deactivate-mark))
                                   (when header (insert "#+HEADER: " header) (forward-line))
                                   (insert str)
                                   (org-tempo-complete-tag)
                                   (when mod (insert mod) (forward-line))
                                   (when text (insert text))))

                               (define-key org-mode-map "<"
                                 (lambda () (interactive)
                                   (if (or (region-active-p) (looking-back "^"))
                                       (hydra-org-template/body)
                                     (self-insert-command 1))))
                               )
        '';
      };

      js = {
        enable = true;
        mode = [
          ''("\\.js\\'" . js-mode)''
          ''("\\.json\\'" . js-mode)''
        ];
        config = ''
          (setq js-indent-level 2)
        '';
      };

      notifications = {
        enable = true;
        command = [ "notifications-notify" ];
      };

      # Remember where we where in a previously visited file. Built-in.
      saveplace = {
        enable = true;
        defer = 1;
        config = ''
          (setq-default save-place t)
          (setq save-place-file (locate-user-emacs-file "places"))
        '';
      };

      # More helpful buffer names. Built-in.
      uniquify = {
        enable = true;
        defer = 5;
        config = ''
          (setq uniquify-buffer-name-style 'post-forward)
        '';
      };

      # Hook up hippie expand.
      hippie-exp = {
        enable = true;
        bind = {
          "M-?" = "hippie-expand";
        };
      };

      which-key = {
        enable = true;
        command = [ "which-key-mode" "which-key-add-major-mode-key-based-replacements" ];
        diminish = [ "which-key-mode" ];
        defer = 3;
        config = "(which-key-mode)";
      };

      # Enable winner mode. This global minor mode allows you to
      # undo/redo changes to the window configuration. Uses the
      # commands C-c <left> and C-c <right>.
      winner = {
        enable = true;
        defer = 2;
        config = "(winner-mode 1)";
      };

      writeroom-mode = {
        enable = true;
        command = [ "writeroom-mode" ];
        bind = {
          "M-[" = "writeroom-decrease-width";
          "M-]" = "writeroom-increase-width";
        };
        hook = [ "(writeroom-mode . visual-line-mode)" ];
        config = ''
          (setq writeroom-bottom-divider-width 0)
        '';
      };

      buffer-move = {
        enable = true;
        bind = {
          "C-S-<up>" = "buf-move-up";
          "C-S-<down>" = "buf-move-down";
          "C-S-<left>" = "buf-move-left";
          "C-S-<right>" = "buf-move-right";
        };
      };

      all-the-icons = {
        enable = true;
        extraPackages = [ pkgs.emacs-all-the-icons-fonts ];
      };

      all-the-icons-dired = {
        enable = true;
        after = [ "dired" "all-the-icons" ];
        hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
      };

      # Lets counsel do prioritization. A fork of smex.
      amx = {
        enable = true;
        command = [ "amx-initialize" ];
      };

      counsel = {
        enable = true;
        bind = {
          "C-x C-d" = "counsel-dired-jump";
          "C-x C-f" = "counsel-find-file";
          "C-x C-M-f" = "counsel-fzf";
          "C-x C-r" = "counsel-recentf";
          "C-x C-y" = "counsel-yank-pop";
          "M-x" = "counsel-M-x";
        };
        diminish = [ "counsel-mode" ];
        config =
          let
            fd = "${pkgs.fd}/bin/fd";
            fzf = "${pkgs.fzf}/bin/fzf";
          in
          ''
            (setq counsel-fzf-cmd "${fd} --type f | ${fzf} -f \"%s\"")
          '';
      };

      nyan-mode = {
        enable = true;
        command = [ "nyan-mode" ];
        config = ''
          (setq nyan-wavy-trail t)
        '';
      };

      string-inflection = {
        enable = true;
        bind = {
          "C-c C-u" = "string-inflection-all-cycle";
        };
      };

      # Configure magit, a nice mode for the git SCM.
      magit = {
        enable = true;
        bind = {
          "C-x g" = "magit-status";
        };
        config = ''
          (add-to-list 'git-commit-style-convention-checks
                       'overlong-summary-line)
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

      multiple-cursors = {
        enable = true;
        bind = {
          "C-S-c C-S-c" = "mc/edit-lines";
          "C-c m" = "mc/mark-all-like-this";
          "C->" = "mc/mark-next-like-this";
          "C-<" = "mc/mark-previous-like-this";
        };
      };

      nix-sandbox = {
        enable = true;
        command = [ "nix-current-sandbox" "nix-shell-command" ];
      };

      avy = {
        enable = true;
        bind = {
          "M-j" = "avy-goto-word-or-subword-1";
        };
        config = ''
          (setq avy-all-windows t)
        '';
      };

      undo-tree = {
        enable = true;
        defer = 1;
        diminish = [ "undo-tree-mode" ];
        command = [ "global-undo-tree-mode" ];
        config = ''
          (setq undo-tree-visualizer-relative-timestamps t
                undo-tree-visualizer-timestamps t
                undo-tree-enable-undo-in-region t)
          (global-undo-tree-mode)
        '';
      };

      literate-calc-mode = {
        enable = true;
      };

      lsp-ui = {
        enable = true;
        command = [ "lsp-ui-mode" ];
        bind = {
          "C-c r d" = "lsp-ui-doc-show";
          "C-c f s" = "lsp-ui-find-workspace-symbol";
        };
        config = ''
          (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
          (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
        '';
        # (setq lsp-ui-doc-enable nil)
        #lsp-ui-doc-enable nil)

        # (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
        # (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


        # (setq lsp-ui-doc-enable t)
        # (setq lsp-eldoc-hook nil)
        # (setq lsp-ui-doc-delay 2)

      };

      # (setq lsp-ui-sideline-enable t
      #       lsp-ui-sideline-show-symbol nil
      #       lsp-ui-sideline-show-hover nil
      #       lsp-ui-sideline-show-code-actions nil
      #       lsp-ui-sideline-update-mode 'point)

      lsp-ui-flycheck = {
        enable = true;
        command = [ "lsp-flycheck-enable" ];
        after = [ "flycheck" "lsp-ui" ];
      };

      lsp-mode = {
        enable = true;
        command = [ "lsp" ];
        after = [ "company" "flycheck" ];
        bind = {
          "C-c r r" = "lsp-rename";
          "C-c r f" = "lsp-format-buffer";
          "C-c r g" = "lsp-format-region";
          "C-c r a" = "lsp-execute-code-action";
          "C-c f r" = "lsp-find-references";
        };
        bindLocal = {
          lsp-mode-map = {
            "C-=" = "lsp-extend-selection";
          };
        };
        config = ''
                    (setq
                       lsp-headerline-breadcrumb-enable t)

          ;; lsp-eldoc-render-all nil
          ;;lsp-modeline-code-actions-enable nil

          (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
          (lsp-register-client
           (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                            :major-modes '(nix-mode)
                            :server-id 'nix))

        '';
        #                 lsp-prefer-flymake nil
        # (setq lsp-enable-semantic-highlighting t)
      };

      lsp-modeline = {
        enable = true;
        after = [ "lsp-mode" ];
      };

      lsp-headerline = {
        enable = true;
        after = [ "lsp-mode" ];
      };

      expand-region = {
        enable = true;
        config = ''
          (global-set-key (kbd "C-=") 'er/expand-region)
        '';
      };

      lsp-rust = {
        enable = true;
        defer = true;
        after = [ "lsp-mode" "rustic" ];
        config = lib.mkForce ''
          (setq lsp-rust-server 'rust-analyzer)
          (setf (lsp--client-priority (gethash 'rust-analyzer lsp-clients)) 100)
          (setq lsp-disabled-clients '(rls))
        '';
      };

      # lsp-treemacs = {
      #   enable = true;
      #   after = [ "lsp-mode" "treemacs" ];
      # };

      # pour dap-mode
      # posframe = {
      #   enable = true;
      # };

      dap-mode = {
        enable = true;
        after = [ "lsp-mode" ];
        command = [ "dap-mode" "dap-auto-configure-mode" ];
        config = ''
          (dap-auto-configure-mode)
        '';
      };

      dap-mouse = {
        enable = true;
        command = [ "dap-tooltip-mode" ];
      };

      dap-ui = {
        enable = true;
        after = [ "dap-mode" ];
        command = [ "dap-ui-mode" ];
        config = ''
          (dap-ui-mode t)
        '';
      };

      dap-lldb = {
        enable = true;
        # config = lib.mkForce "";
        after = [ "dap-mode" ];
        package = "dap-mode";
        config = ''
          (setq dap-lldb-debug-program "${pkgs.lldb}/bin/lldb-vscode")
        '';
      };

      frog-jump-buffer = {
        enable = true;
      };

      markdown-mode = {
        enable = true;
        config = ''
          (setq markdown-command "${pkgs.pandoc}/bin/pandoc")
        '';
      };

      pandoc-mode = {
        enable = true;
        after = [ "markdown-mode" ];
        hook = [ "markdown-mode" ];
        bindLocal = {
          markdown-mode-map = {
            "C-c C-c" = "pandoc-run-pandoc";
          };
        };
      };

      nix-mode = {
        enable = true;
        hook = [ "(nix-mode . subword-mode)" ];
      };

      notmuch = {
        enable = true;
        config = ''
          (setq notmuch-search-oldest-first nil)
        '';
      };

      # Use ripgrep for fast text search in projects. I usually use
      # this through Projectile.
      ripgrep = {
        enable = true;
        command = [ "ripgrep-regexp" ];
      };

      # html export
      htmlize = {
        enable = true;
      };

      org-web-tools = {
        enable = true;
        after = [ "org" ];
        extraPackages = [ pkgs.pandoc ];
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

          (define-advice org-set-tags-command (:around (fn &rest args) my-counsel-tags)
            "Forward to `counsel-org-tag' unless given non-nil arguments."
            (if (remq nil args)
                (apply fn args)
              (counsel-org-tag)))

          (setq org-image-actual-width 400)
          (setq org-extend-today-until 4)
          (setq org-export-backends (quote (ascii html icalendar latex md odt)))

          ;; https://old.reddit.com/r/orgmode/comments/hg8qik/weird_joined_lines_bug/fw73kml/
          (add-hook 'org-capture-prepare-finalize-hook 'add-newline-at-end-if-none)
        '';
      };

      org-ql.enable = true;
      org-sidebar.enable = true;

      nginx-mode.enable = true;

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
                 (concat org-directory "/gtd.org")))
          (setq org-agenda-span 5
                org-deadline-warning-days 14
                org-agenda-show-all-dates t
                org-agenda-skip-deadline-if-done t
                org-agenda-skip-scheduled-if-done t
                org-agenda-start-on-weekday nil)
        '';
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

      hide-mode-line = {
        enable = true;
      };

      org-tree-slide = {
        enable = true;
        command = [ "org-tree-slide-mode" ];
        hook = [
          "(org-tree-slide-play . efs/presentation-setup)"
          "(org-tree-slide-stop . efs/presentation-end)"
        ];
      };

      # Set up yasnippet. Defer it for a while since I don't generally
      # need it immediately.
      yasnippet = {
        enable = true;
        defer = 3;
        diminish = [ "yas-minor-mode" ];
        command = [ "yas-global-mode" "yas-minor-mode" "yas-expand-snippet" ];
        hook = [
          # Yasnippet interferes with tab completion in ansi-term.
          "(term-mode . (lambda () (yas-minor-mode -1)))"
          "(yas-minor-mode-hook . (lambda () (yas-activate-extra-mode 'fundamental-mode)))"
        ];
        config = "(yas-global-mode 1)";
      };

      yasnippet-snippets = {
        enable = true;
        after = [ "yasnippet" ];
      };

      smartparens = {
        enable = true;
        defer = 3;
        diminish = [ "smartparens-mode" ];
        command = [ "smartparens-global-mode" "show-smartparens-global-mode" ];
        bindLocal = {
          smartparens-mode-map = {
            "C-M-f" = "sp-forward-sexp";
            "C-M-b" = "sp-backward-sexp";
          };
        };
        config = ''
          (require 'smartparens-config)
          (smartparens-global-mode t)
          (show-smartparens-global-mode t)

          (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
          (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
          (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
        '';
      };

      fill-column-indicator = {
        enable = true;
        command = [ "fci-mode" ];
      };

      flycheck = {
        enable = true;
        diminish = [ "flycheck-mode" ];
        command = [ "global-flycheck-mode" ];
        defer = 1;
        bind = {
          "M-n" = "flycheck-next-error";
          "M-p" = "flycheck-previous-error";
        };
        config = ''
          ;; Only check buffer when mode is enabled or buffer is saved.
          (setq flycheck-check-syntax-automatically '(mode-enabled save))

          ;; Enable flycheck in all eligible buffers.
          (global-flycheck-mode)

          (flycheck-add-mode 'javascript-eslint 'web-mode)
        '';
      };

      flycheck-plantuml = {
        enable = true;
        hook = [ "(flycheck-mode . flycheck-plantuml-setup)" ];
      };

      selectrum = {
        enable = true;
        config = ''
          (selectrum-mode +1)
        '';
      };

      prescient = {
        enable = true;
        config = ''
          ;; to save your command history on disk, so the sorting gets more
          ;; intelligent over time
          (prescient-persist-mode +1)
        '';
      };

      selectrum-prescient = {
        enable = true;
        after = [ "prescient" "selectrum" ];
        config = ''
          ;; to make sorting and filtering more intelligent
          (selectrum-prescient-mode +1)
        '';
      };

      company-prescient = {
        enable = true;
        after = [ "prescient" "company" ];
        config = ''
          (company-prescient-mode +1)
        '';
      };

      ctrlf = {
        enable = true;
        config = ''
          (ctrlf-mode +1)
        '';
      };

      projectile = {
        enable = true;
        diminish = [ "projectile-mode" ];
        command = [ "projectile-mode" "projectile-project-root" ];
        bindKeyMap = {
          "C-c p" = "projectile-command-map";
        };
        config = ''
          (setq projectile-completion-system 'default)
          (setq projectile-enable-caching t)
          (push "vendor" projectile-globally-ignored-directories)
          (push ".yarn" projectile-globally-ignored-directories)
          (push ".direnv" projectile-globally-ignored-directories)
          (projectile-mode 1)
        '';

        init = ''
          (when (file-directory-p "~/dev")
          (setq projectile-project-search-path '("~/dev")))
          ;;(setq projectile-switch-project-action #'projectile-dired)
        '';
      };

      plantuml-mode = {
        enable = true;
        mode = [ ''"\\.puml\\'"'' ];
      };

      ace-window = {
        enable = true;
        extraConfig = ''
          :bind* (("C-c w" . ace-window)
                  ("M-o" . ace-window))
        '';
      };

      company = {
        enable = true;
        diminish = [ "company-mode" ];
        command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
        defer = 1;
        extraConfig = ''
          :bind (:map company-mode-map
                      ([remap completion-at-point] . company-complete-common)
                      ([remap complete-symbol] . company-complete-common))
        '';
        config = ''
          (setq company-idle-delay 0.0
                company-minimum-prefix-length 1
                company-show-numbers t
                company-tooltip-maximum-width 100
                company-tooltip-minimum-width 20
                ; Allow me to keep typing even if company disapproves.
                company-require-match nil)

          (global-company-mode)
        '';
      };

      company-box = {
        enable = true;
        hook = [ "(company-mode . company-box-mode)" ];
        config = ''
          (setq company-box-icons-alist 'company-box-icons-all-the-icons)
        '';
      };

      company-yasnippet = {
        enable = true;
        after = [ "company" "yasnippet" ];
        bind = {
          "M-/" = "company-yasnippet";
        };
      };

      company-dabbrev = {
        enable = true;
        after = [ "company" ];
        command = [ "company-dabbrev" ];
        config = ''
          (setq company-dabbrev-downcase nil
                company-dabbrev-ignore-case t)
        '';
      };

      company-quickhelp = {
        enable = true;
        after = [ "company" ];
        command = [ "company-quickhelp-mode" ];
        config = ''
          (company-quickhelp-mode 1)
        '';
      };

      company-cabal = {
        enable = true;
        after = [ "company" ];
        command = [ "company-cabal" ];
        config = ''
          (add-to-list 'company-backends 'company-cabal)
        '';
      };

      company-restclient = {
        enable = true;
        after = [ "company" "restclient" ];
        command = [ "company-restclient" ];
        config = ''
          (add-to-list 'company-backends 'company-restclient)
        '';
      };

      protobuf-mode = {
        enable = true;
        mode = [ ''"'\\.proto\\'"'' ];
      };

      python = {
        enable = true;
        mode = [ ''("\\.py\\'" . python-mode)'' ];
        hook = [ "ggtags-mode" ];
      };

      restclient = {
        enable = true;
        mode = [ ''("\\.http\\'" . restclient-mode)'' ];
      };

      ob-restclient = {
        enable = true;
        mode = [ ''("\\.http\\'" . restclient-mode)'' ];
      };

      transpose-frame = {
        enable = true;
        bind = {
          "C-c f t" = "transpose-frame";
        };
      };

      tt-mode = {
        enable = true;
        mode = [ ''"\\.tt\\'"'' ];
      };

      smart-tabs-mode = {
        enable = false;
        config = ''
          (smart-tabs-insinuate 'c 'c++ 'cperl 'java)
        '';
      };

      yaml-mode.enable = true;

      wc-mode = {
        enable = true;
        command = [ "wc-mode" ];
      };

      web-mode = {
        enable = true;
        mode = [
          ''"\\.html\\'"''
          ''"\\.jsx?\\'"''
          ''"\\.tsx\\'"''
        ];
        config = ''
          (setq web-mode-attr-indent-offset 4
                web-mode-code-indent-offset 2
                web-mode-markup-indent-offset 2)

          (add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
        '';
      };

      dired = {
        enable = true;
        command = [ "dired" "dired-jump" ];
        config = ''
          (put 'dired-find-alternate-file 'disabled nil)
          ;; Use the system trash can.
          (setq delete-by-moving-to-trash t)
          (setq dired-dwim-target t)
          (setq dired-listing-switches "-agho --group-directories-first")
        '';
      };

      wdired = {
        enable = true;
        bindLocal = {
          dired-mode-map = {
            "C-c C-w" = "wdired-change-to-wdired-mode";
          };
        };
        config = ''
          ;; I use wdired quite often and this setting allows editing file
          ;; permissions as well.
          (setq wdired-allow-to-change-permissions t)
        '';
      };

      dired-single = {
        enable = true;
        after = [ "dired" ];
      };

      # Hide hidden files when opening a dired buffer. But allow showing them by
      # pressing `.`.
      dired-x = {
        enable = true;
        # after = [ "dired" ];
        hook = [ "(dired-mode . dired-omit-mode)" ];
        bindLocal.dired-mode-map = {
          "." = "dired-omit-mode";
        };
        config = ''
          (setq dired-omit-verbose nil
                dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
        '';
      };

      recentf = {
        enable = true;
        command = [ "recentf-mode" ];
        config = ''
          (setq recentf-save-file (locate-user-emacs-file "recentf")
                recentf-max-menu-items 20
                recentf-max-saved-items 500
                recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))
        '';
      };

      nxml-mode = {
        enable = true;
        mode = [ ''"\\.xml\\'"'' ];
        config = ''
          (setq nxml-child-indent 4
                nxml-attribute-indent 4
                nxml-slash-auto-complete-flag t)
          (add-to-list 'rng-schema-locating-files
                       "~/.emacs.d/nxml-schemas/schemas.xml")
        '';
      };

      rustic = {
        enable = true;
        config = ''
          (setq rustic-lsp-server 'rust-analyzer)
        '';
      };

      # sendmail = {
      #   enable = false;
      #   mode = [
      #     ''("mutt-" . mail-mode)''
      #     ''("\\.article" . mail-mode))''
      #   ];
      #   hook = [
      #     ''
      #       (lambda ()
      #             (auto-fill-mode)     ; Avoid having to M-q all the time.
      #             (rah-mail-flyspell)  ; I spel funily soemtijms.
      #             (rah-mail-reftex)    ; Make it easy to include references.
      #             (mail-text))         ; Jump to the actual text.
      #     ''
      #   ];
      # };

      systemd = {
        enable = true;
        defer = true;
      };

      terraform-mode.enable = true;

      treemacs = {
        enable = true;
        bind = {
          "C-c t f" = "treemacs-find-file";
          "C-c t t" = "treemacs";
        };
      };

      treemacs-projectile = {
        enable = true;
        after = [ "treemacs" "projectile" ];
      };

      verb = {
        enable = true;
        defer = true;
        after = [ "org" ];
        config = ''
          (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
          (setq verb-trim-body-end "[ \t\n\r]+")
        '';
      };

      visual-fill-column = {
        enable = true;
        command = [ "visual-fill-column-mode" ];
      };

      vterm = {
        enable = true;
        command = [ "vterm" ];
      };

      whole-line-or-region = {
        enable = true;
        config = ''
          (whole-line-or-region-global-mode t)
        '';
      };

    };
  };
}
