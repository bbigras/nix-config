{ pkgs
, lib
, osConfig
, ...
}:

{
  imports = [
    ./consult.nix
    ./rust.nix
    # ./go.nix
    ./typescript.nix
    ./org-mode.nix
    ./git.nix
    # ./latex.nix
    ./tree-sitter.nix
  ];

  home.packages = with pkgs; [
  ] ++ lib.optionals (stdenv.hostPlatform.system == "x86_64-linux") [
    samba # for tramp
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-json-languageserver
    yaml-language-server
  ];

  programs.emacs = {
    enable = true;

    package = pkgs.emacs-unstable;

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
        (set-face-attribute 'default
                            nil
                            :height 105
                            :family "Source Code Pro")
        (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 100)
        (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

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

                (setq custom-file (expand-file-name (concat "custom-" (system-name) ".el") "~/dev/emacs"))
                (when (file-exists-p custom-file)
                  (load custom-file 'noerror))

                (setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
                (with-eval-after-load 'tramp
                (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
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

                ; (server-start)

                (add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)

                ; Enable mouse in terminal/TTY
                (xterm-mouse-mode 1)

                (if (version< emacs-version "29.0")
                    (pixel-scroll-mode)
                  (pixel-scroll-precision-mode 1)
                  (setq pixel-scroll-precision-large-scroll-height 35.0))

                (defun bh/prepare-meeting-notes ()
                  "Prepare meeting notes for email
                   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
                  (interactive)
                  (let (prefix)
                    (save-excursion
                      (save-restriction
                        (narrow-to-region (region-beginning) (region-end))
                        (untabify (point-min) (point-max))
                        (goto-char (point-min))
                        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
                          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
                        (goto-char (point-min))
                        (kill-ring-save (point-min) (point-max))))))

        (defun renz/async-shell-command-filter-hook ()
          "Filter async shell command output via `comint-output-filter'."
          (when (equal (buffer-name (current-buffer)) "*Async Shell Command*")
            ;; When `comint-output-filter' is non-nil, the carriage return characters ^M
            ;; are displayed
            (setq-local comint-inhibit-carriage-motion nil)
            (when-let ((proc (get-buffer-process (current-buffer))))
              ;; Attempting a solution found here:
              ;; https://gnu.emacs.help.narkive.com/2PEYGWfM/m-chars-in-async-command-output
              (set-process-filter proc 'comint-output-filter))))


        (add-hook 'shell-mode-hook #'renz/async-shell-command-filter-hook)

        ; tramp
        (setq vc-handled-backends '(Git)
                file-name-inhibit-locks t
                tramp-inline-compress-start-size 1000
                tramp-copy-size-limit 10000
                tramp-verbose 1)
        ; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
        (setq tramp-default-method "scp")
        (setq projectile--mode-line "Projectile")

      '';

      usePackage = {
        dwim-shell-command = {
          enable = true;
          extraPackages = [ pkgs.atool pkgs.zrok ];
          config = ''
                        (require 'dwim-shell-commands)

            (defun my/dwim-shell-commands-zrok-serve-dir ()
              "HTTP serve current directory."
              (interactive)
              (cond ((executable-find "zrok")
                     (dwim-shell-command-on-marked-files
                      "zrok serve current dir"
                      "zrok share public --backend-mode web --headless ."
                      :utils "zrok"
                      :focus-now t
                      :no-progress t))
                    (t
                     (error "zrok not found"))))

          '';
        };

        ef-themes = {
          enable = true;
          config = ''
            (mapc #'disable-theme custom-enabled-themes)
            (ef-themes-select 'ef-elea-dark)
          '';
        };

        bufler.enable = true;

        # Save and restore frames and windows with their buffers in Emacs
        burly.enable = true;

        autorevert = {
          enable = true;
          command = [ "auto-revert-mode" ];
        };

        dogears = {
          enable = true;
          bind = {
            "M-g d" = "dogears-go";
            "M-g M-b" = "dogears-back";
            "M-g M-f" = "dogears-forward";
            "M-g M-d" = "dogears-list";
            "M-g M-D" = "dogears-sidebar";
          };
          config = ''
            (dogears-mode)
          '';
        };

        copy-as-format = {
          enable = true;
          command = [
            "copy-as-format"
            "copy-as-format-asciidoc"
            "copy-as-format-bitbucket"
            "copy-as-format-disqus"
            "copy-as-format-github"
            "copy-as-format-gitlab"
            "copy-as-format-hipchat"
            "copy-as-format-html"
            "copy-as-format-jira"
            "copy-as-format-markdown"
            "copy-as-format-mediawiki"
            "copy-as-format-org-mode"
            "copy-as-format-pod"
            "copy-as-format-rst"
            "copy-as-format-slack"
          ];
        };

        # https://github.com/bbatsov/crux
        crux = {
          enable = true;
          bind = {
            "C-c d" = "crux-duplicate-current-line-or-region";
            "C-c M-d" = "crux-duplicate-and-comment-current-line-or-region";

            # "C-k" = "crux-smart-kill-line";
            "C-k" = "crux-kill-and-join-forward";

            "C-S-RET" = "crux-smart-open-line-above";
            "S-RET" = "crux-smart-open-line";
            "C-x 4 t" = "crux-transpose-windows";
            "C-c D" = "crux-delete-file-and-buffer";
            # "C-c r" = "crux-rename-file-and-buffer";
            "C-c TAB" = "crux-indent-rigidly-and-copy-to-clipboard";
            # "Super-j" = "crux-top-join-line";
            # "C-Backspace" = "crux-kill-line-backwards";
            "M-o" = "crux-other-window-or-switch-buffer";
          };
        };

        ws-butler = {
          enable = true;
          hook = [
            "(text-mode . ws-butler-mode)"
            "(prog-mode . ws-butler-mode)"
          ];
        };

        deadgrep = {
          enable = true;
          bind = {
            "C-x f" = "deadgrep";
          };
        };

        envrc = {
          enable = true;
          defer = 1;
          package = _epkgs: pkgs.emacsPackages.envrc;
          command = [ "envrc-global-mode" ];
          config = ''
            (envrc-global-mode 1)
          '';
        };

        dockerfile-mode.enable = true;

        doom-modeline = {
          enable = true;
          extraConfig = ":disabled";
        };

        drag-stuff = {
          enable = true;
          bind = {
            "M-<up>" = "drag-stuff-up";
            "M-<down>" = "drag-stuff-down";
          };
        };

        diff-hl = {
          enable = true;
          config = "(global-diff-hl-mode)";
        };

        eldoc = {
          enable = true;
          command = [ "eldoc-mode" ];
        };

        eldoc-box = {
          enable = true;
        };

        fira-code-mode = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          hook = [ "prog-mode" ];
          config = ''
            (fira-code-mode-set-font)
          '';
        };

        hcl-mode.enable = true;

        graphql-mode.enable = true;

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

        # Remember where we where in a previously visited file. Built-in.
        saveplace = {
          enable = true;
          defer = 1;
          config = ''
            (setq-default save-place t)
            (setq save-place-file (locate-user-emacs-file "places"))
          '';
        };

        apheleia = {
          enable = true;
          config = ''
            (apheleia-global-mode +1)
          '';
        };

        which-key = {
          enable = true;
          command = [ "which-key-mode" "which-key-add-major-mode-key-based-replacements" ];
          defer = 3;
          config = "(which-key-mode)";
        };

        all-the-icons = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
        };

        all-the-icons-completion = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          hook = [ "(marginalia-mode . all-the-icons-completion-marginalia-setup)" ];
        };

        all-the-icons-dired = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          after = [ "dired" "all-the-icons" ];
          hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
        };

        marginalia = {
          enable = true;
          command = [ "marginalia-mode" ];
          # after = [ "vertico" ];
          defer = 1;
          config = "(marginalia-mode)";
        };

        epkg-marginalia = {
          enable = true;
          # command = [ "marginalia-mode" ];
          # after = [ "vertico" ];
          defer = 1;
          # config = "(marginalia-mode)";
          config = ''
            (with-eval-after-load 'marginalia
              (cl-pushnew 'epkg-marginalia-annotate-package
                          (alist-get 'package marginalia-annotator-registry)))
          '';
        };

        embark = {
          enable = true;
          bind = {
            "C-." = "embark-act";
            "M-." = "embark-dwim";
            "C-h B" = "embark-bindings";
          };
          init = ''
            ;; Optionally replace the key help with a completing-read interface
            (setq prefix-help-command #'embark-prefix-help-command)
          '';
          config = ''
            ;; Hide the mode line of the Embark live/completions buffers
            (add-to-list 'display-buffer-alist
                         '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           nil
                           (window-parameters (mode-line-format . none))))

            (defun embark-magit-status (file)
              "Run `magit-status` on repo containing the embark target."
              (interactive "GFile: ")
              (magit-status (locate-dominating-file file ".git")))

            (defun embark-target-this-buffer-file ()
              (cons 'this-buffer-file (or (buffer-file-name) (buffer-name))))

            (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)

            (add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))

            ;; ------------------------------------------------------------


            ; Colorize the current Vertico candidate differently when acting
            (defun embark-vertico-indicator ()
              (let ((fr face-remapping-alist))
                (lambda (&optional keymap _targets prefix)
                  (when (bound-and-true-p vertico--input)
                    (setq-local face-remapping-alist
                                (if keymap
                                    (cons '(vertico-current . embark-target) fr)
                                  fr))))))

            (add-to-list 'embark-indicators #'embark-vertico-indicator)

            ; Automatically resizing auto-updating Embark Collect buffers to fit their contents
            (add-hook 'embark-collect-post-revert-hook
                      (defun resize-embark-collect-window (&rest _)
                        (when (memq embark-collect--kind '(:live :completions))
                          (fit-window-to-buffer (get-buffer-window)
                                                (floor (frame-height) 2) 1))))

            ; Switch between candidates and actions like in Helm
            (defun with-minibuffer-keymap (keymap)
              (lambda (fn &rest args)
                (minibuffer-with-setup-hook
                    (lambda ()
                      (use-local-map
                       (make-composed-keymap keymap (current-local-map))))
                  (apply fn args))))

            (defvar embark-completing-read-prompter-map
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "<tab>") 'abort-recursive-edit)
                map))

            (advice-add 'embark-completing-read-prompter :around
                        (with-minibuffer-keymap embark-completing-read-prompter-map))
            (define-key vertico-map (kbd "<tab>") 'embark-act-with-completing-read)

              (defun embark-act-with-completing-read (&optional arg)
                (interactive "P")
                (let* ((embark-prompter 'embark-completing-read-prompter)
                       (act (propertize "Act" 'face 'highlight))
                       (embark-indicator (lambda (_keymap targets) nil)))
                  (embark-act arg)))

            ; Show the current Embark target types in the modeline
            (defvar embark--target-mode-timer nil)
            (defvar embark--target-mode-string "")

            (defun embark--target-mode-update ()
              (setq embark--target-mode-string
                    (if-let (targets (embark--targets))
                        (format "[%s%s] "
                                (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
                                (mapconcat (lambda (x) (format ", %s" (plist-get x :type)))
                                           (cdr targets)
                                           ""))
                      "")))

            (define-minor-mode embark-target-mode
              "Shows the current targets in the modeline."
              :global t
              (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
              (when embark--target-mode-timer
                (cancel-timer embark--target-mode-timer)
                (setq embark--target-mode-timer nil))
              (when embark-target-mode
                (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
                (setq embark--target-mode-timer
                      (run-with-idle-timer 0.1 t #'embark--target-mode-update))))
          '';

          bindLocal = {
            minibuffer-local-completion-map = {
              "M-o" = "embark-act";
            };
            embark-file-map = {
              "s" = "sudo-edit";
              "l" = "vlf";
            };
          };
        };

        string-inflection = {
          enable = true;
          bind = {
            "C-c C-u" = "string-inflection-all-cycle";
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

        avy = {
          enable = true;
          # avy-org-goto-heading-timer
          # avy-goto-char-timer
          # avy-org-refile-as-child
          # avy-goto-word-or-subword-1
          bind = {
            "C-:" = "avy-goto-char";
            "C-'" = "avy-goto-char-2";
            "M-g f" = "avy-goto-line";
            "M-g w" = "avy-goto-word-1";
            "M-g e" = "avy-goto-word-0";
            "C-c C-j" = "avy-resume";
          };
          command = [ "avy-process" ];
          config = ''
            (setq avy-all-windows t)
          '';
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

        hammy = {
          enable = true;
          config = ''
            (hammy-mode 1)
          '';
        };

        expand-region = {
          enable = true;
          config = ''
            (global-set-key (kbd "C-=") 'er/expand-region)
          '';
        };

        markdown-mode = {
          enable = true;
          extraPackages = [ pkgs.marksman ];
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

        wgrep.enable = true;

        topsy = {
          enable = true;
          hook = [
            "(prog-mode . topsy-mode)"
            "(magit-section-mode . topsy-mode)"
          ];
        };

        nginx-mode.enable = true;

        smartparens = {
          enable = true;
          defer = 3;
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

        flycheck = {
          enable = true;
          command = [ "global-flycheck-mode" ];
          defer = 1;
          bind = {
            "M-n" = "flycheck-next-error";
            "M-p" = "flycheck-previous-error";
          };
          config = ''
            ;; Only check buffer when mode is enabled or buffer is saved.
            (setq flycheck-check-syntax-automatically '(mode-enabled save)
                flycheck-markdown-mdl-executable "${pkgs.mdl}/bin/mdl")

            ;; Enable flycheck in all eligible buffers.
            (global-flycheck-mode)
          '';
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

        savehist = {
          enable = true;
          init = "(savehist-mode)";
          config = ''
            (setq history-delete-duplicates t
                  history-length 100)
          '';
        };

        emacs = {
          enable = true;
          init = ''
            ;; Add prompt indicator to `completing-read-multiple'.
            (defun crm-indicator (args)
              (cons (concat "[CRM] " (car args)) (cdr args)))
            (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

            ;; Grow and shrink minibuffer
            ;;(setq resize-mini-windows t)

            ;; Do not allow the cursor in the minibuffer prompt
            (setq minibuffer-prompt-properties
                  '(read-only t cursor-intangible t face minibuffer-prompt))
            (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

            ;; Enable recursive minibuffers
            (setq enable-recursive-minibuffers t)
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
            (push "node_modules" projectile-globally-ignored-directories)
            (push ".cargo" projectile-globally-ignored-directories)
            (push ".idea" projectile-globally-ignored-directories)
            (push "~/" projectile-ignored-projects)

            ;;(setq projectile-ignored-projects
            ;;'("~/" "test"))

            (projectile-mode 1)
          '';

          init = ''
            (when (file-directory-p "~/dev")
            (setq projectile-project-search-path '("~/dev")))
            ;;(setq projectile-switch-project-action #'projectile-dired)
          '';
        };

        plantuml-mode.enable = true;

        company = {
          enable = true;
          command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
          defer = 1;
          extraConfig = ''
            :bind (:map company-mode-map
                        ([remap completion-at-point] . company-complete-common)
                        ([remap complete-symbol] . company-complete-common))
          '';
          config = ''
            (setq company-idle-delay 0.2
                  company-minimum-prefix-length 3
                  company-show-quick-access t
                  company-tooltip-maximum-width 100
                  company-tooltip-minimum-width 20
                  ; Allow me to keep typing even if company disapproves.
                  company-require-match nil)

            (global-company-mode)
          '';
        };

        company-posframe = {
          enable = true;
          hook = [ "(company-mode . company-posframe-mode)" ];
        };

        protobuf-mode = {
          enable = true;
          mode = [ ''"'\\.proto\\'"'' ];
        };

        yaml-mode.enable = true;

        devdocs = {
          enable = true;
          bind = {
            "C-c D" = "devdocs-lookup";
          };
        };

        dirvish = {
          enable = true;
          config = ''
            (require 'dirvish-quick-access)
            (require 'dirvish-history)
            (require 'dirvish-ls)
            (require 'dirvish-extras)
            (require 'dirvish-emerge)
            (require 'dirvish-subtree)
            (require 'dirvish-vc)
            (require 'dirvish-yank)
            (require 'dirvish-fd)
            (require 'dirvish-narrow)
            (dirvish-override-dired-mode)

            (setq dirvish-emerge-groups '(("Recent" (predicate . ‘recent-files-2h’))
              ("README" (regex . "README"))
              ("PDF"    (extensions "pdf"))
              ("Documents" (extensions "pdf" "tex" "bib" "epub"))
              ("Video" (extensions "mp4" "mkv" "webm"))
              ("Pictures" (extensions "jpg" "png" "svg" "gif"))
              ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
              ("Archives" (extensions "gz" "rar" "zip"))
              ("LaTeX"  (extensions "tex" "bib"))))
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

            ;; Save the file list every 10 minutes.
            (run-at-time nil (* 10 60) 'recentf-save-list)

            (recentf-mode)
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

        systemd = {
          enable = true;
          defer = true;
        };

        terraform-mode.enable = true;

        eglot = {
          enable = true;
          hook = [
            "(js-ts-mode . eglot-ensure)"
            "(nix-mode . eglot-ensure)"
            "(css-ts-mode . eglot-ensure)"
            "(json-ts-mode . eglot-ensure)"
            "(markdown-mode . eglot-ensure)"
            "(yaml-ts-mode . eglot-ensure)"
          ];
        };

        visual-fill-column = {
          enable = true;
          command = [ "visual-fill-column-mode" ];
        };
      };
    };
  } // lib.optionalAttrs (osConfig.programs.sway.enable or false) {
    package = pkgs.emacs-pgtk;
  };
}
