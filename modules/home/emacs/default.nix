{
  pkgs,
  lib,
  config,
  osConfig,
  flake,
  ...
}:

let
  inherit (flake) inputs;
  inherit (inputs) minimal-emacs-d;
in
{
  imports = [
    ./consult.nix
    ./denote.nix
    ./rust.nix
    ./typescript.nix
    ./org-mode.nix
    ./tree-sitter.nix
  ];

  home.packages =
    with pkgs;
    lib.optionals (stdenv.hostPlatform.system == "x86_64-linux") [
      samba # for tramp
    ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;

    init = {
      enable = true;

      packageQuickstart = false;
      recommendedGcSettings = false;
      usePackageVerbose = false;

      prelude = builtins.readFile "${minimal-emacs-d}/init.el";
      earlyInit =
        (builtins.readFile "${minimal-emacs-d}/early-init.el")
        + "\n"
        + ''
          (defconst dw/is-termux (getenv "ANDROID_ROOT"))

          ;; list font families with `fc-list : family`
          (set-face-attribute 'default nil :family "NotoSansM Nerd Font Mono" :height 130)
        '';

      usePackage = {
        # logview.enable = true;

        journalctl-mode.enable = true;

        project = {
          enable = true;
          config = lib.optionalString config.programs.jujutsu.enable ''
            (add-to-list 'project-vc-extra-root-markers ".jj")
          '';
        };

        jinx = {
          enable = true;
          hook = [
            "(emacs-startup . global-jinx-mode)"
          ];
          bind = {
            "M-$" = "jinx-correct";
            "C-M-$" = "jinx-languages";
          };
          custom = {
            "jinx-languages" = ''"fr_CA en_CA"'';
          };
        };

        kirigami = {
          enable = true;
          # config = ''
          #   (global-set-key (kbd "C-c k o") 'kirigami-open-fold)     ; Open fold at point
          #   (global-set-key (kbd "C-c k O") 'kirigami-open-fold-rec) ; Open fold recursively
          #   (global-set-key (kbd "C-c k m") 'kirigami-close-folds)   ; Close all folds
          #   (global-set-key (kbd "C-c k c") 'kirigami-close-fold)    ; Close fold at point
          #   (global-set-key (kbd "C-c k r") 'kirigami-open-folds)    ; Open all folds
          #   (global-set-key (kbd "C-c k TAB") 'kirigami-toggle-fold) ; Toggle fold at point
          # '';
        };

        p-search = {
          enable = true;
          config = ''
            (require 'p-search-x-denote)
          '';
        };

        agent-shell = {
          enable = true;
          package = _epkgs: pkgs.emacs.pkgs.agent-shell;
          config = ''
            (setopt agent-shell-file-completion-enabled t)
          '';
        };

        gleam-ts-mode = {
          enable = true;
          extraConfig = ''
            :mode (rx ".gleam" eos)
          '';
        };

        meow = {
          enable = true;
          # config = ''
          #   (meow-setup)
          #   (meow-global-mode 1)
          # '';
        };

        dock = {
          enable = true;
          init = ''
            (add-hook 'compilation-finish-functions
                       (lambda (_buf _msg) (dock-set-needs-attention)))
          '';
        };

        aidermacs = {
          enable = true;
          bind = {
            "C-c a" = "aidermacs-transient-menu";
          };
          custom = {
            "aidermacs-default-chat-mode" = "architect";
            "aidermacs-default-model" = ''"sonnet"'';
          };
          config = ''
            (aidermacs-setup-minor-mode)
          '';
        };

        # god-mode = {
        #   enable = true;
        # };

        casual-suite = {
          enable = true;
          after = [
            #   "bookmark"
            #   "calc"
            #   "dired"
            #   "ediff"
            #   "ibuffer"
            #   "info"
            "org-agenda"
            #   "re-builder"
          ];
          # bind = {
          # "C-o" = "casual-editkit-main-tmenu";
          # (casual-ediff-tmenu)
          # };
          #   "C-o" = "casual-suite-tmenu";
          #   "M-g" = "casual-avy-tmenu";
          # };
          bindLocal = {
            bookmark-bmenu-mode-map."C-o" = "casual-bookmarks-tmenu";
            calc-mode-map."C-o" = "casual-calc-tmenu";
            dired-mode-map."C-o" = "casual-dired-tmenu";
            isearch-mode-map."C-o" = "casual-isearch-tmenu";
            org-agenda-mode-map."C-o" = "casual-agenda-tmenu";
            reb-lisp-mode-map."C-o" = "casual-re-builder-tmenu";
            reb-mode-map."C-o" = "casual-re-builder-tmenu";
            symbol-overlay-map."C-o" = "casual-symbol-overlay-tmenu";
            ediff-mode-map."C-o" = "casual-ediff-tmenu";
          };
          # config = ''
          #   (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
          #   (keymap-global-set "M-g" #'casual-avy-tmenu)
          # '';
          # https://kickingvegas.github.io/casual/Ediff-Install.html
          init = ''
            (casual-ediff-install)
            (setq ediff-keep-variants nil
                  ediff-window-setup-function 'ediff-setup-windows-plain
                  ediff-split-window-function 'split-window-horizontally
            )
            (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
            (keymap-global-set "M-g" #'casual-avy-tmenu)
          '';
        };

        # inter-present-mode = {
        #   enable = true;
        #   package = _epkgs: pkgs.emacsPackages.inter-present-mode;
        #   command = [
        #     "inter-present-mode"
        #   ];
        #   # bindLocal = {
        #   #   emo-map."C-s" = "(lambda () (interactive) (inter-present-mode 'toggle))";
        #   # };
        #   config = ''
        #     (keymap-local-unset "C-z")
        #     (setopt inter-present-mode-keymap-prefix "C-z"
        #       inter-present-mode-face-height 280
        #       inter-present-mode-start-heading t)
        #   '';
        # };

        kele = {
          enable = true;
          # config = ''
          #   (kele-mode 1)
          # '';
        };

        # combobulate = {
        #   enable = true;
        #   package = _epkgs: pkgs.emacsPackages.combobulate;
        #   config = ''
        #     ;; You can customize Combobulate's key prefix here.
        #     ;; Note that you may have to restart Emacs for this to take effect!
        #     (setq combobulate-key-prefix "C-c o")

        #     ;; Optional, but recommended.
        #     ;;
        #     ;; You can manually enable Combobulate with `M-x
        #     ;; combobulate-mode'.
        #     :hook ((python-ts-mode . combobulate-mode)
        #            (js-ts-mode . combobulate-mode)
        #            (css-ts-mode . combobulate-mode)
        #            (rust-ts-mode . combobulate-mode)
        #            (yaml-ts-mode . combobulate-mode)
        #            (typescript-ts-mode . combobulate-mode)
        #            (tsx-ts-mode . combobulate-mode))
        #   '';
        # };

        disproject = {
          enable = true;
          bindLocal = {
            ctl-x-map."p" = "disproject-dispatch";
          };
        };

        pulsar = {
          enable = true;
          config = ''
            (pulsar-global-mode 1)
            (add-hook 'next-error-hook #'pulsar-pulse-line)
            (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-blue)

            ;; integration with the `consult' package:
            (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
            (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
          '';
        };

        dashboard = {
          enable = true;
          config = ''
            (setq dashboard-projects-backend 'project-el)
            (setq dashboard-items '((recents   . 5)
                                    (bookmarks . 5)
                                    (projects  . 5)
                                    (agenda    . 5)
                                    (registers . 5)))
            (setq dashboard-week-agenda t)
            (dashboard-setup-startup-hook)
          '';
        };

        # sqlformat = {
        #   enable = true;
        #   config = ''
        #     (setq sqlformat-command 'pgformatter)
        #   '';
        # };

        # activities = {
        #   enable = true;
        #   bind = {
        #     # "M-g d" = "dogears-go";
        #     "C-x C-a C-n" = "activities-new";
        #     "C-x C-a C-a" = "activities-resume";
        #     "C-x C-a C-s" = "activities-suspend";
        #     "C-x C-a C-k" = "activities-kill";
        #     "C-x C-a RET" = "activities-switch";
        #     "C-x C-a g" = "activities-revert";
        #     "C-x C-a l" = "activities-list";
        #   };
        #   init = ''
        #     ;; Prevent `edebug' default bindings from interfering.
        #     (setq edebug-inhibit-emacs-lisp-mode-bindings t)
        #   '';
        # };

        catppuccin-theme = {
          enable = true;
          config = ''
            (load-theme 'catppuccin :no-confirm)
          '';
        };

        dwim-shell-command = {
          enable = true;
          extraPackages = [
            pkgs.atool
            pkgs.zrok
          ];
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

        # consult-recoll.enable = true;

        tramp = {
          enable = false;
          custom = {
            "tramp-default-method" = ''"rsync"'';
          };
          config = ''
            (setq vc-ignore-dir-regexp
                  (format "\\(%s\\)\\|\\(%s\\)"
                          vc-ignore-dir-regexp
                          tramp-file-name-regexp))

            (setq remote-file-name-inhibit-locks t
                  remote-file-name-inhibit-auto-save-visited t)

            (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
                  tramp-verbose 2)
          '';
        };

        ultra-scroll = {
          enable = true;
          package = _epkgs: pkgs.emacs.pkgs.ultra-scroll;
          init = ''
            (setq scroll-conservatively 101 ; important!
                    scroll-margin 0)
          '';
          config = ''
            (ultra-scroll-mode 1)
          '';
        };

        # gptel = {
        #   enable = true;
        #   package = _epkgs: pkgs.emacs.pkgs.gptel;
        #   init = ''
        #     (setq gptel-model "gpt-4o")
        #   '';
        #   config = ''
        #     (require 'gptel-curl)
        #     (require 'auth-source)
        #     (require 'gptel-kagi)

        #     (defvar my-kagi-service "kagi-service-name") ;; Replace with your service name as in the auth-info file
        #     (defvar my-kagi-user "bigras.bruno@gmail.com") ;; Replace with your user name as in the auth-info file

        #     (let ((credential (auth-source-user-and-password my-kagi-service my-kagi-user)))
        #       (defvar gptel--kagi
        #         (gptel-make-kagi "Kagi" :key (if credential (cadr credential) ""))))

        #     ;; Function that requests kagi for a url summary and shows it in a side-window
        #     (defun my/kagi-summarize (url)
        #       (let ((gptel-backend gptel--kagi)
        #             (gptel-model "summarize:agnes")) ;or summarize:cecil, summarize:daphne, summarize:muriel
        #         (gptel-request
        #             url
        #           :callback
        #           (lambda (response info)
        #             (if response
        #                 (with-current-buffer (get-buffer-create "*Kagi Summary*")
        #                   (let ((inhibit-read-only t))
        #                     (erase-buffer)
        #                     (visual-line-mode 1)
        #                     (insert response)
        #                     (display-buffer
        #                      (current-buffer)
        #                      '((display-buffer-in-side-window
        #                         display-buffer-at-bottom)
        #                        (side . bottom))))
        #                   (special-mode 1))
        #                 (message "gptel-request failed with message: %s"
        #                          (plist-get info :status)))))))

        #     ;; Make this function available to Embark
        #     ;;(keymap-set embark-url-map "=" #'my/kagi-summarize)

        #     (cl-defun my/clean-up-gptel-refactored-code (beg end)
        #       "Clean up the code responses for refactored code in the current buffer.

        #                 The response is placed between BEG and END.  The current buffer is
        #                 guaranteed to be the response buffer."
        #       (when gptel-mode          ; Don't want this to happen in the dedicated buffer.
        #         (cl-return-from my/clean-up-gptel-refactored-code))
        #       (when (and beg end)
        #         (save-excursion
        #           (let ((contents
        #                  (replace-regexp-in-string
        #                   "\n*``.*\n*" ""
        #                   (buffer-substring-no-properties beg end))))
        #             (delete-region beg end)
        #             (goto-char beg)
        #             (insert contents))
        #           ;; Indent the code to match the buffer indentation if it's messed up.
        #           (indent-region beg end)
        #           (pulse-momentary-highlight-region beg end))))

        #     (add-hook 'gptel-post-response-functions #'my/clean-up-gptel-refactored-code)
        #   '';
        # };

        # # Save and restore frames and windows with their buffers in Emacs
        # burly.enable = true;

        # autorevert = {
        #   enable = true;
        #   command = [ "auto-revert-mode" ];
        # };

        # dogears = {
        #   enable = true;
        #   bind = {
        #     "M-g d" = "dogears-go";
        #     "M-g M-b" = "dogears-back";
        #     "M-g M-f" = "dogears-forward";
        #     "M-g M-d" = "dogears-list";
        #     "M-g M-D" = "dogears-sidebar";
        #   };
        #   config = ''
        #     (dogears-mode)
        #   '';
        # };

        # copy-as-format = {
        #   enable = true;
        #   command = [
        #     "copy-as-format"
        #     "copy-as-format-asciidoc"
        #     "copy-as-format-bitbucket"
        #     "copy-as-format-disqus"
        #     "copy-as-format-github"
        #     "copy-as-format-gitlab"
        #     "copy-as-format-hipchat"
        #     "copy-as-format-html"
        #     "copy-as-format-jira"
        #     "copy-as-format-markdown"
        #     "copy-as-format-mediawiki"
        #     "copy-as-format-org-mode"
        #     "copy-as-format-pod"
        #     "copy-as-format-rst"
        #     "copy-as-format-slack"
        #   ];
        # };

        # https://github.com/bbatsov/crux
        crux = {
          enable = true;
          # bind = {
          #   "C-c d" = "crux-duplicate-current-line-or-region";
          #   "C-c M-d" = "crux-duplicate-and-comment-current-line-or-region";

          #   # "C-k" = "crux-smart-kill-line";
          #   "C-k" = "crux-kill-and-join-forward";

          #   "C-S-RET" = "crux-smart-open-line-above";
          #   "S-RET" = "crux-smart-open-line";
          #   "C-x 4 t" = "crux-transpose-windows";
          #   "C-c D" = "crux-delete-file-and-buffer";
          #   # "C-c r" = "crux-rename-file-and-buffer";
          #   "C-c TAB" = "crux-indent-rigidly-and-copy-to-clipboard";
          #   # "Super-j" = "crux-top-join-line";
          #   # "C-Backspace" = "crux-kill-line-backwards";
          #   "M-o" = "crux-other-window-or-switch-buffer";
          # };
          config = ''
            (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
            (global-set-key (kbd "C-c o") #'crux-open-with)
            (global-set-key [(shift return)] #'crux-smart-open-line)
            (global-set-key (kbd "s-r") #'crux-recentf-find-file)
            (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
            (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
            (global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)

            (setq save-abbrevs 'silently)
            (setq-default abbrev-mode t)
          '';
        };

        # ws-butler = {
        #   enable = true;
        #   hook = [
        #     "(text-mode . ws-butler-mode)"
        #     "(prog-mode . ws-butler-mode)"
        #   ];
        # };

        # deadgrep = {
        #   enable = true;
        #   bind = {
        #     "C-x f" = "deadgrep";
        #   };
        # };

        copilot = {
          enable = false;
          config = ''
            (add-hook 'prog-mode-hook 'copilot-mode)
            (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
            (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
          '';
        };
        # # (setq copilot--server-executable "${pkgs.my_copilot-node-server}/lib/node_modules/copilot-node-server/copilot/dist/agent.js")
        # #
        # # my_copilot-node-server
        # # "/home/bbigras/.emacs.d/.cache/copilot/bin/copilot-node-server"

        envrc = {
          enable = true;
          defer = 1;
          # package = _epkgs: pkgs.emacs.pkgs.envrc;
          command = [ "envrc-global-mode" ];
          config = ''
            (setq envrc-remote t)
          '';
          hook = [ "(after-init . envrc-global-mode)" ];
        };

        # dockerfile-mode.enable = true;

        # eat = {
        #   enable = true;
        #   extraConfig = ''
        #     :hook ('eshell-load-hook #'eat-eshell-mode)
        #   '';
        # };

        # A minor-mode menu for the mode line
        minions = {
          enable = true;
          hook = [ "(doom-modeline-mode . minions-mode)" ];
        };

        doom-modeline = {
          enable = true;
          hook = [ "(after-init . doom-modeline-mode)" ];
        };

        # drag-stuff = {
        #   enable = true;
        #   bind = {
        #     "M-<up>" = "drag-stuff-up";
        #     "M-<down>" = "drag-stuff-down";
        #   };
        # };

        # diff-hl = {
        #   enable = true;
        #   config = "(global-diff-hl-mode)";
        # };

        # eldoc = {
        #   enable = true;
        #   command = [ "eldoc-mode" ];
        # };

        # eldoc-box = {
        #   enable = true;
        # };

        # hcl-mode.enable = true;

        # graphql-mode.enable = true;

        # hydra = {
        #   enable = true;
        #   # after = [ "org-tempo" ];
        #   after = [
        #     "org"
        #     "org-tempo"
        #   ];
        #   config = ''
        #     (progn
        #       (defhydra hydra-org-template (:color blue :hint nil)
        #         "
        #                  _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
        #                  _l_atex   _E_xample   _p_erl          _i_ndex:
        #                  _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
        #                  _s_rc     _n_ote      plant_u_ml      _H_TML:
        #                  _h_tml    ^ ^         ^ ^             _A_SCII:
        #                 "
        #         ("s" (hot-expand "<s"))
        #         ("E" (hot-expand "<e"))
        #         ("q" (hot-expand "<q"))
        #         ("v" (hot-expand "<v"))
        #         ("n" (hot-expand "<not"))
        #         ("c" (hot-expand "<c"))
        #         ("l" (hot-expand "<l"))
        #         ("h" (hot-expand "<h"))
        #         ("a" (hot-expand "<a"))
        #         ("L" (hot-expand "<L"))
        #         ("i" (hot-expand "<i"))
        #         ("e" (hot-expand "<s" "emacs-lisp"))
        #         ("p" (hot-expand "<s" "perl"))
        #         ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
        #         ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
        #         ("I" (hot-expand "<I"))
        #         ("H" (hot-expand "<H"))
        #         ("A" (hot-expand "<A"))
        #         ("<" self-insert-command "ins")
        #         ("o" nil "quit"))

        #       ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
        #       (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
        #       (defun hot-expand (str &optional mod header)
        #         "Expand org template.

        #                 STR is a structure template string recognised by org like <s. MOD is a
        #                 string with additional parameters to add the begin line of the
        #                 structure element. HEADER string includes more parameters that are
        #                 prepended to the element after the #+HEADER: tag."
        #         (let (text)
        #           (when (region-active-p)
        #             (setq text (buffer-substring (region-beginning) (region-end)))
        #             (delete-region (region-beginning) (region-end))
        #             (deactivate-mark))
        #           (when header (insert "#+HEADER: " header) (forward-line))
        #           (insert str)
        #           (org-tempo-complete-tag)
        #           (when mod (insert mod) (forward-line))
        #           (when text (insert text))))

        #       (define-key org-mode-map "<"
        #         (lambda () (interactive)
        #           (if (or (region-active-p) (looking-back "^"))
        #               (hydra-org-template/body)
        #               (self-insert-command 1))))
        #       )
        #   '';
        # };

        # # Remember where we where in a previously visited file. Built-in.
        # saveplace = {
        #   enable = true;
        #   defer = 1;
        #   config = ''
        #     (setq-default save-place t)
        #     (setq save-place-file (locate-user-emacs-file "places"))
        #   '';
        # };

        apheleia = {
          enable = true;
          config = ''
            (apheleia-global-mode +1)
          '';
        };

        # which-key = {
        #   enable = true;
        #   command = [
        #     "which-key-mode"
        #     "which-key-add-major-mode-key-based-replacements"
        #   ];
        #   defer = 3;
        #   init = ''
        #     (which-key-mode)
        #   '';
        #   config = ''
        #     (setq which-key-idle-delay 0.3)
        #   '';
        # };

        all-the-icons = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
        };

        all-the-icons-completion = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          hook = [ "(marginalia-mode . all-the-icons-completion-marginalia-setup)" ];
        };

        # dired-plus = {
        #   enable = true;
        #   # package = _epkgs: pkgs.emacs.pkgs.dired-plus;
        # };

        dired-du = {
          enable = true;
        };

        all-the-icons-dired = {
          enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          after = [
            "dired"
            "all-the-icons"
          ];
          hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
        };

        flycheck-eglot = {
          # enable = pkgs.stdenv.hostPlatform.system == "x86_64-linux";
          enable = true;
          after = [
            "eglot"
            "flycheck"
          ];
          # hook = [ "(flycheck-mode . eglot-flycheck-setup)" ];
          config = ''
            (global-flycheck-eglot-mode 1)
          '';
        };

        emacs = {
          enable = true;
          custom = {
            "tab-always-indent" = "complete";
            "text-mode-ispell-word-completion" = "nil";
            "read-extended-command-predicate" = "#'command-completion-default-include-p";
          };
        };

        tempel = {
          enable = true;
          bind = {
            "M-+" = "tempel-complete";
            "M-*" = "tempel-insert";
          };
          init = ''
            ;; Setup completion at point
            (defun tempel-setup-capf ()
              ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
              ;; only triggers on exact matches. We add `tempel-expand' *before* the main
              ;; programming mode Capf, such that it will be tried first.
              (setq-local completion-at-point-functions
                          (cons #'tempel-expand completion-at-point-functions))

              ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
              ;; a trigger prefix character in order to prevent Tempel from triggering
              ;; unexpectly.
              ;; (setq-local corfu-auto-trigger "/"
              ;;             completion-at-point-functions
              ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
              ;;                   completion-at-point-functions))
            )

            (add-hook 'conf-mode-hook 'tempel-setup-capf)
            (add-hook 'prog-mode-hook 'tempel-setup-capf)
            (add-hook 'text-mode-hook 'tempel-setup-capf)

            ;; Optionally make the Tempel templates available to Abbrev,
            ;; either locally or globally. `expand-abbrev' is bound to C-x '.
            ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
            ;; (global-tempel-abbrev-mode)
          '';
        };

        tempel-collection = {
          enable = true;
        };

        corfu = {
          enable = true;
          config = ''
            (global-corfu-mode)
          '';
        };

        nerd-icons-corfu = {
          enable = true;
          config = ''
            (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
          '';
        };

        cape = {
          enable = true;
          bind = {
            # "C-." = "embark-act";
            "C-c p" = "cape-prefix-map";
          };
          init = ''
            ;; Add to the global default value of `completion-at-point-functions' which is
            ;; used by `completion-at-point'.  The order of the functions matters, the
            ;; first function returning a result wins.  Note that the list of buffer-local
            ;; completion functions takes precedence over the global list.
            ;;(add-hook 'completion-at-point-functions #'cape-dabbrev)
            ;;(add-hook 'completion-at-point-functions #'cape-file)
            ;;(add-hook 'completion-at-point-functions #'cape-elisp-block)
            ;; (add-hook 'completion-at-point-functions #'cape-history)

            (defun my/eglot-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super
                                #'cape-emoji
                                 #'eglot-completion-at-point
                                 #'tempel-expand))))

            (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

          '';
        };

        marginalia = {
          enable = true;
          command = [ "marginalia-mode" ];
          # after = [ "vertico" ];
          defer = 1;
          config = "(marginalia-mode)";
        };

        # epkg-marginalia = {
        #   enable = true;
        #   # command = [ "marginalia-mode" ];
        #   # after = [ "vertico" ];
        #   defer = 1;
        #   # config = "(marginalia-mode)";
        #   config = ''
        #     (with-eval-after-load 'marginalia
        #       (cl-pushnew 'epkg-marginalia-annotate-package
        #                   (alist-get 'package marginalia-annotator-registry)))
        #   '';
        # };

        # embark = {
        #   enable = true;
        #   bind = {
        #     "C-." = "embark-act";
        #     "M-." = "embark-dwim";
        #     "C-h B" = "embark-bindings";
        #   };
        #   init = ''
        #     ;; Optionally replace the key help with a completing-read interface
        #     (setq prefix-help-command #'embark-prefix-help-command)
        #   '';
        #   config = ''
        #     ;; Hide the mode line of the Embark live/completions buffers
        #     (add-to-list 'display-buffer-alist
        #                  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
        #                    nil
        #                    (window-parameters (mode-line-format . none))))

        #     (defun embark-magit-status (file)
        #       "Run `magit-status` on repo containing the embark target."
        #       (interactive "GFile: ")
        #       (magit-status (locate-dominating-file file ".git")))

        #     (defun embark-target-this-buffer-file ()
        #       (cons 'this-buffer-file (or (buffer-file-name) (buffer-name))))

        #     (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)

        #     (add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))

        #     ;; ------------------------------------------------------------

        #     ; Colorize the current Vertico candidate differently when acting
        #     (defun embark-vertico-indicator ()
        #       (let ((fr face-remapping-alist))
        #         (lambda (&optional keymap _targets prefix)
        #           (when (bound-and-true-p vertico--input)
        #             (setq-local face-remapping-alist
        #                         (if keymap
        #                             (cons '(vertico-current . embark-target) fr)
        #                           fr))))))

        #     (add-to-list 'embark-indicators #'embark-vertico-indicator)

        #     ; Automatically resizing auto-updating Embark Collect buffers to fit their contents
        #     (add-hook 'embark-collect-post-revert-hook
        #               (defun resize-embark-collect-window (&rest _)
        #                 (when (memq embark-collect--kind '(:live :completions))
        #                   (fit-window-to-buffer (get-buffer-window)
        #                                         (floor (frame-height) 2) 1))))

        #     ; Switch between candidates and actions like in Helm
        #     (defun with-minibuffer-keymap (keymap)
        #       (lambda (fn &rest args)
        #         (minibuffer-with-setup-hook
        #             (lambda ()
        #               (use-local-map
        #                (make-composed-keymap keymap (current-local-map))))
        #           (apply fn args))))

        #     (defvar embark-completing-read-prompter-map
        #       (let ((map (make-sparse-keymap)))
        #         (define-key map (kbd "<tab>") 'abort-recursive-edit)
        #         map))

        #     (advice-add 'embark-completing-read-prompter :around
        #                 (with-minibuffer-keymap embark-completing-read-prompter-map))
        #     (define-key vertico-map (kbd "<tab>") 'embark-act-with-completing-read)

        #       (defun embark-act-with-completing-read (&optional arg)
        #         (interactive "P")
        #         (let* ((embark-prompter 'embark-completing-read-prompter)
        #                (act (propertize "Act" 'face 'highlight))
        #                (embark-indicator (lambda (_keymap targets) nil)))
        #           (embark-act arg)))

        #     ; Show the current Embark target types in the modeline
        #     (defvar embark--target-mode-timer nil)
        #     (defvar embark--target-mode-string "")

        #     (defun embark--target-mode-update ()
        #       (setq embark--target-mode-string
        #             (if-let (targets (embark--targets))
        #                 (format "[%s%s] "
        #                         (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
        #                         (mapconcat (lambda (x) (format ", %s" (plist-get x :type)))
        #                                    (cdr targets)
        #                                    ""))
        #               "")))

        #     (define-minor-mode embark-target-mode
        #       "Shows the current targets in the modeline."
        #       :global t
        #       (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
        #       (when embark--target-mode-timer
        #         (cancel-timer embark--target-mode-timer)
        #         (setq embark--target-mode-timer nil))
        #       (when embark-target-mode
        #         (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
        #         (setq embark--target-mode-timer
        #               (run-with-idle-timer 0.1 t #'embark--target-mode-update))))
        #   '';

        #   bindLocal = {
        #     minibuffer-local-completion-map = {
        #       "M-o" = "embark-act";
        #     };
        #     embark-file-map = {
        #       "s" = "sudo-edit";
        #       "l" = "vlf";
        #     };
        #   };
        # };

        # string-inflection = {
        #   enable = true;
        #   bind = {
        #     "C-c C-u" = "string-inflection-all-cycle";
        #   };
        # };

        secrets = {
          enable = true;
          config = ''
            (setq auth-sources '(
            "secrets:Mots de passe"
            ))
          '';
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

        # avy = {
        #   enable = true;
        #   # avy-org-goto-heading-timer
        #   # avy-goto-char-timer
        #   # avy-org-refile-as-child
        #   # avy-goto-word-or-subword-1
        #   bind = {
        #     "C-:" = "avy-goto-char";
        #     "C-'" = "avy-goto-char-2";
        #     "M-g f" = "avy-goto-line";
        #     "M-g w" = "avy-goto-word-1";
        #     "M-g e" = "avy-goto-word-0";
        #     "C-c C-j" = "avy-resume";
        #   };
        #   command = [ "avy-process" ];
        #   config = ''
        #     (setq avy-all-windows t)
        #   '';
        # };

        vundo = {
          enable = true;
          defer = 1;
          config = ''
            (setq vundo-glyph-alist vundo-unicode-symbols)
            (vundo-popup-mode 1)
          '';
        };

        # hammy = {
        #   enable = true;
        #   config = ''
        #     (hammy-mode 1)
        #   '';
        # };

        # expand-region = {
        #   enable = true;
        #   config = ''
        #     (global-set-key (kbd "C-=") 'er/expand-region)
        #   '';
        # };

        markdown-mode = {
          enable = true;
          # extraPackages = [ pkgs.marksman ];
          config = ''
            (setq markdown-command "marked")
            (add-hook 'markdown-mode-hook #'dw/setup-markdown-mode)
            (dolist (face '((markdown-header-face-1 . 1.2)
                             (markdown-header-face-2 . 1.1)
                             (markdown-header-face-3 . 1.0)
                             (markdown-header-face-4 . 1.0)
                             (markdown-header-face-5 . 1.0)))
               (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))
          '';
        };

        # pandoc-mode = {
        #   enable = true;
        #   after = [ "markdown-mode" ];
        #   hook = [ "markdown-mode" ];
        #   bindLocal = {
        #     markdown-mode-map = {
        #       "C-c C-c" = "pandoc-run-pandoc";
        #     };
        #   };
        # };

        nix-mode = {
          enable = true;
          hook = [ "(nix-mode . subword-mode)" ];
        };

        # wgrep.enable = true;

        topsy = {
          enable = true;
          hook = [
            "(prog-mode . topsy-mode)"
            "(magit-section-mode . topsy-mode)"
          ];
        };

        # nginx-mode.enable = true;

        # flycheck = {
        #   enable = true;
        #   command = [ "global-flycheck-mode" ];
        #   defer = 1;
        #   bind = {
        #     "M-n" = "flycheck-next-error";
        #     "M-p" = "flycheck-previous-error";
        #   };
        #   config = ''
        #     ;; Only check buffer when mode is enabled or buffer is saved.
        #     (setq flycheck-check-syntax-automatically '(mode-enabled save)
        #         flycheck-markdown-mdl-executable "${pkgs.mdl}/bin/mdl")

        #     ;; Enable flycheck in all eligible buffers.
        #     (global-flycheck-mode)
        #   '';
        # };

        vertico = {
          enable = true;
          command = [ "vertico-mode" ];
          init = "(vertico-mode)";
          extraConfig = ''
            :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ;; M-v is taken by vertico
              ("M-g M-c" . switch-to-completions)
              ;; Original tab binding, which we want sometimes when
              ;; using orderless completion.
              ("M-TAB" . vertico-insert))
          '';
        };

        orderless = {
          enable = true;
          init = ''
                        (setq completion-styles '(orderless partial-completion basic)
                              completion-category-defaults nil)

                              (setq completion-category-overrides '((eglot (styles orderless))
                                                                    (eglot-capf (styles orderless))))

            ;; Enable cache busting, depending on if your server returns
            ;; sufficiently many candidates in the first place.
            (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
          '';
        };

        # plantuml-mode.enable = true;

        # company = {
        #   enable = true;
        #   defer = 1;
        #   config = ''
        #     (global-company-mode t)

        #     (setq-default
        #      company-idle-delay 0.2
        #      ;;company-require-match nil
        #      ;;company-minimum-prefix-length 0

        #      ;; get only preview
        #      company-frontends '(company-preview-frontend)
        #      ;; also get a drop down
        #      company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
        #      )
        #   '';
        # };

        # protobuf-mode = {
        #   enable = true;
        #   mode = [ ''"'\\.proto\\'"'' ];
        # };

        # yaml-mode.enable = true;

        # devdocs = {
        #   enable = true;
        #   bind = {
        #     "C-c D" = "devdocs-lookup";
        #   };
        # };

        # dirvish = {
        #   enable = true;
        #   config = ''
        #     (require 'dirvish-quick-access)
        #     (require 'dirvish-history)
        #     (require 'dirvish-ls)
        #     (require 'dirvish-extras)
        #     (require 'dirvish-emerge)
        #     (require 'dirvish-subtree)
        #     (require 'dirvish-vc)
        #     (require 'dirvish-yank)
        #     (require 'dirvish-fd)
        #     (require 'dirvish-narrow)
        #     (dirvish-override-dired-mode)

        #     (setq dirvish-emerge-groups '(("Recent" (predicate . ‘recent-files-2h’))
        #       ("README" (regex . "README"))
        #       ("PDF"    (extensions "pdf"))
        #       ("Documents" (extensions "pdf" "tex" "bib" "epub"))
        #       ("Video" (extensions "mp4" "mkv" "webm"))
        #       ("Pictures" (extensions "jpg" "png" "svg" "gif"))
        #       ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
        #       ("Archives" (extensions "gz" "rar" "zip"))
        #       ("LaTeX"  (extensions "tex" "bib"))))
        #   '';
        # };

        # recentf = {
        #   enable = true;
        #   command = [ "recentf-mode" ];
        #   config = ''
        #     (setq recentf-save-file (locate-user-emacs-file "recentf")
        #           recentf-max-menu-items 20
        #           recentf-max-saved-items 500
        #           recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))

        #     ;; Save the file list every 10 minutes.
        #     (run-at-time nil (* 10 60) 'recentf-save-list)

        #     (recentf-mode)
        #   '';
        # };

        # nxml-mode = {
        #   enable = true;
        #   mode = [ ''"\\.xml\\'"'' ];
        #   config = ''
        #     (setq nxml-child-indent 4
        #           nxml-attribute-indent 4
        #           nxml-slash-auto-complete-flag t)
        #     (add-to-list 'rng-schema-locating-files
        #                  "~/.emacs.d/nxml-schemas/schemas.xml")
        #   '';
        # };

        # systemd = {
        #   enable = true;
        #   defer = true;
        # };

        # terraform-mode.enable = true;

        # visual-fill-column = {
        #   enable = true;
        #   command = [ "visual-fill-column-mode" ];
        # };
      };
    };
  }
  // lib.optionalAttrs (osConfig.services.desktopManager.cosmic.enable or false) {
    package = pkgs.emacs-pgtk;
  };
}
