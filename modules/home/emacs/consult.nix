{
  programs.emacs = {
    init = {
      usePackage = {
        consult = {
          enable = true;
          after = [ "org" ];
          hook = [
            "(completion-list-mode . consult-preview-at-point-mode)"
          ];
          bind = {
            "C-c M-x" = "consult-mode-command";
            "C-c h" = "consult-history";
            "C-c k" = "consult-kmacro";
            "C-c i" = "consult-info";
            # ([remap Info-search] = "consult-info";
            # C-x bindings (ctl-x-map)
            "C-x M-:" = "consult-complex-command"; # orig. repeat-complex-command
            "C-x b" = "consult-buffer"; # orig. switch-to-buffer
            "C-x 4 b" = "consult-buffer-other-window"; # orig. switch-to-buffer-other-window
            "C-x 5 b" = "consult-buffer-other-frame"; # orig. switch-to-buffer-other-frame
            "C-x r b" = "consult-bookmark"; # orig. bookmark-jump
            # "C-x p b" = "consult-project-buffer"; # orig. project-switch-to-buffer
            # Custom M-# bindings for fast register access
            "M-#" = "consult-register-load";
            "M-'" = "consult-register-store"; # orig. abbrev-prefix-mark (unrelated)
            "C-M-#" = "consult-register";
            # Other custom bindings
            "M-y" = "consult-yank-pop"; # orig. yank-pop
            # M-g bindings (goto-map)
            "M-g e" = "consult-compile-error";
            "M-g f" = "consult-flymake"; # Alternative: consult-flycheck
            "M-g g" = "consult-goto-line"; # orig. goto-line
            "M-g M-g" = "consult-goto-line"; # orig. goto-line
            "M-g o" = "consult-outline"; # Alternative: consult-org-heading
            "M-g m" = "consult-mark";
            "M-g k" = "consult-global-mark";
            "M-g i" = "consult-imenu";
            "M-g I" = "consult-imenu-multi";
            # M-s bindings (search-map)
            "M-s d" = "consult-find";
            "M-s D" = "consult-locate";
            "M-s g" = "consult-grep";
            "M-s G" = "consult-git-grep";
            "M-s r" = "consult-ripgrep";
            "M-s l" = "consult-line";
            "M-s L" = "consult-line-multi";
            "M-s k" = "consult-keep-lines";
            "M-s u" = "consult-focus-lines";
            # Isearch integration
            "M-s e" = "consult-isearch-history";
          };

          bindLocal = {
            isearch-mode-map = {
              "M-e" = "consult-isearch-history"; # orig. isearch-edit-string
              "M-s e" = "consult-isearch-history"; # orig. isearch-edit-string
              "M-s l" = "consult-line"; # needed by consult-line to detect isearch
              "M-s L" = "consult-line-multi"; # needed by consult-line to detect isearch
            };
            # Minibuffer history
            minibuffer-local-map = {
              "M-s" = "consult-history"; # orig. next-matching-history-element
              "M-r" = "consult-history"; # orig. previous-matching-history-element
            };
          };

          init = ''
            ;; These settings can be applied before consult loads since they
            ;; configure built-in Emacs variables, not consult functions.
            (setq register-preview-delay 0.5
                  register-preview-function #'consult-register-format)

            ;; Optionally tweak the register preview window.
            (advice-add #'register-preview :override #'consult-register-window)

            ;; Use Consult to select xref locations with preview
            (setq xref-show-xrefs-function #'consult-xref
                  xref-show-definitions-function #'consult-xref)

            ;; org clock (can be set before consult loads)
            (setq org-clock-persist t)
            (with-eval-after-load 'org
              (org-clock-persistence-insinuate))
          '';

          config = ''
            (require 'consult-xref)
            (require 'consult-org)

            ;; Configure preview keys per-command
            (consult-customize
             consult-theme :preview-key '(:debounce 0.2 any)
             consult-ripgrep consult-git-grep consult-grep
             consult-bookmark consult-recent-file consult-xref
             :preview-key '(:debounce 0.4 any))

            ;; Configure the narrowing key.
            (setq consult-narrow-key "<")

            (defvar consult--previous-point nil
                "Location of point before entering minibuffer.
            Used to preselect nearest headings and imenu items.")

            (defun consult--set-previous-point ()
              "Save location of point. Used before entering the minibuffer."
              (setq consult--previous-point (point)))

            (advice-add #'consult-org-heading :before #'consult--set-previous-point)
            (advice-add #'consult-outline :before #'consult--set-previous-point)

            (advice-add #'vertico--update :after #'consult-vertico--update-choose)

            (defun consult-vertico--update-choose (&rest _)
              "Pick the nearest candidate rather than the first after updating candidates."
              (when (and consult--previous-point
                         (memq current-minibuffer-command
                               '(consult-org-heading consult-outline)))
                (setq vertico--index
                      (max 0 ; if none above, choose the first below
                           (1- (or (seq-position
                                    vertico--candidates
                                    consult--previous-point
                                    (lambda (cand point-pos) ; counts on candidate list being sorted
                                      (> (cl-case current-minibuffer-command
                                           (consult-outline
                                            (car (consult--get-location cand)))
                                           (consult-org-heading
                                            (get-text-property 0 'consult--candidate cand)))
                                         point-pos)))
                                   (length vertico--candidates))))))
              (setq consult--previous-point nil))

            (defun consult-clock-in (&optional match scope resolve)
              "Clock into an Org heading."
              (interactive (list nil nil current-prefix-arg))
              (require 'org-clock)
              (org-clock-load)
              (save-window-excursion
                (consult-org-heading
                 match
                 (or scope
                     (thread-last org-clock-history
                       (mapcar 'marker-buffer)
                       (mapcar 'buffer-file-name)
                       (delete-dups)
                       (delq nil))
                     (user-error "No recent clocked tasks")))
                (org-clock-in nil (when resolve
                                    (org-resolve-clocks)
                                    (org-read-date t t)))))

            (consult-customize consult-clock-in
                               :prompt "Clock in: "
                               :preview-key "M-."
                               :group
                               (lambda (cand transform)
                                 (let* ((marker (get-text-property 0 'consult--candidate cand))
                                        (name (if (member marker org-clock-history)
                                                  "*Recent*"
                                                (buffer-name (marker-buffer marker)))))
                                   (if transform (substring cand (1+ (length name))) name))))
          '';
        };

        # consult-imenu = {
        #   enable = true;
        # };

        # consult-dir = {
        #   enable = true;
        #   bind = {
        #     "C-x C-d" = "consult-dir";
        #   };
        #   config = ''
        #     (define-key minibuffer-local-completion-map (kbd "C-x C-d") #'consult-dir)
        #     (define-key minibuffer-local-completion-map (kbd "C-x C-j") #'consult-dir-jump-file)
        #   '';
        # };

        # consult-flycheck = {
        #   enable = true;
        #   bindLocal = {
        #     flycheck-command-map = {
        #       "!" = "consult-flycheck";
        #     };
        #   };
        # };

        embark-consult = {
          enable = true;
          after = [
            "embark"
            "consult"
          ];
          hook = [
            "(embark-collect-mode . consult-preview-at-point-mode)"
          ];
        };
      };
    };
  };
}
