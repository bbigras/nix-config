(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  )

(setq org-directory "~/dev/org-mode/")
(setq org-roam-directory "~/dev/org-mode/notes/")
(setq org-roam-dailies-directory "journal/")

(use-package! crux
  :bind (
         ("C-c d" . crux-duplicate-current-line-or-region)
         ))

(setq doom-theme 'catppuccin)
(load-theme 'catppuccin t t)
(catppuccin-set-color 'mauve "#89b4fa" 'mocha)

(setq vc-handled-backends '(Git))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-ssh-controlmaster-options ""))
