;;; init.el -*- lexical-binding: t; -*-

;; Copyright 2024 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (setq doom-font (font-spec :family "Fira Mono" :size 12))
;; (setq doom-font (font-spec :family "Iosevka" :size 12))

;; (setq doom-font (font-spec :family "Fira Mono" :size 12))

(doom! :completion
       (vertico +icons)
       (company +childframe)

       :ui
       doom
       doom-dashboard
       modeline
       nav-flash
       ophints
       (popup +defaults)
       window-select
       minimap
       (ligatures +extra)

       :checkers
       grammar
       spell
       syntax

       :editor
       file-templates
       (format +onsave)
       multiple-cursors
       snippets
       word-wrap

       :emacs
       (undo +tree)
       (dired +icons)
       tramp
       electric

       :term
       ;; eshell
       vterm

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       ;; (cc +lsp)
       ;; csharp
       ;; data
       (dart +flutter +lsp)
       emacs-lisp
       ;; (go +lsp)
       (graphql +lsp)
       ;; (haskell +lsp)
       (json +lsp)
       (java +lsp)
       (javascript +lsp)
       ;; latex
       ;; lua
       (markdown +grip)
       (nix +lsp)
       ;; (org +pretty +roam2)
       ;; (python +lsp +pyright +cython)
       ;; (ruby +rails)
       (rust +lsp)
       ;; (scheme +guile)
       (kotlin +lsp)

       ;; (org +pretty +roam)
       ;; (python +lsp +pyright +cython)
       ;; (ruby +rails)
       ;; (scheme +guile)
       (sh +lsp)
       (web +css +html +lsp)
       (yaml +lsp)
       plantuml

       ;; :email
       ;; (mu4e +org +gmail)
       ;; (wanderlust +gmail)

       ;; :app
       ;; (rss +org)

       ;; :email
       ;; (mu4e +org +gmail)
       ;; (wanderlust +gmail)

       ;; :app
       ;; (rss +org)

       :tools
       direnv
       (lsp +peek)
       (debugger +lsp)
       (docker +lsp)
       (lookup +dictionary +docsets)
       (magit)
       (rgb)
       (terraform)
       (tmux)
       tree-sitter

       :config
       (default +bindings +smartparens))
