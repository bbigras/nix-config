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

;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! evil-escape :disable t)

(package! dotenv-mode)

(package! nginx-mode)
(package! protobuf-mode)
(package! systemd)
;; (package! meson-mode)
;; (package! caddyfile-mode)
;; (package! vala-mode)
;; (package! bpftrace-mode)
;; (package! emacsql-sqlite3)
;; (package! sqlite3)

;; (unpin! projectile)

(package! projectile)
(package! crux)

(package! copilot)
(package! catppuccin-theme)

;; (use-package! projectile
;; :init
;; ;; (add-to-list 'project-vc-extra-root-markers ".jj")
;; (add-to-list 'projectile-project-root-files-bottom-up ".jj")
;; ;; :build (:not compile)
;; )

;; (after! projectile
;;   ;; (add-to-list 'project-vc-extra-root-markers ".jj")
;;   (add-to-list 'projectile-project-root-files-bottom-up ".jj")
;; )
;;
;;(require 'config)
