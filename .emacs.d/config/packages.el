(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(unless (package-installed-p 'evil-escape)
  (package-install 'evil-escape))

(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

(unless (package-installed-p 'evil-org)
  (package-install 'evil-org))

(unless (package-installed-p 'org-timeblock)
  (package-install 'org-timeblock))

(unless (package-installed-p 'org-journal)
  (package-install 'org-journal))

;; ----- Language Modes -----
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(unless (package-installed-p 'solidity-mode)
  (package-install 'solidity-mode))

(unless (package-installed-p 'nix-mode)
  (package-install 'nix-mode))

(unless (package-installed-p 'terraform-mode)
  (package-install 'terraform-mode))

(unless (file-exists-p "~/.emacs.d/packages/web-mode")
  (message "web-mode not found, please clone it into ~/.emacs.d/packages/web-mode"))

(add-to-list 'load-path "~/.emacs.d/packages/web-mode")


(unless (package-installed-p 'elcord)
  (package-install 'elcord))

(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(unless (package-installed-p 'counsel)
  (package-install 'counsel))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'rg)
  (package-install 'rg))

(unless (package-installed-p 'counsel-projectile)
  (package-install 'counsel-projectile))

(unless (package-installed-p 'magit)
  (package-install 'magit))

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

(unless (package-installed-p 'minimap)
  (package-install 'minimap))

;; --- pg + pgmacs ---
(unless (package-installed-p 'pg)
   (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
(unless (package-installed-p 'pgmacs)
   (package-vc-install "https://github.com/emarsden/pgmacs" nil nil 'pgmacs))

;; --- Copilot + Dependencies ---
(unless (package-installed-p 'dash)
  (package-install 'dash))

(unless (package-installed-p 's)
  (package-install 's))

(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

(unless (package-installed-p 'f)
  (package-install 'f))

(unless (file-exists-p "~/.emacs.d/packages/copilot")
  (message "copilot not found, please clone it into ~/.emacs.d/packages/copilot"))

(add-to-list 'load-path "~/.emacs.d/packages/copilot")

;; GPTel
(unless (file-exists-p "~/.emacs.d/packages/gptel")
  (message "gtpel not found, please clone it into ~/.emacs.d/packages/gptel"))

(add-to-list 'load-path "~/.emacs.d/packages/gptel")

;; --- LSP ---
(unless (file-exists-p "~/.emacs.d/packages/lsp-bridge")
  (message "lsp-bridge not found, please clone it into ~/.emacs.d/packages/lsp-bridge"))

(add-to-list 'load-path "~/.emacs.d/packages/lsp-bridge")

(load "~/.emacs.d/config/config.el")
