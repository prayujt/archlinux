(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")))
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

(when (eq system-type 'darwin)
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu4e"))

(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))


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

(unless (file-exists-p "~/.emacs.d/packages/web-mode")
  (message "copilot not found, please clone it into ~/.emacs.d/packages/web-mode"))

(add-to-list 'load-path "~/.emacs.d/packages/web-mode")


(unless (package-installed-p 'elcord)
  (package-install 'elcord))

(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(unless (package-installed-p 'swiper)
  (package-install 'swiper))

(unless (package-installed-p 'counsel)
  (package-install 'counsel))

(unless (package-installed-p 'eww)
  (package-install 'eww))

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

;; --- Copilot Dependencies ---
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

(unless (file-exists-p "~/.emacs.d/packages/aider")
  (message "aider not found, please clone it into ~/.emacs.d/packages/aider"))

(add-to-list 'load-path "~/.emacs.d/packages/aider")


;; --- LSP ---
(unless (file-exists-p "~/.emacs.d/packages/lsp-bridge")
  (message "lsp-bridge not found, please clone it into ~/.emacs.d/packages/lsp-bridge"))

(add-to-list 'load-path "~/.emacs.d/packages/lsp-bridge")

(load "~/.emacs.d/config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
       "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
       "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
       "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
       "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
       "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
       "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
       default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
