;; ----- Appearance -----
(tool-bar-mode -1) ;; disable tool bar
(menu-bar-mode -1) ;; disable menu bar

(scroll-bar-mode -1) ;; disable scroll bar

;; --- Smooth scrolling ---
(setq scroll-step 1)        ;; scroll one line at a time
(setq scroll-conservatively 10000)  ;; prevent jumping when scrolling
(setq scroll-margin 3)      ;; number of lines to keep visible at top and bottom
(setq auto-window-vscroll nil)  ;; disables automatic vertical scrolling when resizing windows


;; ----- Functionality -----
(require 'evil)
(evil-mode 1)

(setq evil-default-state 'normal)
(setq evil-motion-state-cursor 'box)

(when (eq system-type 'darwin)
  (setq exec-path (append '("/opt/homebrew/bin" "/Users/prayuj/.nvm/versions/node/v22.3.0/bin") exec-path))
  (setenv "PATH" (concat "/opt/homebrew/bin:/Users/prayuj/.nvm/versions/node/v22.3.0/bin:" (getenv "PATH"))))

(when (eq system-type 'gnu/linux)
  (setq exec-path (append '("/home/prayuj/.go/bin") exec-path))
  (setenv "PATH" (concat "/home/prayuj/.go/bin:" (getenv "PATH"))))

(require 'elcord)
(elcord-mode)

(require 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)  ;; enable ivy recent files and bookmarks
(setq ivy-count-format "(%d/%d) ")  ;; show result counts

(require 'lsp-bridge)
(require 'yasnippet)
(yas-global-mode 1)

(setq doom-themes-enable-bold t)    ; if nil, bold is universally disabled
(setq doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one-light t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(require 'projectile)
(projectile-mode +1)

(with-eval-after-load 'projectile
  (add-to-list 'projectile-ignored-projects "~/"))

(defun projectile-search-file ()
  "Run `projectile-find-file` only if the current directory is not `~/`."
  (interactive)
  (if (string= (expand-file-name "~/") (file-truename default-directory))
      (call-interactively 'projectile-switch-project)
    (call-interactively 'projectile-find-file)))


;; ----- Language Modes -----
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'typescript-mode-hook #'lsp-bridge-mode)
(add-hook 'python-mode-hook #'lsp-bridge-mode)
(add-hook 'go-mode-hook #'lsp-bridge-mode)

;; ----- LSP Configs -----
(setq lsp-bridge-enable-hover-diagnostic t)  ;; show diagnostics in hover popups
(setq lsp-bridge-signature-help-enable t)    ;; enable signature help
(setq lsp-bridge-enable-log nil) ;; ensure logging is disabled for performance

(with-eval-after-load 'lsp-bridge
  (evil-define-key 'insert lsp-bridge-mode-map
    (kbd "C-j") 'acm-select-next
    (kbd "C-k") 'acm-select-prev)
  (define-key lsp-bridge-mode-map (kbd "C-j") 'acm-select-next)
  (define-key lsp-bridge-mode-map (kbd "C-k") 'acm-select-prev)
  (define-key lsp-bridge-mode-map (kbd "C-SPC") 'lsp-bridge-find-def))


;; ----- Language Configs -----
(setq typescript-indent-level 2)


;; ----- Key Bindings -----

;; --- Global Bindings ---
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "q w") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "SPC f s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC `") 'evil-switch-to-windows-last-buffer)

  ;; --- Window Bindings ---
  (define-key evil-normal-state-map (kbd "SPC w c") 'delete-window)
  (define-key evil-normal-state-map (kbd "M-RET") 'evil-window-vsplit)

  (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)

  (define-key evil-normal-state-map (kbd "SPC b k") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "SPC q q") 'save-buffers-kill-terminal)

  (define-key evil-normal-state-map (kbd "SPC SPC") 'projectile-search-file)
  (define-key evil-normal-state-map (kbd "SPC p s") 'projectile-switch-project)
  (define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)

  (define-key evil-normal-state-map (kbd "SPC /") 'counsel-projectile-rg)
  (define-key evil-normal-state-map (kbd "SPC ?") 'projectile-ripgrep))


(global-set-key (kbd "C-/") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x) ;; overrides default command exec




;; --- Mode Bindings ---

;; Ivy

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-d") 'minibuffer-keyboard-quit))
