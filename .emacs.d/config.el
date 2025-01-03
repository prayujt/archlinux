;; ----- Appearance -----
(tool-bar-mode -1) ;; disable tool bar
(menu-bar-mode -1) ;; disable menu bar

(scroll-bar-mode -1) ;; disable scroll bar
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; --- Smooth scrolling ---
(setq scroll-step 1)        ;; scroll one line at a time
(setq scroll-conservatively 10000)  ;; prevent jumping when scrolling
(setq scroll-margin 3)      ;; number of lines to keep visible at top and bottom
(setq auto-window-vscroll nil)  ;; disables automatic vertical scrolling when resizing windows

;; --- Window splitting ---
(add-hook 'compilation-start-hook
  (lambda (_process)
    (when-let ((result (get-buffer-window "*compilation*")))
      (select-window result))))

(setq display-buffer-alist
  `(("*compilation*"
      (display-buffer-reuse-window display-buffer-at-bottom)
      (reusable-frames . visible)
      (window-height . 0.3))
     ("*dashboard*"
       (display-buffer-reuse-window display-buffer-same-window)
       (reusable-frames . visible))
     ("^magit:.*"
       (display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.4))
     ;; all other buffers use default rules
     (".*"
       (display-buffer-reuse-window display-buffer-same-window))))


;; ----- Functionality -----
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(setq evil-default-state 'normal)
(setq evil-motion-state-cursor 'box)

(evil-set-undo-system 'undo-redo)

(setq evil-undo-system 'undo-redo)

(require 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "qw")
(setq-default evil-escape-delay 0.1)

(require 'evil-collection)
(evil-collection-init '(dired magit mu4e))

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-heading-shorcut-format " [%s]")
(setq dashboard-items '((projects   . 5)
                        (recents  . 5)
                        (agenda    . 5)))
(setq dashboard-item-shortcuts '((recents   . "r")
                                 (projects  . "p")
                                 (agenda    . "a")))

(with-eval-after-load 'dashboard
  (evil-define-key 'normal dashboard-mode-map (kbd "r") 'dashboard-jump-to-recents)
  (evil-define-key 'normal dashboard-mode-map (kbd "p") 'dashboard-jump-to-projects))


(when (eq system-type 'darwin)
  (setq exec-path (append '("/opt/homebrew/bin" "/Users/prayuj/.go/bin" "/Users/prayuj/.nvm/versions/node/v22.3.0/bin") exec-path))
  (setenv "PATH" (concat "/opt/homebrew/bin:/Users/prayuj/.go/bin:/Users/prayuj/.nvm/versions/node/v22.3.0/bin:" (getenv "PATH")))
  (setenv "GOPATH" "/Users/prayuj/.go"))

(when (eq system-type 'gnu/linux)
  (setq exec-path (append '("/home/prayuj/.go/bin" "/home/prayuj/.local/bin") exec-path))
  (setenv "PATH" (concat "/home/prayuj/.go/bin:/home/prayuj/.local/bin" (getenv "PATH")))
  (setenv "GOPATH" "/home/prayuj/.go"))

(require 'mu4e)
;; Set the mail directory
(setq mu4e-maildir "~/Mail")

;; Account settings
(setq mu4e-contexts
    `(,(make-mu4e-context
	:name "Gmail"
	:match-func (lambda (msg) (when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address      . "prayujtuli@gmail.com")
		(user-full-name         . "Prayuj Tuli")
		(mu4e-sent-folder       . "/Gmail/[GMAIL].SENT MAIL")
		(mu4e-drafts-folder     . "/Gmail/[GMAIL].DRAFTS")
		(mu4e-refile-folder     . "/Gmail/[Gmail].All Mail")))))

;; Keybindings for mu4e
(global-set-key (kbd "C-c m") 'mu4e)

(evil-define-key 'normal mu4e-headers-mode-map
  (kbd "j") 'evil-next-line
  (kbd "k") 'evil-previous-line)

(setq mu4e-maildir-shortcuts
  '((:maildir "/Gmail/Inbox" :key ?g)))

;; Org Agenda
(setq org-agenda-files '("~/.emacs.d/org"))


(require 'elcord)
(elcord-mode 1)

(require 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)  ;; enable ivy recent files and bookmarks
(setq ivy-count-format "(%d/%d) ")  ;; show result counts

(require 'lsp-bridge)
(require 'yasnippet)
(yas-global-mode 1)

(setq doom-themes-enable-bold t)    ; if nil, bold is universally disabled
(setq doom-themes-enable-italic t) ; if nil, italics is universally disabled

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

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(require 'aider)
(setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))

(defun load-api-key (file-path)
  "Load the secret key from a file and set it as an environment variable."
  (let ((key (with-temp-buffer
               (insert-file-contents file-path)
               (buffer-string))))
    (setenv "ANTHROPIC_API_KEY" (string-trim key))))

(when (eq system-type 'gnu/linux)
  (load-api-key "/home/prayuj/.emacs.d/.anthropic-key.txt"))

(global-set-key (kbd "C-c a") 'aider-transient-menu)


;; ----- Language Modes -----
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(global-lsp-bridge-mode)

(add-to-list 'copilot-indentation-alist
  '(go-mode 4))

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
(setq lisp-indent-offset 2)
(setq typescript-indent-level 2)


;; ----- Key Bindings -----

;; --- Evil Bindings ---
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC f s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC f r") 'recentf-open)
  (define-key evil-normal-state-map (kbd "SPC `") 'evil-switch-to-windows-last-buffer)

  ;; --- Windows ---
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
  (define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)
  (define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)

  (define-key evil-normal-state-map (kbd "SPC /") 'counsel-projectile-rg)
  (define-key evil-normal-state-map (kbd "SPC ?") 'projectile-ripgrep))


;; --- Global Bindings ---
(global-set-key (kbd "C-c C-w") 'whitespace-mode)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x) ;; overrides default command exec
(global-set-key (kbd "C-<tab>") 'other-window) ;; overrides default command exec


;; --- Mode Bindings ---

(defun latex-compile ()
  (interactive)
  (compile (concat "pdflatex " buffer-file-name " && rm -rf *.log *.aux *.out")))

(defun latex-open ()
  (interactive)
  (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))

(add-hook 'tex-mode-hook
  (lambda ()
    (define-key tex-mode-map (kbd "C-c C-c") 'latex-compile)
    (define-key tex-mode-map (kbd "C-c C-x") 'latex-open)))

(defun go-compile ()
  "Compile or test Go files based on the current buffer."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                          (error "Not in a Projectile project"))))
    (cd project-root)
    (if (string-match-p "_test\\.go\\'" (file-name-nondirectory buffer-file-name))
        (compile "go test -v ./...")
      (compile "go get && go build && rm -rf ~/go"))))

(add-hook 'go-mode-hook
  (lambda ()
    (define-key go-mode-map (kbd "C-c C-c") 'go-compile)))


;; --- Magit Bindings ---
(defun magit-stash-current-buffer-file ()
  "Stash changes to the file in the current buffer with a prompt for a message.
The default message includes the branch name and latest commit hash."
  (interactive)
  (let* ((file (buffer-file-name))
         (branch (magit-get-current-branch))
         (commit (magit-git-string "rev-parse" "--short" "HEAD"))
         (default-message (format "%s changing some more things" commit))
         (message (read-string
                   (format "Stash message (default: %s): " default-message)
                   nil nil default-message)))
    (if file
        (magit-run-git "stash" "push" "-m" message "--" file)
      (message "Current buffer is not visiting a file!"))))

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-p") 'magit-pull)
(global-set-key (kbd "C-c C-a") 'magit-stage-buffer-file)
(global-set-key (kbd "C-c C-u") 'magit-unstage-buffer-file)
(global-set-key (kbd "C-c C-s") 'magit-stash-current-buffer-file)

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "^") 'evil-beginning-of-line)
  (define-key magit-mode-map (kbd "w") 'evil-forward-word-begin)
  (define-key magit-mode-map (kbd "l") 'evil-forward-char)
  (define-key magit-mode-map (kbd "h") 'evil-backward-char)
  (define-key magit-mode-map (kbd "P") 'magit-pull)
  (define-key magit-mode-map (kbd "L") 'magit-log)
  (define-key magit-mode-map (kbd "H") 'magit-dispatch))

;; Ivy

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-d") 'minibuffer-keyboard-quit))
