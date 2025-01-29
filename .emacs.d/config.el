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
     ("*run*"
       (display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.4))
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
(setq dashboard-projects-backend 'projectile)

(with-eval-after-load 'dashboard
  (evil-define-key 'normal dashboard-mode-map (kbd "r") 'dashboard-jump-to-recents)
  (evil-define-key 'normal dashboard-mode-map (kbd "p") 'dashboard-jump-to-projects))


(when (eq system-type 'darwin)
  (setq exec-path (append '("/opt/homebrew/bin" "/Users/prayuj/.go/bin" "/Users/prayuj/.nvm/versions/node/v22.3.0/bin") exec-path))
  (setenv "PATH" (concat "/opt/homebrew/bin:/Users/prayuj/.go/bin:/Users/prayuj/.nvm/versions/node/v22.3.0/bin:" (getenv "PATH")))
  (setenv "GOPATH" "/Users/prayuj/.go"))

(when (eq system-type 'gnu/linux)
  (setq exec-path (append '("/home/prayuj/.go/bin" "/home/prayuj/.local/bin" "/home/prayuj/.config/nvm/versions/node/v22.2.0/bin") exec-path))
  (setenv "PATH" (concat "/home/prayuj/.go/bin:/home/prayuj/.local/bin:/home/prayuj/.config/nvm/versions/node/v22.2.0/bin" (getenv "PATH")))
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
(setq copilot-max-char -1)

(add-to-list 'copilot-indentation-alist '(typescript-mode 2))
(add-to-list 'copilot-indentation-alist '(go-mode 4))
(add-to-list 'copilot-indentation-alist '(python-mode 4))
(add-to-list 'copilot-indentation-alist '(lisp-mode 2))

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
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; ----- LSP Configs -----
(require 'lsp-bridge)

(global-lsp-bridge-mode)

(setq lsp-bridge-enable-log 't)
(setq lsp-bridge-python-lsp-server 'pylsp)
(setq lsp-bridge-enable-hover-diagnostic t)  ;; show diagnostics in hover popups
(setq lsp-bridge-signature-help-enable t)    ;; enable signature help
(setq lsp-bridge-enable-log nil) ;; ensure logging is disabled for performance
(setq lsp-bridge-single-lang-server-mode-list
  '(((web-mode) . "svelteserver")))
(setq lsp-bridge-get-language-id
(lambda (project-path file-path server-name extension-name)
(when (string-equal server-name "tailwindcss")
    (cond ((string-equal extension-name "html") "html")
        ((string-equal extension-name "svelte") "svelte")
        (t "")))))

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
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x) ;; overrides default command exec
(global-set-key (kbd "C-<tab>") 'other-window) ;; overrides default command exec


;; --- Mode Bindings ---
(defun run (command directory)
  "Run a shell COMMAND in a specified DIRECTORY and display the output in a dedicated buffer.
Automatically checks for a .env file in DIRECTORY and sources it if present."
  (let* ((run-buffer-name "*run*")
         (default-directory (or directory default-directory))
         (env-file (expand-file-name ".env" default-directory))
         (env-status (if (file-exists-p env-file) "Included" "Not found"))
         (timestamp (format-time-string "%a %b %d %H:%M:%S"))
         (header (format "-*- mode: run; default-directory: \"%s\" -*-\n-*- .env: %s -*-\nCompilation started at %s\n\n"
                         default-directory env-status timestamp))
        (full-command (if (file-exists-p env-file)
                        (format "bash -c 'export $(grep -v '^#' %s | xargs) && %s'" env-file command)
                        command)))
    (with-current-buffer (get-buffer-create run-buffer-name)
      (read-only-mode -1) ;; Allow writing to the buffer
      (erase-buffer) ;; Clear the buffer for new output
      (insert header)
      (insert (format "%s\n" command)) ;; Command summary
      (insert "\n")
      (start-process-shell-command
       "run-command" run-buffer-name full-command)
      (read-only-mode 1)) ;; Set buffer to read-only mode
    (pop-to-buffer run-buffer-name)))

;; --- Go ---
(defun go-compile ()
  "Compile or test Go files based on the current buffer."
  (interactive)
  (if (string-match-p "_test\\.go\\'" (file-name-nondirectory buffer-file-name))
    (compile (concat "go test -v " buffer-file-name))
    (let ((project-root (or (projectile-project-root)
                          (error "Not in a Projectile project"))))
      (cd project-root)
      (compile "make build"))))

(defun go-run ()
  "Run the current Go project with env"
  (interactive)
  (let* ((project-root (or (projectile-project-root)
                           (error "Not in a Projectile project")))
         (env-file (expand-file-name ".env" project-root)))
    (run "make" project-root)))

(add-hook 'go-mode-hook
  (lambda ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (define-key go-mode-map (kbd "C-c C-c") 'go-compile)
    (define-key go-mode-map (kbd "C-c C-x") 'go-run)))


;; --- TypeScript ---
(defun ts-compile ()
  "Compile the current TypeScript project."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                        (error "Not in a Projectile project"))))
    (cd project-root)
    (compile "npm run build")))

(defun ts-run ()
  "Run a development server for the current TypeScript project."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                        (error "Not in a Projectile project"))))
    (cd project-root)
    (run "npm run dev" project-root)))

(add-hook 'typescript-mode-hook
  (lambda ()
    (define-key typescript-mode-map (kbd "C-c C-c") 'ts-compile)
    (define-key typescript-mode-map (kbd "C-c C-x") 'ts-run)))

;; --- Java ---
(defun find-pom-directory (root)
  "Recursively find the directory containing a pom.xml starting from ROOT."
  (let ((subdirs (directory-files root t "^[^.].*")) ; Exclude `.` and `..`
        (found nil))
    (dolist (dir subdirs found)
      (when (and (file-directory-p dir)
                 (file-exists-p (expand-file-name "pom.xml" dir)))
        (setq found dir)))
    (or found root)))

(defun java-compile ()
  "Compile the current Java project using Maven."
  (interactive)
  (if-let ((project-root (projectile-project-root)))
      (let ((pom-dir (find-pom-directory project-root)))
        (cd pom-dir)
        (compile "mvn package"))
    (error "Not in a Projectile project")))

(add-hook 'java-mode-hook
  (lambda ()
    (define-key java-mode-map (kbd "C-c C-w") 'whitespace-mode)
    (define-key java-mode-map (kbd "C-c C-c") 'java-compile)))

;; --- LaTeX ---
(defun latex-compile ()
  (interactive)
  (compile (concat "pdflatex " buffer-file-name " && rm -rf *.log *.aux *.out *.run.xml")))

(defun latex-open ()
  (interactive)
  (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))

(add-hook 'tex-mode-hook
  (lambda ()
    (define-key tex-mode-map (kbd "C-c C-c") 'latex-compile)
    (define-key tex-mode-map (kbd "C-c C-x") 'latex-open)))


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
