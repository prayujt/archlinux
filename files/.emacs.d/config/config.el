(setq enable-local-eval t)

;; ----- Appearance -----
(tool-bar-mode -1) ;; disable tool bar
(menu-bar-mode -1) ;; disable menu bar

(scroll-bar-mode -1) ;; disable scroll bar
(electric-indent-mode -1) ;; disable weird indentation
(electric-pair-mode) ;; enable pair matching
(setq native-comp-async-report-warnings-errors nil)

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
	 ("\\*rg\\*"
       (display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.5))
	 ("\\*Calendar\\*"
	  (display-buffer-in-side-window)
	  (side . right)
	  (window-width . 0.4)
	  (window-parameters . ((no-other-window . t)
							(no-delete-other-windows . t))))
	 ("\\`CAPTURE-.*\\.org\\'"
       (display-buffer-in-side-window)
       (side . right)
       (window-width . 0.5)
       (window-parameters . ((no-other-window . t)
                              (no-delete-other-windows . t))))
     ("\\*Claude\\*"
       (display-buffer-in-side-window)
       (side . right)
       (slot . 0)
       (window-width . 0.5))

     ;; all other buffers use default rules
     (".*"
       (display-buffer-reuse-window display-buffer-same-window))))


;; ----- Functionality -----
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(setq evil-default-state 'normal)
(setq evil-motion-state-cursor 'box)
(setq evil-shift-width 2)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defvar indent-space-size-map nil
  "Map of major modes to indentation sizes.")

(setq indent-space-size-map
      '((c-mode . 4)
        (c++-mode . 4)
        (typescript-mode . 2)
        (python-mode . nil)
        (go-mode . 4)
        (lisp-mode . 2)
        (elisp-mode . 2)
        (emacs-lisp-mode . 2)
        (makefile-mode . nil)
        (makefile-gmake-mode . nil)
        (terraform-mode . 2)
        (web-mode . 2)))

(defun get-indent-space-size ()
  "Get indentation size for the current major mode.
Returns nil if the mode should use tabs."
  (cdr (assoc major-mode indent-space-size-map)))

(defun insert-space-tab ()
  "Insert spaces based on the current major mode's indentation size.
If indent size is nil, insert a tab character."
  (interactive)
  (let ((spaces (get-indent-space-size)))
    (if spaces
        (insert (make-string spaces ?\s))
      (insert "\t"))))

(define-key evil-insert-state-map (kbd "TAB") 'insert-space-tab)
(define-key evil-insert-state-map (kbd "<backtab>") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "<backtab>") 'indent-for-tab-command)
(define-key evil-visual-state-map (kbd "<backtab>") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

(evil-set-undo-system 'undo-redo)

(setq evil-undo-system 'undo-redo)

(require 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "qw")
(setq-default evil-escape-delay 0.1)

(require 'evil-collection)
(evil-collection-init '(dired magit vterm))

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
  (evil-define-key 'normal dashboard-mode-map (kbd "p") 'dashboard-jump-to-projects)
  (evil-define-key 'normal dashboard-mode-map (kbd "a") 'dashboard-jump-to-agenda))

(setq calendar-width 5)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 140)
  (setq exec-path (append '("/opt/homebrew/bin" "/Users/prayuj/.go/bin" "/Users/prayuj/.nvm/versions/node/v22.3.0/bin") exec-path))
  (setenv "PATH" (concat "/opt/homebrew/bin:/Users/prayuj/.go/bin:/Users/prayuj/.nvm/versions/node/v22.3.0/bin:" (getenv "PATH")))
  (setenv "GOPATH" "/Users/prayuj/.go"))

(when (eq system-type 'gnu/linux)
  (setq exec-path (append '("/home/prayuj/.nix-profile/bin" "/home/prayuj/.local/bin" "/home/prayuj/.elixirls" "/home/prayuj/.go/bin") exec-path))
  (setenv "PATH" (concat "/home/prayuj/.nix-profile/bin:/home/prayuj/.local/bin:/home/prayuj/.go/bin:" (getenv "PATH")))
  (setenv "GOPATH" "/home/prayuj/.go"))


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
(load-theme 'doom-nord-light t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(require 'projectile)
(projectile-mode +1)

(with-eval-after-load 'projectile
  ;; hybrid = alien's fast listing + projectile's own ignore filtering, so the
  ;; lists below apply even to generated files that aren't gitignored
  (setq projectile-indexing-method 'hybrid)
  (add-to-list 'projectile-ignored-projects "~/")

  (setq projectile-enable-caching t)

  ;; directories to skip in SPC SPC file search
  (dolist (dir '("node_modules" "dist" "build" "target" "vendor" "bin" "obj"
                 "gen" "generated" ".gen" "__generated__"
                 ".next" ".nuxt" ".svelte-kit" "__pycache__"
                 ".venv" ".mypy_cache" ".pytest_cache" "coverage"))
    (add-to-list 'projectile-globally-ignored-directories dir))

  ;; generated / build-artifact file suffixes to skip (protobuf, grpc,
  ;; minified, sourcemaps, etc.)
  (dolist (suf '(".pb.go" ".pb.gw.go" "_grpc.pb.go"
                 "_pb2.py" "_pb2_grpc.py" ".pb.cc" ".pb.h"
                 ".pb.ts" ".pb.js" "_pb.js" ".pb.dart"
                 ".g.dart" ".freezed.dart" ".gen.go"
                 ".min.js" ".min.css" ".map"))
    (add-to-list 'projectile-globally-ignored-file-suffixes suf)))

;; custom cache file for compilation commands
(defvar projectile-compilation-cmd-cache-file
  (expand-file-name "projectile-compilation-cmds.eld" user-emacs-directory)
  "File to store projectile compilation commands.")

;; load compilation commands on startup
(when (file-exists-p projectile-compilation-cmd-cache-file)
  (setq projectile-compilation-cmd-map
        (projectile-unserialize projectile-compilation-cmd-cache-file)))

(defun projectile-compile-with-default (default-cmd &optional force-prompt)
  "Run projectile compile with DEFAULT-CMD as the suggested default.
Uses projectile's caching - if a command is already saved for this project,
use that. Otherwise, prompt with DEFAULT-CMD pre-filled.
With prefix argument (C-u), always prompt to change the command."
  (interactive)
  (let* ((project-root (projectile-acquire-root))
         (default-directory project-root)
         (cached-cmd (gethash project-root projectile-compilation-cmd-map))
         (command (if (and cached-cmd (not current-prefix-arg))
                      cached-cmd
                    (projectile-maybe-read-command project-root
                                                   (or cached-cmd default-cmd)
                                                   "Compile command: "))))
    (puthash project-root command projectile-compilation-cmd-map)
    (projectile-serialize projectile-compilation-cmd-map projectile-compilation-cmd-cache-file)
    (projectile-run-compilation command)))

(defun projectile-configure-compile-command (default-cmd)
  "Change the saved compile command for the current project."
  (interactive)
  (let* ((project-root (projectile-acquire-root))
         (cached-cmd (gethash project-root projectile-compilation-cmd-map))
         (command (projectile-maybe-read-command project-root
                                                 (or cached-cmd default-cmd)
                                                 "Compile command: ")))
    (puthash project-root command projectile-compilation-cmd-map)
    (projectile-serialize projectile-compilation-cmd-map projectile-compilation-cmd-cache-file)
    (message "Compile command saved: %s" command)))

(defun projectile-search-file ()
  "Run `projectile-find-file` only if the current directory is not `~/`."
  (interactive)
  (if (file-equal-p default-directory (expand-file-name "~"))
      (let ((projectile-auto-discover nil))
        (call-interactively 'projectile-switch-project))
    (call-interactively 'projectile-find-file)))


(require 'minimap)
;;(minimap-mode 1)

(setq minimap-window-location 'right)
(setq minimap-update-delay 0.1)  ;; defaults to 0.5
(setq minimap-highlight-line t)


;; ----- AI Assistants -----
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(setq copilot-max-char -1)
(setq copilot-indent-offset 2)

(add-to-list 'copilot-indentation-alist '(typescript-mode 2))
(add-to-list 'copilot-indentation-alist '(makefile-gmake-mode 2))
(add-to-list 'copilot-indentation-alist '(makefile-mode 2))
(add-to-list 'copilot-indentation-alist '(go-mode 4))
(add-to-list 'copilot-indentation-alist '(c++-mode 2))
(add-to-list 'copilot-indentation-alist '(c-mode 2))
(add-to-list 'copilot-indentation-alist '(python-mode 4))
(add-to-list 'copilot-indentation-alist '(nix-mode 2))
(add-to-list 'copilot-indentation-alist '(lisp-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
(add-to-list 'copilot-indentation-alist '(sql-mode 2))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; ----- Claude Code IDE -----
(require 'claude-code-ide)

(setq claude-code-ide-cli-path "/usr/bin/claude")
;; mirror the zsh alias: skip permission prompts
(setq claude-code-ide-cli-extra-flags "--dangerously-skip-permissions")

;; vterm terminal backend (smoothest terminal in Emacs)
(setq claude-code-ide-terminal-backend 'vterm)

;; full IDE integration: expose Emacs (xref, project, tree-sitter, imenu...)
;; to Claude via MCP so it opens/edits real Emacs buffers
(setq claude-code-ide-enable-mcp-server t)
(claude-code-ide-emacs-tools-setup)

;; apply Claude's edits silently (no auto ediff popup); the file buffers just
;; refresh in place via auto-revert. Review diffs on demand with SPC c d / c l.
(setq claude-code-ide-use-ide-diff nil)

;; window placement for the Claude session
(setq claude-code-ide-window-side 'right)
(setq claude-code-ide-window-width 100)

;; Terminal UX: the Claude buffer stays in evil insert state and behaves like a
;; normal terminal. Our global evil insert binds (RET -> newline-and-indent,
;; TAB -> insert-space-tab) would otherwise shadow the terminal, so send them
;; straight to the process here. C-j/C-k scroll, M-hjkl / C-<tab> manage windows.
(with-eval-after-load 'vterm
  (evil-collection-define-key 'insert 'vterm-mode-map
    (kbd "RET") 'vterm-send-return
    (kbd "<return>") 'vterm-send-return
    (kbd "TAB") 'vterm-send-tab
    (kbd "<tab>") 'vterm-send-tab
    ;; Esc interrupts Claude (sends escape to the terminal) instead of leaving
    ;; insert state, matching Claude's normal interrupt behavior.
    ;; Note: bind <escape> only, not ESC (ESC is the Meta prefix for M-h etc.)
    (kbd "<escape>") 'vterm-send-escape
    ;; here C-SPC opens the highlighted/at-point URL (elsewhere it's jump-to-def)
    (kbd "C-SPC") 'browse-url-region-or-point
    ;; scroll the terminal
    (kbd "C-j") 'evil-scroll-line-down
    (kbd "C-k") 'evil-scroll-line-up
    ;; window management
    (kbd "M-h") 'evil-window-left
    (kbd "M-l") 'evil-window-right
    (kbd "M-k") 'evil-window-up
    (kbd "M-j") 'evil-window-down
    (kbd "C-<tab>") 'other-window)
  ;; C-SPC opens the URL in the Claude buffer from normal/visual state too
  (evil-collection-define-key '(normal visual) 'vterm-mode-map
    (kbd "C-SPC") 'browse-url-region-or-point))

;; claude-code-ide keys its sessions by project.el's root, but we navigate by
;; projectile. Make project.el resolve the same root projectile uses, so a
;; session started in a project is found from *any* buffer in that project
;; (fixes the brittle "No Claude Code session for this project" message).
(defun claude-code-ide--projectile-project-root (dir)
  "Return a project.el `transient' project for DIR using projectile's root."
  (when-let ((root (and (fboundp 'projectile-project-root)
                        (projectile-project-root dir))))
    (cons 'transient root)))
(with-eval-after-load 'project
  (add-to-list 'project-find-functions
               'claude-code-ide--projectile-project-root))

;; on-demand review of Claude's applied edits (working tree vs HEAD)
(defun claude-code-review-diff ()
  "Review all uncommitted changes (e.g. Claude's edits) against HEAD."
  (interactive)
  (require 'magit)
  (magit-diff-range "HEAD"))


;; ----- Auto-revert: reflect external edits (e.g. Claude Code) live -----
;; When Claude Code (or git, or any external process) writes a file on disk,
;; the visiting buffer would otherwise go stale and warn "file changed on
;; disk". Auto-revert reloads those buffers automatically.
(global-auto-revert-mode 1)

(setq global-auto-revert-non-file-buffers t) ;; also refresh dired, ibuffer, etc.
(setq auto-revert-verbose nil)               ;; no minibuffer spam on every revert
(setq auto-revert-use-notify t)              ;; use inotify -> near-instant updates
(setq auto-revert-avoid-polling t)           ;; prefer file notifications over polling
(setq auto-revert-interval 1)                ;; polling fallback: check every 1s
(setq auto-revert-check-vc-info t)           ;; keep VC/modeline state fresh too
(setq revert-without-query '(".*"))          ;; never prompt when reverting
(setq auto-revert-remote-files nil)          ;; don't hammer TRAMP/remote files

;; keep point/scroll stable across external edits (never jump or follow)
(setq switch-to-buffer-preserve-window-point t)


;; ---- Doc View ----
(with-eval-after-load 'doc-view-mode
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-page))


;; ----- pgmacs -----
(defun pgmacs-start ()
  "Prompt user for PostgreSQL connection and open it in pgmacs."
  (interactive)
  (let ((conn-string (get-postgres-connection-string)))
    (when conn-string
      (pgmacs-open-uri conn-string))))

(defun get-postgres-connection-string ()
  "Prompt user for PostgreSQL connection name and return the connection string."
  (interactive)
  (let* ((all-connections (auth-source-search :max 100)) ; Get all auth entries
         (postgres-connections 
          (seq-filter (lambda (conn)
                        (and (equal (plist-get conn :port) "postgres")
                             (or (plist-get conn :name)
                                 (string-match-p "planewise" (plist-get conn :host)))))
                      all-connections)))
    
    (message "Found %d postgres connections" (length postgres-connections))
    
    (if (null postgres-connections)
        (message "No PostgreSQL connections found in .authinfo")
      (let* ((names (mapcar (lambda (auth)
                              (or (plist-get auth :name)
                                  (plist-get auth :host)))
                            postgres-connections))
             (selected-name (completing-read "Select PostgreSQL connection: " names nil t))
             (auth (car (seq-filter (lambda (a)
                                      (or (equal selected-name (plist-get a :name))
                                          (equal selected-name (plist-get a :host))))
                                    postgres-connections))))
        (when auth
          (let ((tls-param (if (equal (plist-get auth :tls) "yes") 
                              "?sslmode=require" 
                              "")))
            (format "postgresql://%s:%s@%s/%s%s"
                   (plist-get auth :user)
                   (funcall (plist-get auth :secret))
                   (plist-get auth :host)
                   (or (plist-get auth :database) "postgres")
                   tls-param)))))))

(with-eval-after-load 'pgmacs
  (setq pgmacs-schemaspy-cmdline "docker run -v %D:/output --userns=keep-id --network=host docker.io/schemaspy/schemaspy:latest -t pgsql11 -host %h -port %P -u %u -p %p -db %d -imageformat svg")
  (define-key pgmacs-row-list-map/table (kbd "g") nil)
  (define-key pgmacs-row-list-map/table (kbd "w") 'evil-forward-word-begin)
  (define-key pgmacs-row-list-map/table (kbd "j") 'evil-next-line)
  (define-key pgmacs-row-list-map/table (kbd "k") 'evil-previous-line)
  (define-key pgmacs-row-list-map/table (kbd "h") 'evil-backward-char)
  (define-key pgmacs-row-list-map/table (kbd "l") 'evil-forward-char)
  (define-key pgmacs-row-list-map/table (kbd "s") 'pgmacs-run-sql)
  (define-key pgmacs-row-list-map/table (kbd "r") 'pgmacs-run-sql)
  (define-key pgmacs-row-list-map/table (kbd "E") 'pgmacs--table-to-csv)

  (define-key pgmacs-table-list-map/table (kbd "g") nil)
  (define-key pgmacs-table-list-map/table (kbd "w") 'evil-forward-word-begin)
  (define-key pgmacs-table-list-map/table (kbd "j") 'evil-next-line)
  (define-key pgmacs-table-list-map/table (kbd "k") 'evil-previous-line)
  (define-key pgmacs-table-list-map/table (kbd "h") 'evil-backward-char)
  (define-key pgmacs-table-list-map/table (kbd "l") 'evil-forward-char)
  (define-key pgmacs-table-list-map/table (kbd "s") 'pgmacs-run-sql)
  (define-key pgmacs-table-list-map/table (kbd "r") 'pgmacs-run-sql)
  (define-key pgmacs-table-list-map/table (kbd "E") 'pgmacs--table-to-csv))


;; ----- Language Modes -----
(require 'web-mode)
(require 'nix-mode)
(require 'solidity-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; ----- LSP Configs -----
(require 'lsp-bridge)

(add-to-list 'lsp-bridge-default-mode-hooks 'protobuf-mode-hook)

(global-lsp-bridge-mode)

(setq lsp-bridge-python-lsp-server 'pylsp)
(setq lsp-bridge-enable-hover-diagnostic t)  ;; show diagnostics in hover popups
(setq lsp-bridge-signature-help-enable t)    ;; enable signature help
(setq lsp-bridge-enable-log nil) ;; ensure logging is disabled for performance

;; use custom langserver definitions (e.g. buf for protobuf)
(setq lsp-bridge-user-langserver-dir "~/.emacs.d/langserver")

(add-to-list 'lsp-bridge-single-lang-server-mode-list '((web-mode) . "svelteserver"))
(add-to-list 'lsp-bridge-single-lang-server-mode-list '((protobuf-mode) . "buf"))

(with-eval-after-load 'lsp-bridge
  (evil-define-key 'insert lsp-bridge-mode-map
    (kbd "C-j") 'acm-select-next
    (kbd "C-k") 'acm-select-prev)
  (define-key lsp-bridge-mode-map (kbd "C-j") 'acm-select-next)
  (define-key lsp-bridge-mode-map (kbd "C-k") 'acm-select-prev)
  (define-key lsp-bridge-mode-map (kbd "C-SPC") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-S-SPC") 'lsp-bridge-find-references))

(with-eval-after-load 'acm
  (evil-define-key 'insert acm-mode-map
	(kbd "RET") 'acm-complete))

;; ----- Language Configs -----
(setq lisp-indent-offset 2)
(setq typescript-indent-level 2)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)
(setq web-mode-block-padding 0)


;; ----- Key Bindings -----

;; --- Global Bindings ---
(defun open-emacs-config ()
  "Open a file from the ~/.emacs.d/config/ directory using counsel."
  (interactive)
  (let ((default-directory (expand-file-name "~/.emacs.d/config/")))
    (counsel-find-file)))

(defun open-org-folder ()
  "Open a file from the ~/.emacs.d/org directory using counsel."
  (interactive)
  (let ((default-directory (expand-file-name "~/.emacs.d/org/")))
    (counsel-find-file)))

(defun counsel-find-file-here ()
  "Run `counsel-find-file` starting in the current buffer's directory."
  (interactive)
  (let ((default-directory
          (if buffer-file-name
              (file-name-directory buffer-file-name)
            default-directory)))  ;; fallback if no file
    (counsel-find-file)))

(defun browse-url-region-or-point ()
  "Open the highlighted region as a URL, or the URL at point, in the browser."
  (interactive)
  (if (use-region-p)
      (browse-url (string-trim
                   (buffer-substring-no-properties (region-beginning) (region-end))))
    (browse-url-at-point)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-x b") 'counsel-switch-buffer)
  (define-key evil-normal-state-map (kbd "SPC f s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC f r") 'recentf-open)
  (define-key evil-normal-state-map (kbd "SPC f p") 'open-emacs-config)
  (define-key evil-normal-state-map (kbd "SPC f o") 'open-org-folder)
  (define-key evil-normal-state-map (kbd "SPC `") 'evil-switch-to-windows-last-buffer)

  ;; --- Windows ---
  (define-key evil-normal-state-map (kbd "SPC w c") 'delete-window)
  (define-key evil-normal-state-map (kbd "M-RET") 'counsel-switch-buffer-other-window)

  (define-key evil-normal-state-map (kbd "C-<tab>") 'other-window)
  (define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x) ;; overrides default command exec
  (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)

  (define-key evil-normal-state-map (kbd "SPC b k") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "SPC q q") 'save-buffers-kill-terminal)

  (define-key evil-normal-state-map (kbd "SPC SPC") 'projectile-search-file)
  (define-key evil-normal-state-map (kbd "SPC p s") 'projectile-switch-project)
  (define-key evil-normal-state-map (kbd "SPC p c") 'projectile-switch-project)
  (define-key evil-normal-state-map (kbd "SPC p i") 'projectile-invalidate-cache)
  (define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file-here)

  (define-key evil-normal-state-map (kbd "SPC /") 'counsel-projectile-rg)
  (define-key evil-normal-state-map (kbd "SPC ?") 'projectile-ripgrep)
  (define-key evil-normal-state-map (kbd "C-c C-/") 'projectile-ripgrep)
  (define-key evil-normal-state-map (kbd "C-s") 'swiper)

  (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-line-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-line-up)

  (define-key evil-normal-state-map (kbd "C-c C-r") 'lsp-bridge-rename)
  (define-key evil-normal-state-map (kbd "C-c C-w") 'whitespace-mode)
  (define-key evil-insert-state-map (kbd "C-c C-w") 'whitespace-mode)
  (define-key evil-normal-state-map (kbd "C-/") 'comment-line)

  (define-key evil-normal-state-map (kbd "SPC o a") 'org-agenda)
  (define-key evil-normal-state-map (kbd "SPC o c") 'org-capture)
  (define-key evil-normal-state-map (kbd "SPC o j") 'org-journal-new-entry)
  (define-key evil-normal-state-map (kbd "C-c o") 'org-capture)
  (define-key evil-normal-state-map (kbd "C-c C-o") 'org-capture)
  (define-key evil-normal-state-map (kbd "SPC o t") 'org-timeblock)

  (define-key evil-normal-state-map (kbd "C-c C-s") 'claude-code-ide-send-prompt)
  (define-key evil-normal-state-map (kbd "SPC c c") 'claude-code-ide-menu)
  (define-key evil-normal-state-map (kbd "SPC c a") 'claude-code-ide-insert-at-mentioned)
  (define-key evil-visual-state-map (kbd "SPC c a") 'claude-code-ide-insert-at-mentioned)
  (define-key evil-visual-state-map (kbd "C-c C-a") 'claude-code-ide-insert-at-mentioned)
  (define-key evil-normal-state-map (kbd "SPC c r") 'claude-code-ide-resume)
  (define-key evil-visual-state-map (kbd "SPC c r") 'claude-code-ide-resume)
  (define-key evil-normal-state-map (kbd "SPC c t") 'claude-code-ide-toggle)
  (define-key evil-normal-state-map (kbd "SPC c b") 'claude-code-ide-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "SPC c l") 'claude-code-ide-list-sessions)
  (define-key evil-normal-state-map (kbd "SPC c k") 'claude-code-ide-send-escape) ;; interrupt Claude from anywhere
  ;; --- review Claude's changes via magit (current + past history) ---
  (define-key evil-normal-state-map (kbd "SPC c d") 'claude-code-review-diff)   ;; all uncommitted changes vs HEAD
  (define-key evil-normal-state-map (kbd "SPC c D") 'magit-diff-buffer-file)    ;; diff of current file
  (define-key evil-normal-state-map (kbd "SPC c h") 'magit-log-current)         ;; project commit history
  (define-key evil-normal-state-map (kbd "SPC c f") 'magit-log-buffer-file)     ;; history of current file

  (define-key evil-normal-state-map (kbd "SPC d c") 'pgmacs-start)

  (define-key evil-normal-state-map (kbd "C-c C-g") 'magit-status)
  (define-key evil-normal-state-map (kbd "C-c C-p") 'magit-pull))


;; --- Mode Bindings ---
(defun find-dir-with-file (filename &optional start-dir)
  "Find the closest ancestor directory containing FILENAME, starting from START-DIR.
If not found, search upward until reaching the home directory (~).
Returns the directory path, or nil if not found."
  (let* ((dir (expand-file-name (or start-dir default-directory)))
         (home (expand-file-name "~"))
         (found nil))
    (while (and dir (not found))
      (if (file-exists-p (expand-file-name filename dir))
          (setq found dir)
        (setq dir (if (equal dir home) nil (file-name-directory (directory-file-name dir))))))
    found))

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

;; --- Emacs Lisp / Lisp ---
(evil-define-key 'normal emacs-lisp-mode-map (kbd "C-c r") 'eval-buffer)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(evil-define-key 'normal lisp-mode-map (kbd "C-c r") 'eval-buffer)
(evil-define-key 'normal lisp-mode-map (kbd "C-c C-c") 'eval-buffer)

;; --- Go ---
(defun go-compile ()
  "Compile Go project using projectile with make build as default."
  (interactive)
  (projectile-compile-with-default "make build"))

(defun go-run ()
  "Run the current Go project with env"
  (interactive)
  (let* ((project-root (or (projectile-project-root)
                           (error "Not in a Projectile project")))
         (env-file (expand-file-name ".env" project-root)))
    (run "make" project-root)))

(with-eval-after-load 'go-mode
  (evil-define-key 'normal go-mode-map (kbd "C-c C-c") 'go-compile)
  (evil-define-key 'normal go-mode-map (kbd "C-c C-x") 'go-run))

(add-hook 'go-mode-hook
  (lambda ()
	(add-hook 'before-save-hook 'gofmt-before-save nil t)))


;; --- Elixir ---
(defun elixir-compile ()
  "Compile Elixir project using projectile with make build as default."
  (interactive)
  (projectile-compile-with-default "make build"))

(with-eval-after-load 'elixir-mode
  (evil-define-key 'normal elixir-mode-map (kbd "C-c C-c") 'elixir-compile))


;; --- TypeScript ---
(defun ts-compile ()
  "Compile the current TypeScript project."
  (interactive)
  (projectile-compile-with-default "yarn build"))

(defun ts-run ()
  "Run a development server for the current TypeScript project."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                        (error "Not in a Projectile project"))))
    (cd project-root)
    (run "yarn dev" project-root)))

(with-eval-after-load 'typescript-mode
  (evil-define-key 'normal typescript-mode-map (kbd "C-c C-c") 'ts-compile)
  (evil-define-key 'normal typescript-mode-map (kbd "C-c C-x") 'ts-run))


;; --- Web ---
(defun web-compile ()
  "Compile the current TypeScript project."
  (interactive)
  (projectile-compile-with-default "yarn build"))

(defun web-run ()
  "Run a development server for the current TypeScript project."
  (interactive)
  (let ((project-root (or (projectile-project-root)
                        (error "Not in a Projectile project"))))
    (cd project-root)
    (run "yarn dev" project-root)))

(with-eval-after-load 'web-mode
  (evil-define-key 'normal web-mode-map (kbd "C-c C-c") 'web-compile)
  (evil-define-key 'normal web-mode-map (kbd "C-c C-x") 'web-run))


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
  (projectile-compile-with-default "mvn package"))

(with-eval-after-load 'java-mode
  (evil-define-key 'normal java-mode-map (kbd "C-c C-w") 'whitespace-mode)
  (evil-define-key 'normal java-mode-map (kbd "C-c C-c") 'java-compile))


;; --- C / C++ ---
(defcustom c-compile-command "make -k" "Command run by `c-compile'." :type 'string)

(defun c-compile ()
  "Compile from the nearest Makefile upward, using the standard *compilation* buffer."
  (interactive)
  (projectile-compile-with-default "make -k"))

(with-eval-after-load 'cc-mode
  (evil-define-key 'normal c-mode-map (kbd "C-c C-c") 'c-compile))

(with-eval-after-load 'c++-mode
  (evil-define-key 'normal c++-mode-map (kbd "C-c C-c") 'c-compile))


;; --- LaTeX ---
(defun latex-compile ()
  (interactive)
  (compile (concat "pdflatex " buffer-file-name " && rm -rf *.log *.aux *.out")))

(defun latex-open ()
  (interactive)
  (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))

(with-eval-after-load 'tex-mode
  (evil-define-key 'normal tex-mode-map (kbd "C-c C-c") 'latex-compile)
  (evil-define-key 'normal tex-mode-map (kbd "C-c C-x") 'latex-open))


;; --- Solidity ---
(defun solidity-compile ()
  "Compile the current Solidity file using solc."
  (interactive)
  (let* ((file (buffer-file-name))
         (default-directory (or (locate-dominating-file file ".git") default-directory)))
    (when file
      (compile (format "solc %s" (file-relative-name file default-directory))))))

(with-eval-after-load 'solidity-mode
  (evil-define-key 'normal solidity-mode-map (kbd "C-c C-c") 'solidity-compile))


;; --- Terraform ---
(with-eval-after-load 'terraform-mode
  (setq terraform-format-on-save t))


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

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "^") 'evil-beginning-of-line)
  (define-key magit-mode-map (kbd "w") 'evil-forward-word-begin)
  (define-key magit-mode-map (kbd "l") 'evil-forward-char)
  (define-key magit-mode-map (kbd "h") 'evil-backward-char)
  (define-key magit-mode-map (kbd "P") 'magit-pull)
  (define-key magit-mode-map (kbd "L") 'magit-log)
  (define-key magit-mode-map (kbd "C") 'magit-commit)
  (define-key magit-mode-map (kbd "C-<tab>") 'other-window)
  (define-key magit-mode-map (kbd "H") 'magit-dispatch))


;; Ivy
(with-eval-after-load 'ivy
  (dolist (map '(ivy-minibuffer-map
                 ivy-switch-buffer-map
                 ivy-reverse-i-search-map))
	(define-key (symbol-value map) (kbd "C-j") 'ivy-next-line)
    (define-key (symbol-value map) (kbd "C-k") 'ivy-previous-line)
    (define-key (symbol-value map) (kbd "C-d") 'minibuffer-keyboard-quit)))


;; Org
(defun org-refile-not-into-todo ()
  "Exclude TODO headings from refile targets."
  (let ((todo (nth 2 (org-heading-components))))
    (not (and todo (member todo org-todo-keywords-1)))))

(with-eval-after-load 'org
  ;; calendar navigation bindings
  (dolist (kv '(("C-h" . (calendar-backward-day 1))
                 ("C-l" . (calendar-forward-day 1))
                 ("C-k" . (calendar-backward-week 1))
                 ("C-j" . (calendar-forward-week 1))
                 ("M-h" . (calendar-backward-month 1))
                 ("M-l" . (calendar-forward-month 1))
                 ("M-k" . (calendar-backward-year 1))
                 ("M-j" . (calendar-forward-year 1))))
    (define-key org-read-date-minibuffer-local-map (kbd (car kv))
      `(lambda () (interactive) (org-eval-in-calendar ',(cdr kv)))))

  ;; org mode bindings
  (evil-define-key* 'normal org-mode-map (kbd "SPC p o") 'org-latex-export-to-pdf)

  (evil-define-key* '(normal insert) org-mode-map (kbd "C-SPC") 'org-open-at-point)
  (evil-define-key* 'normal org-mode-map (kbd "TAB") 'org-cycle)

  (evil-define-key* 'normal org-mode-map (kbd "C-c C-r") 'org-refile)
  (evil-define-key* 'normal org-mode-map (kbd "C-c C-a") 'org-archive-subtree)
  (define-key org-mode-map (kbd "C-c C-a") 'org-archive-subtree)
  (define-key org-mode-map (kbd "C-c C-r") 'org-refile))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-directory "~/.emacs.d/org")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-agenda-skip-unavailable-files 't)
(setq org-agenda-show-future-repeats nil)
(setq org-archive-location (expand-file-name "archive/%s::" org-directory))
(setq org-refile-targets
  `((,(expand-file-name "agenda.org" org-directory) :maxlevel . 2)
     (,(expand-file-name "inbox.org" org-directory) :maxlevel . 1)))
(setq org-refile-target-verify-function 'org-refile-not-into-todo)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-hide-drawer-startup t)

;; Capture
(defun org-capture-init ()
  (setq header-line-format
    (list
      "Org Capture – "
      "finish '" (propertize "C-c C-c" 'face 'help-key-binding) "', "
      "refile '" (propertize "C-c C-r" 'face 'help-key-binding) "', "
      "cancel '" (propertize "C-c C-k" 'face 'help-key-binding) "'"))
  (evil-define-key* 'normal org-capture-mode-map (kbd "SPC b k") 'org-capture-kill))

(add-hook 'org-capture-mode-hook 'org-capture-init)
(setq org-capture-templates
  `(
	;; Quick task (deadline with no offset; will prompt for date)
	("t" "Task" entry
	  (file+headline ,(expand-file-name "inbox.org" org-directory) "Tasks")
	  "** TODO %?\nDEADLINE: %t\n"
	  :empty-lines 1)

	 ;; Habit
	("h" "Habit" entry
	  (file+headline ,(expand-file-name "inbox.org" org-directory) "Habits")
	  "** TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:STYLE:    habit\n:END:"
	  :empty-lines 1)

	;; Event
	("e" "Event" entry
	  (file+headline ,(expand-file-name "inbox.org" org-directory) "Event")
	  "** %?\n<%<%Y-%m-%d %a> 09:00-17:00>\n"
	  :empty-lines 1)

	;; Note (with backlink to where you were)
	("n" "Note" entry
	  (file+headline ,(expand-file-name "inbox.org" org-directory) "Notes")
	  "** %?\n:CREATED: %U\n%a"
	  :empty-lines 1)

	;; Idea (plain, no backlink)
	("i" "Idea" entry
	  (file+headline ,(expand-file-name "inbox.org" org-directory) "Ideas")
	  "** %? :idea:\n:CREATED: %U\n"
	  :empty-lines 1)

	;; Office visit
    ("o" "Office visit" plain
      (file+olp ,(expand-file-name "office.org" org-directory) "Office Visits" "Office Visit")
      "- <%<%Y-%m-%d %a 09:00-17:00>>"
      :immediate-finish t
      :empty-lines 0)

	;; Grocery list
	("g" "Grocery list" entry
	  (file+headline ,(expand-file-name "groceries.org" org-directory) "Groceries")
	  "** TODO Get Groceries :grocery:\nSCHEDULED: %t\n\n*** Whole Foods\t:store:wholefoods:\n- [ ] Spinach\n- [ ] Arugula\n- [ ] Feta Cheese%?"
	  :empty-lines 1)))

;; Habit
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-habit-graph-column 80)

;; Journal
(setq org-journal-dir (expand-file-name "journal" org-directory))
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-file-format "%Y%m%d.org")

(require 'org-journal)

;; Timeblock
(with-eval-after-load 'org-timeblock
  (evil-define-key 'normal org-timeblock-mode-map (kbd "h") 'org-timeblock-backward-column)
  (evil-define-key 'normal org-timeblock-mode-map (kbd "l") 'org-timeblock-forward-column)
  (evil-define-key 'normal org-timeblock-mode-map (kbd "j") 'org-timeblock-forward-block)
  (evil-define-key 'normal org-timeblock-mode-map (kbd "k") 'org-timeblock-backward-block)

  (evil-define-key 'normal org-timeblock-mode-map (kbd "C-h") 'org-timeblock-day-earlier)
  (evil-define-key 'normal org-timeblock-mode-map (kbd "C-l") 'org-timeblock-day-later)

  (evil-define-key 'normal org-timeblock-mode-map (kbd "g r") 'org-timeblock-redraw-buffers))
