;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 's)
(require 'compile)
(require 'lsp-java)

(load-theme 'doom-henna t)

;; (setq doom-font (font-spec :family "Source Code Pro" :size 12))
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#3e4446")
;; (set-face-foreground 'highlight nil)

;; (add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'java-mode-hook #'lsp)

;; (setq org-roam-database-connector 'sqlite-builtin)
(setq default-tab-width 2)
(setq projectile-enable-caching nil)
(setq flycheck-disabled-checkers t)
(setq lsp-enable-links nil)

(setq user-full-name "Prayuj Tuli"
      user-mail-address "prayujtuli@hotmail.com")

(setq display-line-numbers-type t)
(setq evil-escape-key-sequence "qw")

(if (string= system-type "darwin")
    (toggle-frame-fullscreen)
  (setq copilot-node-executable "/home/prayuj/.nvm/versions/node/v17.9.1/bin/node")
)

(elcord-mode)

;; (if (string= system-type ";; gnu/linux")
    ;; (setq copilot-node-executable "/home/prayuj/.nvm/versions/node/v17.9.1/bin/node")
    ;; (toggle-frame-maximized)
;; )

;; Org Agenda Configuration

(setq org-directory "~/iCloud/org/")
(setq org-agenda-sorting-strategy
       '((agenda timestamp-up time-up priority-down category-keep habit-down)
       (todo time-up priority-down category-keep)
       (tags time-up priority-down category-keep)
       (search category-keep))
)
;; (setq org-priority-default 10)
;; (setq org-priority-lowest 64)
;; (setq org-priority-highest 1)

;; (defun emacs-startup-org ()
;;   "Display the weekly org-agenda and all todos."
;;   (org-agenda nil "n"))
;; (add-hook 'emacs-startup-hook #'emacs-startup-org)

;; ---- compilation and run commands ----

(defun code-compile ()
  (interactive)
  (let ((file (file-name-nondirectory buffer-file-name)))
        (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h")) (+make/run))
              ((or (equal (file-name-extension file) "java") (equal (file-name-extension file) "gradle")) (gradle-build))
              ;; ((or (equal (file-name-extension file) "org")) (insert-current-date))
              ((equal (file-name-extension file) "tex") (compile (concat "pdflatex " buffer-file-name "; rm *.log *.aux *.out;"))))))

(defun code-run ()
  (interactive)
  (let ((file (file-name-nondirectory buffer-file-name)))
        (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h") (equal (file-name-extension file) "hpp")) (shell-command "./main"))
                ((or (equal (file-name-extension file) "java") (equal (file-name-extension file) "gradle")) (gradle-test (read-string "Enter Test Names: ")))
                ((equal (file-name-extension file) "tex") (code-compile) (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))
                ((equal (file-name-extension file) "py") (shell-command (concat "python3 " buffer-file-name)))
                ((or (equal (file-name-extension file) "js") (equal (file-name-extension file) "svelte")) (shell-command "npm run start")))))


;; ---- gradle variables and commands ----

(defcustom gradle-executable-path (executable-find "gradle")
  "String representation of the Gradle executable location."
  :group 'gradle
  :type 'string)

(defcustom gradle-gradlew-executable "./gradlew"
  "String representation of the gradlew executable."
  :group 'gradle
  :type 'string)

(defcustom gradle-use-gradlew t
  "Use gradlew or `gradle-executable-path'."
  :group 'gradle
  :type 'boolean)

(defun gradle-is-project-dir (dir)
  (let ((dirname (file-name-nondirectory
        (directory-file-name (expand-file-name dir)))))
    (or (file-exists-p (expand-file-name "build.gradle" dir))
        (file-exists-p (expand-file-name
                (concat dirname ".gradle") dir)))))

(defun gradle-is-gradlew-dir (dir)
  (file-exists-p (expand-file-name "gradlew" dir)))

(defun gradle-kill-compilation-buffer ()
  (progn
    (if (get-buffer "*compilation*")
        (progn
                (delete-windows-on (get-buffer "*compilation*"))
                (kill-buffer "*compilation*")))))

(defun gradle-run-from-dir (is-dir)
  (locate-dominating-file default-directory is-dir))

(defun gradle-run (gradle-tasks)
  (gradle-kill-compilation-buffer)
  (let ((default-directory
        (gradle-run-from-dir (if gradle-use-gradlew
                'gradle-is-gradlew-dir
                'gradle-is-project-dir))))
    (compile (gradle-make-command gradle-tasks))))

(defun gradle-make-command (gradle-tasks)
  (let ((gradle-executable (if gradle-use-gradlew
        gradle-gradlew-executable
        gradle-executable-path)))
    (s-join " " (list gradle-executable gradle-tasks))))

(defun gradle-execute (tasks)
  (interactive "sType tasks to run: ")
  (gradle-run
   (s-concat tasks " --daemon")))

(defun gradle-build ()
  (interactive)
  (gradle-run "build --daemon"))

(defun gradle-test (test-name)
  (interactive)
  (gradle-run
   (s-concat "test --tests " test-name " --daemon")))


;; ---- miscellanious ----

(defun generate-makefile ()
  (interactive)
  (let (makefile_directory)
    (setq makefile_directory (read-directory-name "Directory:"))
    (message "Generated makefile in %s" makefile_directory)
    (f-write-text "CC = g++\nCFLAGS  = -w -g\n\nTARGET = main\nSOURCES := $(shell find . -name '*.cpp')\nall:\n\t$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES)\nclean:\n\trm $(TARGET)"
                  'utf-8 (concat makefile_directory "/Makefile"))
  )
)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


;; new keybindings
;; (define-key java-mode-map (kbd "C-c C-c") 'code-compile)
;; (define-key cpp-mode-map (kbd "C-c C-c") 'code-compile)
;; (define-key tex-mode-map (kbd "C-c C-c") 'code-compile)
;; (define-key python-mode-map (kbd "C-c C-c") 'code-compile)

(map! :map general-override-mode-map "C-c C-c" 'code-compile)

(map! :map general-override-mode-map "C-c C-x" 'code-run)
(map! :map general-override-mode-map "C-c C-g" 'generate-makefile)
(map! :map general-override-mode-map "C-/" 'comment-line)
;; (map! :map general-override-mode-map "C-c C-d" 'insert-current-date)
(map! :map general-override-mode-map "M-t" 'shell)
(map! :map general-override-mode-map "M-T" 'shell-command)

(map! :map general-override-mode-map "M-l" 'evil-window-right)
(map! :map general-override-mode-map "M-h" 'evil-window-left)
(map! :map general-override-mode-map "M-j" 'evil-window-down)
(map! :map general-override-mode-map "M-k" 'evil-window-up)
(map! :map general-override-mode-map "M-w" 'evil-window-delete)

(map! :map general-override-mode-map "M-RET" 'evil-window-vsplit)

(global-set-key (kbd "C-c o")
        (lambda () (interactive) (find-file "~/iCloud/org/todo.org")))

;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;          ("C-<tab>" . 'copilot-accept-completion-by-word)
;;          :map copilot-completion-map
;;          ("<tab>" . 'copilot-accept-completion)
;;          ("TAB" . 'copilot-accept-completion)))

;; (use-package! lsp-mode
;;   ;; Need to repeat this line from the Doom lsp module
;;   ;; declaration to keep it from eager-loading:
;;   :commands lsp-install-server

;;   :config
;;   (setq
;;         lsp-diagnostics-provider :none
;;         lsp-ui-sideline-enable nil
;;         lsp-modeline-diagnostics-enable nil))
