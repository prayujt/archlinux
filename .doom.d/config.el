;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'compile)
(require 'dashboard)
(load-theme 'doom-snazzy t)

;; (setq doom-font (font-spec :family "Source Code Pro" :size 12))
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#3e4446")
;; (set-face-foreground 'highlight nil)

(elcord-mode)

(add-hook 'prog-mode-hook 'copilot-mode)

(setq default-tab-width 2)
(setq-default tab-width 2)

(setq user-full-name "Prayuj Tuli"
      user-mail-address "prayujtuli@hotmail.com")

(setq display-line-numbers-type t)
(setq evil-escape-key-sequence "qw")

(if (string= system-type "darwin")
    (progn
      (toggle-frame-fullscreen)
      (setq copilot-node-executable "/Users/prayuj/.nvm/versions/node/v16.20.1/bin/node")))

(use-package lsp-mode
  :hook ((prog-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))))

;;
;; (use-package tramp
;;   :ensure nil
;;   :custom
;;   (setq tramp-default-method "ssh"))

(if (string= system-type ";; gnu/linux")
    (progn
        (setq copilot-node-executable "/home/prayuj/.nvm/versions/node/v17.9.1/bin/node")
        (toggle-frame-maximized)))

;; Org Agenda Configuration

(setq org-directory "~/iCloud/org/")

;; (setq org-agenda-custom-commands
;;         '(("n" "Agenda for Today"
;;            ((agenda "" ((org-agenda-span 1) (org-agenda-start-day "-0d")))))))
                ;; ((org-ql-block '(or
                ;;         (tags "classes")
                ;;         (planning 'today))
                ;; ((org-ql-block-header "Agenda for Today")))))))

(setq org-agenda-sorting-strategy
       '((agenda timestamp-up time-up priority-down category-keep habit-down)
       (todo time-up priority-down category-keep)
       (tags time-up priority-down category-keep)
       (search category-keep))
)

(setq org-todo-keywords
      '(
        (sequence "TODO(t)" "PROJ(p)" "WAITING(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "SOMEDAY(s)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

(setq org-deadline-warning-days 7)
(setq org-scheduled-delay-days 7)
;; (setq org-agenda-skip-scheduled-if-done t)

(setq org-super-agenda-groups
       '(
         (:name "Important"
                :tag "bills"
                :priority "A"
                :order 1
                :transformer (--> it (propertize it 'face ' (:foreground "OrangeRed"))))

         (:name "Today"
                :order 2
                :transformer (--> it (propertize it 'face ' (:foreground "DarkOrange"))))

        (:name "Overdue"
                :deadline past
                :order 2
                :transformer (--> it (propertize it 'face ' (:foreground "Red"))))

        (:name "Due Soon"
                :deadline future
                :transformer (--> it (propertize it 'face ' (:foreground "Green")))
                :order 3)

        (:name "Upcoming Exams"
                :tag "exams"
                :transformer (--> it (propertize it 'face ' (:foreground "Yellow")))
                :order 4)

        (:name "Class Work"
                :tag "class"
                :order 5)

        (:name "Edugator Work"
                :tag "edugator"
                :order 6)

        (:name "Personal"
                :habit t
                :tag "personal"
                :order 7)

        (:name "Clubs"
                :tag "clubs"
                :order 7)

        (:name "Class Schedule"
                :tag "classes"
                :order 8)

        (:name "In Progress"
                :todo "WAITING"
                :order 9)

        (:name "Someday"
                :todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                :order 10)

        (:name "Other work"
                :priority<= "B"
                :order 11)
))

(org-super-agenda-mode)

;; ---- org dashboard ----
(setq dashboard-banner-logo-title 'nil)
(setq dashboard-startup-banner 2)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))
(setq dashboard-set-footer nil)
;; (setq dashboard-agenda-sort-strategy 'time-up)

(dashboard-setup-startup-hook)


;; ---- lsp setup ----

(setq lsp-pylsp-plugins-flake8-ignore ["E501"])
(setq lsp-warn-no-matched-clients 'nil)


;; ---- compilation and run commands ----

(defun latex-compile ()
  (interactive)
  (compile (concat "pdflatex " buffer-file-name " && rm -rf *.log *.aux *.out")))

(defun latex-open ()
  (interactive)
        (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))

(defun go-compile ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "go.mod")))

  ;; (if (-non-nil (string-match "_test.go" (file-name-nondirectory buffer-file-name)))
  (if (string-match "_test.go" (file-name-nondirectory buffer-file-name))
      (compile "go test -v ./...")
  (compile "go get; go build; sudo rm -rf ~/go"))

  (cd (file-name-directory buffer-file-name)))

(defun go-run ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "go.mod")))

  ;; (if (-non-nil (string-match "_test.go" (file-name-nondirectory buffer-file-name)))
  (if (string-match "_test.go" (file-name-nondirectory buffer-file-name))
      (compile "go test -v ./...")
  (compile "go get; go build; sudo rm -rf ~/go"))

  (cd (file-name-directory buffer-file-name)))

(defun ts-compile ()
  (interactive)
  (compile "npm run build"))

(defun ts-run ()
  (interactive)
  (compile "npm run dev"))

(defun python-compile ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "setup.py")))
  (compile "pip install .")
  (cd (file-name-directory buffer-file-name)))

(defun cpp-compile ()
  (interactive)
  (cd (file-name-directory (find-file-in-heirarchy "./" "Makefile")))
  (compile "make")
  (cd (file-name-directory buffer-file-nam)))

(defun rust-compile ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "Cargo.toml")))
  (compile "cargo build")
  (cd (file-name-directory buffer-file-name)))

(defun rust-run ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "Cargo.toml")))
  (compile "cargo run")
  (cd (file-name-directory buffer-file-name)))

(defun rust-check ()
  (interactive)

  (cd (file-name-directory (find-file-in-heirarchy "./" "Cargo.toml")))
  (compile "cargo check")
  (cd (file-name-directory buffer-file-name)))

;; (defun code-compile ()
;;   (interactive)
;;   (let ((file (file-name-nondirectory buffer-file-name)))
;;         (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h")) (+make/run))
;;               ((or (equal (file-name-extension file) "java") (equal (file-name-extension file) "gradle")) (gradle-build))
;;               ;; ((or (equal (file-name-extension file) "org")) (org-babel-execute-src-block))
;;               ((equal (file-name-extension file) "tex") (compile (concat "pdflatex " buffer-file-name "; rm *.log *.aux *.out;"))))))

;; (defun code-run ()
;;   (interactive)
;;   (let ((file (file-name-nondirectory buffer-file-name)))
;;         (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h") (equal (file-name-extension file) "hpp")) (shell-command "./main"))
;;                 ((or (equal (file-name-extension file) "java") (equal (file-name-extension file) "gradle")) (gradle-test (read-string "Enter Test Names: ")))
;;                 ((equal (file-name-extension file) "tex") (code-compile) (if (string= system-type "darwin") (shell-command (concat "open " (file-name-sans-extension buffer-file-name) ".pdf")) (TeX-view)))
;;                 ((equal (file-name-extension file) "py") (shell-command (concat "python3 " buffer-file-name)))
;;                 ((or (equal (file-name-extension file) "js") (equal (file-name-extension file) "svelte")) (shell-command "npm run start")))))


;; ---- projectile ----
;; (after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
          ;; projectile-project-root-files-bottom-up)))


;; ---- miscellanious ----

;; default project creation entry point
(defun create-project ()
  (interactive)
  (let (choice)
    (let ((choices '("C++" "Rust")))
    (setq choice (completing-read "New Project:" choices)))
    (cond
      ((equal choice "C++") (init-cpp-project))
      ((equal choice "Rust") (init-cargo-project)))
))

(defun init-cpp-project ()
  (interactive)
    (setq project_directory (read-directory-name "Directory: "))
    (if (file-directory-p project_directory) () (make-directory project_directory))
    (cd project_directory)
    (create-cpp-files project_directory))

(defun create-cpp-files (project_directory)
    (f-write-text "CC = g++\nINCLUDES = \nCFLAGS = -w -g\n\nTARGET = main\nSOURCES := $(shell find . -name '*.cpp')\nall:\n\t$(CC) $(SOURCES) $(CFLAGS) $(INCLUDES) -o $(TARGET)\nclean:\n\trm $(TARGET)\n"
                  'utf-8 (concat project_directory "/Makefile"))
    (if (file-exists-p (concat project_directory "/main.cpp")) ()
      (f-write-text "using namespace std;\n\nint main() {\n\treturn 0;\n}"
                'utf-8 (concat project_directory "/main.cpp")))
    (message "Initialized new C++ project in %s" project_directory))

(defun init-cargo-project ()
  (interactive)
    (setq project_directory (read-directory-name "Directory: "))
    (if (file-directory-p project_directory) () (make-directory project_directory))
    (setq project_name (read-string "Enter Project Name: "))
    (cd project_directory)
    (compile (format "cargo new %s --vcs=\"none\"" project_name))
    (message "Initialized new Rust project in %s" project_directory)
)

(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-file-in-heirarchy (current-dir fname)
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (find-file-in-heirarchy parent fname)))))

(defun ts-prettier-write ()
  (shell-command (concat "npx prettier --write " buffer-file-name)))



;; compilation keybindings

(add-hook 'java-mode-hook
  (lambda ()
    (define-key java-mode-map (kbd "C-c C-c") 'gradle-build)
    (flycheck-mode nil)))

(add-hook 'c-mode-hook
  (lambda ()
    (define-key c-mode-map (kbd "C-c C-c") 'cpp-compile)))

(add-hook 'c++-mode-hook
  (lambda ()
    (define-key c++-mode-map (kbd "C-c C-c") 'cpp-compile)))

(add-hook 'latex-mode-hook
  (lambda ()
    (define-key latex-mode-map (kbd "C-c C-c") 'latex-compile)
    (define-key latex-mode-map (kbd "C-c C-x") 'latex-open)))

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (define-key LaTeX-mode-map (kbd "C-c C-c") 'latex-compile)
    (define-key LaTeX-mode-map (kbd "C-c C-x") 'latex-open)))

(add-hook 'tex-mode-hook
  (lambda ()
    (define-key tex-mode-map (kbd "C-c C-c") 'latex-compile)
    (define-key tex-mode-map (kbd "C-c C-x") 'latex-open)))

(add-hook 'ein:notebook-mode-hook
  (lambda ()
    (define-key ein:notebook-mode-map (kbd "C-c j") 'ein:worksheet-goto-next-input)
    (define-key ein:notebook-mode-map (kbd "C-c k") 'ein:worksheet-goto-prev-input)
    (define-key ein:notebook-mode-map (kbd "C-c C-j") 'ein:worksheet-insert-cell-below)
    (define-key ein:notebook-mode-map (kbd "C-c C-k") 'ein:worksheet-insert-cell-above)
    (define-key ein:notebook-mode-map (kbd "C-c C-d") 'ein:worksheet-delete-cell)
    (define-key ein:notebook-mode-map (kbd "C-c C-r") 'ein:notebook-restart-session-command)
    (define-key ein:notebook-mode-map (kbd "C-c C-<backspace>") 'ein:worksheet-clear-output)))

(add-hook 'go-mode-hook
  (lambda ()
    (define-key go-mode-map (kbd "C-c C-c") 'go-compile)
    (define-key go-mode-map (kbd "C-c C-x") 'projectile-run-project)))

(add-hook 'typescript-mode-hook
  (lambda ()
    (setq lsp-before-save-edits nil)
    (add-hook 'after-save-hook 'ts-prettier-write)
    (define-key typescript-mode-map (kbd "C-c C-c") 'ts-compile)
    (define-key typescript-mode-map (kbd "C-c C-x") 'projectile-run-project)))

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "C-c C-c") 'python-compile)
    (define-key python-mode-map (kbd "C-c C-x") 'projectile-run-project)))

(setq rust-format-on-save t)
(add-hook 'rustic-mode-hook
  (lambda ()
    (define-key rustic-mode-map (kbd "C-c C-c") 'rust-compile)
    (define-key rustic-mode-map (kbd "C-c C-x") 'rust-run)
    (define-key rustic-mode-map (kbd "C-c C-v") 'rust-check)))

(add-hook 'org-mode-hook
 (lambda ()
   (define-key org-mode-map (kbd "C-c C-c") 'org-icalendar-export-to-ics)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'doom/reload)))

(add-hook 'org-agenda-mode-hook
  (lambda ()
    (define-key org-super-agenda-header-map (kbd "k") nil)
    (define-key org-super-agenda-header-map (kbd "j") nil)
    (define-key org-super-agenda-header-map (kbd "l") nil)
    (define-key org-super-agenda-header-map (kbd "h") nil)

    (define-key org-agenda-mode-map (kbd "k") 'org-agenda-next-item)
    (define-key org-agenda-mode-map (kbd "j") 'org-agenda-previous-item)

    (define-key org-super-agenda-header-map (kbd "C-h") 'org-agenda-earlier)
    (define-key org-super-agenda-header-map (kbd "C-l") 'org-agenda-later)

    (define-key org-agenda-mode-map (kbd "C-h") 'org-agenda-earlier)
    (define-key org-agenda-mode-map (kbd "C-l") 'org-agenda-later)))

;; (map! :map general-override-mode-map "C-c C-c" 'code-compile)
;; (map! :map general-override-mode-map "C-c C-x" 'code-run)

;; vertico mode hooks
(add-hook 'vertico-mode-hook
  (lambda ()
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)))


(map! :map general-override-mode-map "C-c e l" 'flycheck-list-errors)
(map! :map general-override-mode-map "C-c C-n" 'create-project)
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
        (lambda () (interactive) (find-file "~/iCloud/org/calendar.org")))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

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

;; ---- ein configuration ---

;; (setq ein:jupyter-server-args '("--ip" "127.0.0.1"))

;; (defun insert-jupyter-json ()
;;         (interactive "*")
;;         (insert
;;                 "{
;;                         \"cells\": [
;;                                 {
;;                                         \"cell_type\": \"code\",
;;                                         \"execution_count\": null,
;;                                         \"id\": \"b727f3c4\",
;;                                         \"metadata\": {},
;;                                         \"outputs\": [],
;;                                         \"source\": []
;;                                 }
;;                         ],
;;                         \"metadata\": {
;;                                 \"kernelspec\" {
;;                                         \"display_name\": \"Python 3 (ipykernel)\",
;;                                         \"language\": \"python\",
;;                                         \"name\": \"python3\"
;;                                 },
;;                                 \"language_info\": {
;;                                         \"codemirror_mode\": {
;;                                                 \"name\": \"ipython\",
;;                                                 \"version\": 3
;;                                         },
;;                                         \"file_extension\": \"py\",
;;                                         \"mimetype\": \"text/x-python\",
;;                                         \"name\": \"python\",
;;                                         \"nbconvert_exporter\": \"python\",
;;                                         \"pygments_lexer\": \"ipython3\",
;;                                         \"version\": \"3.10.9\"
;;                                 }
;;                         },
;;                         \"nbformat\": 4,
;;                         \"nbformat_minor\": 5
;;                 }"))
