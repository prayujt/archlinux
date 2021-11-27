;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Prayuj Tuli"
      user-mail-address "prayujtuli@hotmail.com")

(setq doom-theme 'doom-one)

(setq org-directory "~/iCloud/org/")

(setq display-line-numbers-type t)
(setq evil-escape-key-sequence "qw")

(if (string= system-type "darwin")
    (toggle-frame-fullscreen)
  ()
)

(defun code-compile ()
  (interactive)
  (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
         (format "%s"
                 (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h") (equal (file-name-extension file) "c") (equal (file-name-extension file) "hpp")) "make all")
                       ((equal (file-name-extension file) "java") (concat "javac " buffer-file-name))))))
  (compile compile-command))

(defun code-run ()
  (interactive)
  (set (make-local-variable 'run-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
         (format "%s"
                 (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h") (equal (file-name-extension file) "hpp")) "./main")
                       ((equal (file-name-extension file) "java") (concat "java " buffer-file-name))
                       ((equal (file-name-extension file) "py") (concat "python3 " buffer-file-name))
                       ((or (equal (file-name-extension file) "js") (equal (file-name-extension file) "svelte")) "npm run start")))))
  (shell-command run-command))

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
(map! :map general-override-mode-map "C-c C-c" 'code-compile)
(map! :map general-override-mode-map "C-c C-x" 'code-run)
(map! :map general-override-mode-map "C-c C-g" 'generate-makefile)
(map! :map general-override-mode-map "C-/" 'comment-line)
(map! :map general-override-mode-map "C-c C-d" 'insert-current-date)
(map! :map general-override-mode-map "M-t" 'shell)
(map! :map general-override-mode-map "M-T" 'shell-command)

(map! :map general-override-mode-map "M-l" 'evil-window-right)
(map! :map general-override-mode-map "M-h" 'evil-window-left)
(map! :map general-override-mode-map "M-j" 'evil-window-down)
(map! :map general-override-mode-map "M-k" 'evil-window-up)
(map! :map general-override-mode-map "M-w" 'evil-window-delete)

;; (map! :map general-override-mode-map "M-L" 'move-window-right)
;; (map! :map general-override-mode-map "M-H" 'move-window-left)
;; (map! :map general-override-mode-map "M-J" 'move-window-down)
;; (map! :map general-override-mode-map "M-K" 'move-window-up)

(map! :map general-override-mode-map "M-RET" 'evil-window-vsplit)

;; (add-hook 'c++-mode-hook
;;           (lambda () (local-set-key (kbd "C-0") #'run-latexmk)))
