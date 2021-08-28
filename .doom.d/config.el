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

(defun code-compile-run ()
  (interactive)
  (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
         (format "%s"
                 (cond ((or (equal (file-name-extension file) "cpp") (equal (file-name-extension file) "h") (equal (file-name-extension file) "hpp")) "make all && ./main")
                       ((equal (file-name-extension file) "js") "npm run start")))))
  (compile compile-command))

(defun generate-makefile ()
  (interactive)
  (let (makefile_directory)
    (setq makefile_directory (read-directory-name "Directory:"))
    (message "Generated makefile in %s" makefile_directory)
    (f-write-text "CC = g++\nCFLAGS  = -g -Wall\n\nTARGET = main\nSOURCES := $(shell find . -name '*.cpp')\nall:\n\t$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES)\nclean:\n\trm $(TARGET)"
                  'utf-8 (concat makefile_directory "/Makefile"))
  )
)

(global-set-key (kbd "C-c C-x") 'code-compile-run)
(global-set-key (kbd "C-c C-g") 'generate-makefile)
