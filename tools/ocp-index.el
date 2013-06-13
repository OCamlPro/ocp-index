;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-index

(require 'auto-complete)

;; Customize defs

(defgroup ocp-index nil
  "ocp-index OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-index-path "ocp-index"
  "*Path to access the ocp-index command"
  :group 'ocp-index :type '(file))

(defcustom ocp-index-options ""
  "*Command-line parameters to add to ocp-index invocations (ex. --show=types)"
  :group 'ocp-index :type 'string)

(defcustom ocp-index-override-auto-complete-defaults t
  "*If set, auto-complete defaults will be reset to a sane setting in ocaml
   buffers. Disable if you prefer to configure auto-complete yourself."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-extra-completion-sources
  (list 'ac-source-words-in-same-mode-buffers)
  "*Completion sources to enable besides ocp-index completion"
  :group 'ocp-index :type '(repeat symbol))

;; Completion

(defvar ac-ocp-index-current-doc nil)

(defun ocp-index-bounds-of-symbol-at-point ()
  "Matches the fully qualified identifier at point, eg [M1.M2.someval] but
   also somerecord.[M1.M2.somefield]"
  (let ((case-fold-search nil))
    (save-excursion
      (while (looking-back "\\<\\([A-Z][a-zA-Z0-9_']*\.\\)*[a-zA-Z0-9_']*"
                           (line-beginning-position) nil)
        (goto-char (match-beginning 0)))
      (when (looking-at "[a-zA-Z0-9_'.]*[a-zA-Z0-9_']")
        (cons (match-beginning 0) (match-end 0))))))

(defun ocp-index-completion-prefix-start ()
  (car-safe (ocp-index-bounds-of-symbol-at-point)))

(defun ocp-index-completion-prefix ()
  (let ((bounds (ocp-index-bounds-of-symbol-at-point)))
    (when bounds
      (buffer-substring (car bounds) (cdr bounds)))))

;; override default prefix definition
(defun ac-prefix-symbol ()
  (ocp-index-completion-prefix-start))

(defun ocp-index-cmd (cmd arg)
  (let ((current-module (upcase-initials
                         (file-name-nondirectory
                          (file-name-sans-extension
                           (buffer-file-name))))))
    (format "%s %s %s -O %s %s"
            ocp-index-path ocp-index-options cmd current-module arg)))

(defun ac-ocp-index-candidates ()
  (let* ((command     (ocp-index-cmd "complete" ac-prefix))
         (output      (shell-command-to-string command))
         (lines       (replace-regexp-in-string "\n \+" " " output))
         (defs        (split-string lines "\n"))
         (split-lines (mapcar '(lambda(def) (split-string def " ")) defs))
         (symbols     (mapcar 'car-safe split-lines)))
    (setq ac-ocp-index-current-doc split-lines)
    symbols))

(defun ac-ocp-index-documentation (symbol)
  (mapconcat 'identity (cdr (assoc symbol ac-ocp-index-current-doc)) " "))

(defun ac-ocp-index-init ()
  (setq ac-ocp-index-current-doc nil))

(defvar ac-source-ocp-index
  '((init . ac-ocp-index-init)
    (candidates . ac-ocp-index-candidates)
    (symbol . "o")
    (document . ac-ocp-index-documentation)))

(defun ocp-index-print-type (ident)
  "Display the type of an ocaml identifier in the mini-buffer using ocp-index"
  (interactive (let ((default (ocp-index-completion-prefix)))
                 (list
                  (read-string
                   (format "type ident (%s): " default) nil nil default))))
  (let* ((command (ocp-index-cmd "type" ident))
         (output  (shell-command-to-string command))
         (type    (replace-regexp-in-string "\n\+$" "" output)))
    (message type)))

(defun ocp-index-jump-to-definition (ident)
  "Jump to the definition of an ocaml identifier using ocp-index"
  (interactive (let ((default (ocp-index-completion-prefix)))
                 (list
                  (read-string
                   (format "lookup ident (%s): " default) nil nil default))))
  (let* ((command (ocp-index-cmd "locate" ident))
         (output  (shell-command-to-string command))
         (match   (string-match "^\\([^:]*\\):\\([0-9]\+\\):\\([0-9]\+\\)"
                                output)))
    (when match
      (let ((file   (match-string 1 output))
            (line   (string-to-number (match-string 2 output)))
            (column (string-to-number (match-string 3 output))))
        (let ((file
               (if (file-exists-p file) file
                 (if (file-exists-p (concat "../" file))
                     (concat "../" file)
                   ))))
          (when file
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- line))
            (forward-char column)))))))

(defun ocp-index-setup-keymap ()
  (interactive nil)
  (local-set-key (kbd "C-c t") 'ocp-index-print-type)
  (local-set-key (kbd "C-c ;") 'ocp-index-jump-to-definition)
  (local-set-key (kbd "C-c TAB") 'auto-complete))

(defun ocp-index-setup-completion ()
  (interactive nil)
  (auto-complete-mode t)
  (setq ac-sources
        (cons 'ac-source-ocp-index
              ocp-index-extra-completion-sources))
  (ocp-index-setup-keymap)
  (when ocp-index-override-auto-complete-defaults
    (set (make-local-variable 'ac-auto-show-menu) t)
    (set (make-local-variable 'ac-auto-start) nil)
    (set (make-local-variable 'ac-delay) 0.0)
    (set (make-local-variable 'ac-expand-on-auto-complete) nil)
    (set (make-local-variable 'ac-ignore-case) nil)
    (set (make-local-variable 'ac-quick-help-delay) 0.2)
    (set (make-local-variable 'ac-trigger-commands) nil)))


(add-to-list 'ac-modes 'tuareg-mode)
(add-to-list 'ac-modes 'caml-mode)

(add-hook 'tuareg-mode-hook 'ocp-index-setup-completion t)
(add-hook 'caml-mode-hook 'ocp-index-setup-completion t)
