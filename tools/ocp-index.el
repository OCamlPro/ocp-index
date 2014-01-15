;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-index

(provide 'ocp-index)
(require 'auto-complete)

;; Customize defs

(defgroup ocp-index nil
  "ocp-index OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-index-path "ocp-index"
  "*Path to access the ocp-index command"
  :group 'ocp-index :type '(file))

(defcustom ocp-index-options "--show=types"
  "*Command-line parameters to add to ocp-index invocations (ex. --show=sigs)"
  :group 'ocp-index :type 'string)

(defcustom ocp-index-override-auto-complete-defaults t
  "*If set, auto-complete defaults will be reset to a sane setting in ocaml
   buffers. Disable if you prefer to configure auto-complete yourself."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-auto-complete-workaround t
  "*Fix a bug in auto-complete whith quick-help at EOF in text mode."
  :group 'ocp-index :type 'boolean)

(defcustom ocp-index-extra-completion-sources
  (list 'ac-source-words-in-same-mode-buffers)
  "*Completion sources to enable besides ocp-index completion"
  :group 'ocp-index :type '(repeat symbol))

;; auto-complete bug workaround (complete at EOF in text mode)
(defun ocp-index-enable-ac-workaround ()
  (defun ac-menu-delete ()
    (ac-remove-quick-help)
    (when ac-menu
      (popup-delete ac-menu)
      (setq ac-menu))))

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

(defun ocp-index-column-offset ()
  (save-excursion (let ((pt (point))) (forward-line 0) (- pt (point)))))

(defvar ocp-index-debug nil)

(defun ocp-index-debug-mode ()
  "Display command sent to ocp-index in the *Message* buffer"
  (interactive nil)
  (if ocp-index-debug
      (progn (message "ocp-index debug mode disabled")
	     (setq ocp-index-debug nil))
    (message "ocp-index debug mode enabled")
    (setq ocp-index-debug t)))

(defun ocp-index-args (cmd &rest args)
  (let*
      ((current-module (upcase-initials
                        (file-name-nondirectory
                         (file-name-sans-extension (buffer-file-name)))))
       (cmd (list* cmd ocp-index-options
                   "--full-open" current-module
                   "--context" ":"
                   args)))
    (when ocp-index-debug
      (message (mapconcat 'identity (list* ocp-index-path cmd) " ")))
    cmd))

(defun ocp-index-run (cmd &rest args)
  (let* ((args (apply 'ocp-index-args cmd args))
         (shell-cmd (mapconcat 'identity (list* ocp-index-path args) " ")))
    (with-output-to-string
      (apply 'call-process-region (point-min) (point) shell-file-name
             nil (list standard-output nil) nil
             (list shell-command-switch shell-cmd)))))

(defun ac-ocp-index-candidates ()
  (let* ((output (ocp-index-run "complete" "--sexp" ac-prefix))
         (defs   (car-safe (read-from-string output))))
    (setq ac-ocp-index-current-doc defs)
    (mapcar 'car-safe defs)))

(defun ac-ocp-index-documentation (symbol)
  (let* ((info (cdr (assoc symbol ac-ocp-index-current-doc)))
         (kind (cdr (assoc :kind info)))
         (type (cdr (assoc :type info)))
         (doc  (cdr (assoc :doc info))))
    (if doc
        (format "<%s> %s\n---\n%s" kind type doc)
      (format "<%s> %s" kind type))))

(defun ac-ocp-index-action ()
  (let* ((symbol (buffer-substring (ocp-index-completion-prefix-start) (point)))
         (info (cdr (assoc symbol ac-ocp-index-current-doc)))
         (type   (cdr (assoc :type info))))
    (message (format "%s: %s" symbol type))))

(defun ac-ocp-index-init ()
  (setq ac-ocp-index-current-doc nil))

(defvar ac-source-ocp-index
  '((init . ac-ocp-index-init)
    (candidates . ac-ocp-index-candidates)
    (symbol . "o")
    (document . ac-ocp-index-documentation)
    (action . ac-ocp-index-action)
    ))

(defun ocp-index-print-type (ident)
  "Display the type of an ocaml identifier in the mini-buffer using ocp-index"
  (interactive (let ((default (ocp-index-completion-prefix)))
                 (list
                  (read-string
                   (format "type ident (%s): " default) nil nil default))))
  (let* ((output  (ocp-index-run "type" ident))
         (output  (if (string= output "") "No definition found" output))
         (type    (replace-regexp-in-string "\n\+$" "" output)))
    (message type)))

(defun ocp-index-jump-to-loc (loc other-window)
  (if (string-match "^\\([^:]*\\):\\([0-9]\+\\):\\([0-9]\+\\)$" loc)
      (let ((file   (match-string 1 loc))
            (line   (string-to-number (match-string 2 loc)))
            (column (string-to-number (match-string 3 loc)))
            (last-buffer (current-buffer)))
        (when file
          (if other-window (find-file-other-window file) (find-file file))
          (goto-char (point-min))
          (forward-line (1- line))
          (forward-char column)
          (when other-window (switch-to-buffer-other-window last-buffer))))
    (message (replace-regexp-in-string "\n\+$" "" loc))))

(defun ocp-index-jump-to-definition (ident sig other-window)
  "Jump to the definition of an ocaml identifier using ocp-index"
  (interactive (let ((default (ocp-index-completion-prefix)))
                 (list
                  (read-string
                   (format "lookup ident (%s): " default) nil nil default)
                  nil t)))
  (let* ((output (if sig (ocp-index-run "locate" "-i" ident)
                   (ocp-index-run "locate" ident)))
         (locs (split-string output "\n" t)))
    (if locs
        (progn
          (ocp-index-jump-to-loc (car locs) other-window)
          (cdr locs))
      (message "No definition found")
      nil)))

(defun ocp-index-print-type-at-point ()
  (interactive nil)
  (ocp-index-print-type (ocp-index-completion-prefix)))

(defun ocp-index-jump (name sig other-window)
  (if (and (eq (car-safe last-command) name)
           (cdr last-command))
      (let* ((locs (cdr last-command)))
        (if locs
            (progn
              (ocp-index-jump-to-loc (car locs) other-window)
              (cdr locs))))
    (let ((next (ocp-index-jump-to-definition (ocp-index-completion-prefix) sig other-window)))
      (setq this-command (list* name next)))))

(defun ocp-index-jump-to-definition-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil nil))
(defun ocp-index-jump-to-definition-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil t))
(defun ocp-index-jump-to-sig-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t nil))
(defun ocp-index-jump-to-sig-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t t))

(defvar ocp-index-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-index-print-type-at-point)
    (define-key map (kbd "C-c ;") 'ocp-index-jump-to-definition-at-point-other-window)
    (define-key map (kbd "C-c :") 'ocp-index-jump-to-sig-at-point-other-window)
    (define-key map (kbd "C-c C-;") 'ocp-index-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-:") 'ocp-index-jump-to-sig-at-point)
    (define-key map (kbd "C-c TAB") 'auto-complete)
    map))

(defun ocp-index-setup-completion ()
  (auto-complete-mode t)
  (setq ac-sources
        (cons 'ac-source-ocp-index
              ocp-index-extra-completion-sources))
  (when ocp-index-override-auto-complete-defaults
    (set (make-local-variable 'ac-auto-show-menu) t)
    (set (make-local-variable 'ac-auto-start) nil)
    (set (make-local-variable 'ac-delay) 0.0)
    (set (make-local-variable 'ac-expand-on-auto-complete) nil)
    (set (make-local-variable 'ac-ignore-case) nil)
    (set (make-local-variable 'ac-quick-help-delay) 0.2)
    (set (make-local-variable 'ac-trigger-commands) nil))
  (when ocp-index-auto-complete-workaround
    (ocp-index-enable-ac-workaround)))

(define-minor-mode ocp-index-mode
  "OCaml auto-completion, documentation and source browsing using ocp-index"
  :group 'ocp-index
  :keymap ocp-index-keymap
  (if ocp-index-mode
      (ocp-index-setup-completion)
    (auto-complete-mode -1))
  )

(add-to-list 'ac-modes 'tuareg-mode)
(add-to-list 'ac-modes 'caml-mode)

(add-hook 'tuareg-mode-hook 'ocp-index-mode t)
(add-hook 'caml-mode-hook 'ocp-index-mode t)
