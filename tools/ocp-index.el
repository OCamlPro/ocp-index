;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-index

(provide 'ocp-index)

;; Customize defs

(defgroup ocp-index nil
  "ocp-index OCaml completion/doc tool binding configuration"
  :group 'languages)

(defcustom ocp-index-path "ocp-index"
  "*Path to access the ocp-index command"
  :group 'ocp-index :type '(file))

(defcustom ocp-grep-path "ocp-grep"
  "*Path to access the ocp-grep command"
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

(defcustom ocp-index-show-help t
  "*If set, show the documentation bubble after completion (otherwise,
   the type is printed in the minibuffer)."
  :group 'ocp-index :type 'boolean)

(defvar ocp-index-has-auto-complete
  (require 'auto-complete nil t))

(defcustom ocp-index-use-auto-complete ocp-index-has-auto-complete
  "*If set, use `auto-complete' instead of `completion-at-point' for completion."
  :group 'ocp-index :type 'boolean)

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

(defun ocp-index-symbol-at-point ()
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
      (message "%s" (mapconcat
                     (lambda (s) (format "\"%s\"" s))
                     (list* ocp-index-path cmd) " ")))
    cmd))

(defun ocp-index-run (cmd &rest args)
  (let* ((args (apply 'ocp-index-args cmd args))
         (shell-command (format "exec %s \"$@\"" ocp-index-path)))
    (with-output-to-string
      (let ((ret
             (apply 'call-process-region (point-min) (point)
                    shell-file-name
                    nil (list standard-output nil) nil
                    (list* shell-command-switch shell-command
                           ocp-index-path args))))
        (when (= 127 ret)
          (error "Could not find the Ocp-index program %S" ocp-index-path))))))

(defun ac-ocp-index-candidates ()
  (let* ((output (ocp-index-run "complete" "--sexp" ac-prefix))
         (defs   (car-safe (read-from-string output))))
    (setq ac-ocp-index-current-doc defs)
    (mapcar 'car-safe defs)))

(defun ac-ocp-index-documentation (symbol)
  (let* ((info (cdr (assoc symbol ac-ocp-index-current-doc)))
         (path (cdr (assoc :path info)))
         (kind (cdr (assoc :kind info)))
         (type (cdr (assoc :type info)))
         (doc  (cdr (assoc :doc info))))
    (if doc
        (format "%s %s: %s\n---\n%s" kind path type doc)
      (format "%s %s: %s" kind path type))))

(defun ac-ocp-index-action ()
  (if ocp-index-show-help
      (ac-last-quick-help)
    (let* ((symbol (buffer-substring (ocp-index-completion-prefix-start) (point)))
           (info   (cdr (assoc symbol ac-ocp-index-current-doc)))
           (path   (cdr (assoc :path info)))
           (kind   (cdr (assoc :kind info)))
           (type   (cdr (assoc :type info))))
      (message (format "%s %s: %s" kind path type)))))

(defun ac-ocp-index-init ()
  (setq ac-ocp-index-current-doc nil))

(defvar ac-source-ocp-index
  '((init . ac-ocp-index-init)
    (candidates . ac-ocp-index-candidates)
    (symbol . "o")
    (document . ac-ocp-index-documentation)
    (action . ac-ocp-index-action)
    ))

(defun ocp-index-setup-auto-complete ()
  (require 'auto-complete)
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
    (ocp-index-enable-ac-workaround))
  (add-to-list 'ac-modes 'tuareg-mode)
  (add-to-list 'ac-modes 'caml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion-at-point support ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ocp-index-completion-data (ident)
  "Return the data for completion of IDENT, i.e. a list of pairs (NAME . TYPE)."
  (let* ((output (ocp-index-run "complete" "--sexp" ident))
         (data (car-safe (read-from-string output))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (ocp-index-completion-format-entry entry)))
            data)))

(defun ocp-index-completion-lookup (string state)
  "Lookup the entry STRING inside the completion table."
  (let ((ret (assoc string ocp-index-completion-annotation-table)))
    (if ret (message "%s%s" (car ret) (cdr ret)))))

(defun ocp-index-completion-annotate (candidate)
  "Retrieve the annotation for candidate CANDIDATE in `ocp-index-annotatation-table'."
  (cdr (assoc candidate ocp-index-completion-annotation-table)))

(defun ocp-index-completion-format-entry (entry)
  "Format the completion entry ENTRY."
  (lexical-let* ((type (cdr (assoc :type (cdr entry))))
                 (kind (cdr (assoc :kind (cdr entry)))))
    (cond ((string-equal kind "val") (replace-regexp-in-string "\n" " " type))
          ((string-equal kind "exception")
           (format "exception%s"
                   (cond ((string-equal type "-") "")
                         (t (concat " " (replace-regexp-in-string "\n" " " type))))))
          (t kind))))

(defun ocp-index-completion-at-point ()
  (lexical-let
      ((bounds (ocp-index-bounds-of-symbol-at-point)))
    (when bounds
      (lexical-let*
          ((start (car bounds))
           (end (point))
           (string (buffer-substring start end)))
        (let ((data (ocp-index-completion-data string)))
          (setq ocp-index-completion-annotation-table
                (mapcar (lambda (a) (cons (car a) (concat ": " (cdr a))))
                    data))
          (list start end 'ocp-index-completion-table .
                (:exit-function 'ocp-index-completion-lookup
                                :annotation-function 'ocp-index-completion-annotate)))))))

(defun ocp-index-completion-table (string pred action)
  "Implement completion for ocp-index using `completion-at-point' API."
  (if (eq 'metadata action)
      '(metadata ((annotation-function . ocp-index-completion-annotate)
                  (exit-function . ocp-index-completion-lookup)))
    (complete-with-action action ocp-index-completion-annotation-table string pred)))

(defun ocp-index-setup-completion-at-point ()
  (add-hook 'completion-at-point-functions
            'ocp-index-completion-at-point nil 'local))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun ocp-index-print-type (ident)
  "Display the type of an ocaml identifier in the mini-buffer using ocp-index"
  (interactive (let ((default (ocp-index-symbol-at-point)))
                 (list
                  (read-string
                   (format "type ident (%s): " default) nil nil default))))
  (let* ((ident   (ocp-index-symbol-at-point))
         (output  (ocp-index-run "print" ident "%k %p: %t"))
         (output  (if (string= output "") "No definition found" output))
         (type    (replace-regexp-in-string "\n\+$" "" output)))
    (message "%s" type)))

(defun ocp-index-grep ()
  "Use ocp-index and ocp-grep to find uses of the identifier under point"
  (interactive nil)
  (let* ((ident (ocp-index-symbol-at-point))
         (path  (ocp-index-run "print" ident "%p"))
         (path  (if (string= path "") ident path))
         (path  (replace-regexp-in-string "\n\+$" "" path))
         (grep-use-null-device nil))
    (grep (format "%s %s" ocp-grep-path path))))

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
  (interactive (let ((default (ocp-index-symbol-at-point)))
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
  (ocp-index-print-type (ocp-index-symbol-at-point)))

(defun ocp-index-jump (name sig other-window)
  (if (and (eq (car-safe last-command) name)
           (cdr last-command))
      (let* ((locs (cdr last-command)))
        (if locs
            (progn
              (ocp-index-jump-to-loc (car locs) other-window)
              (cdr locs))))
    (let ((next (ocp-index-jump-to-definition (ocp-index-symbol-at-point) sig other-window)))
      (setq this-command (list* name next)))))

(defun ocp-index-jump-to-definition-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil nil))
(defun ocp-index-jump-to-definition-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-definition-at-point nil t))
(defun ocp-index-jump-to-sig-at-point ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t nil))
(defun ocp-index-jump-to-sig-at-point-other-window ()
  (interactive nil) (ocp-index-jump 'ocp-index-jump-to-sig-at-point t t))

(defun ocp-index-complete ()
  (interactive)
  (if ocp-index-use-auto-complete (auto-complete) (completion-at-point)))

(defvar ocp-index-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'ocp-index-print-type-at-point)
    (define-key map (kbd "C-c ;") 'ocp-index-jump-to-definition-at-point-other-window)
    (define-key map (kbd "C-c :") 'ocp-index-jump-to-sig-at-point-other-window)
    (define-key map (kbd "C-c C-;") 'ocp-index-jump-to-definition-at-point)
    (define-key map (kbd "C-c C-:") 'ocp-index-jump-to-sig-at-point)
    (define-key map (kbd "C-c /") 'ocp-index-grep)
    (define-key map (kbd "C-c TAB") 'ocp-index-complete)
    map))

(defun ocp-index-setup-completion ()
  (if ocp-index-use-auto-complete (ocp-index-setup-auto-complete))
  (ocp-index-setup-completion-at-point))

(define-minor-mode ocp-index-mode
  "OCaml auto-completion, documentation and source browsing using ocp-index"
  :group 'ocp-index
  :keymap ocp-index-keymap
  (if ocp-index-mode
      (ocp-index-setup-completion)
    (when ocp-index-use-auto-complete (auto-complete-mode -1)))
  )

(add-hook 'tuareg-mode-hook 'ocp-index-mode t)
(add-hook 'caml-mode-hook 'ocp-index-mode t)
