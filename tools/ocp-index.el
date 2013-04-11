;; auto-completion for ocaml using auto-complete
;; (https://github.com/auto-complete/auto-complete) and ocp-index

(require 'auto-complete)

(defvar ac-ocp-index-current-doc nil)

(defun ac-ocp-index-candidates ()
  (let* ((command     (format "ocp-index complete %s" ac-prefix))
         (output      (shell-command-to-string command))
         (lines       (replace-regexp-in-string "\n \+" " " output))
         (defs        (split-string lines "\n"))
         (split-lines (mapcar '(lambda(def) (split-string def " ")) defs))
         (symbols     (mapcar 'car split-lines)))
    (setq ac-ocp-index-current-doc split-lines)
    symbols))

(defun ac-ocp-index-documentation (symbol)
  (mapconcat 'identity (cdr (assoc symbol ac-ocp-index-current-doc)) " "))

(defun ac-ocp-index-init ()
  (setq ac-ocp-index-current-doc nil))

(defvar ac-source-ocp-index
  '((init . ac-ocp-index-init)
    (candidates . ac-ocp-index-candidates)
    (document . ac-ocp-index-documentation)))

(add-hook
 'tuareg-mode-hook
 '(lambda()
    (auto-complete-mode 1)
    (setq ac-sources '(ac-source-ocp-index))))

(add-to-list 'ac-modes 'tuareg-mode)
