(require 'cl)
(require 'ob-core)

(defgroup plumber nil
  "plumber group")

(defcustom plumber-show-type 'message
  "which type used to show translated content"
  :type '(choice (const message)
                 (const tooltip)
                 (const overlay)))

(defcustom plumber-config-alist nil
  "The format of element is like (regexp translater-function keybinding)")

(defun plumber--match-p (content rule)
  (let ((pattern (first rule))
        (keybinding (third rule)))
    (and (string-match-p pattern content)
         (or (null keybinding)
             (equal keybinding (this-command-keys))
             (equal keybinding (this-command-keys-vector))))))

(defun plumber-translate (start end)
  ""
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (config (cl-find-if (lambda (rule)
                               (plumber--match-p content rule))  plumber-config-alist))
         (translater (second config))
         (show-function (intern (format "plumber-display-by-%s" plumber-show-type))))
    (funcall show-function start end (funcall translater content))))

(defun plumber-display-by-message (start end content)
  "display by message"
  (message "%s" content))

(defun plumber-display-by-tooltip (start end content)
  "display by tooltip"
  (tooltip-show (format "%s" content)))

(defun plumber-display-by-overlay (start end content)
  "display by overlay"
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'display content)))

(defun plumber--read-from-org-table-str (org-table-str)
  (with-temp-buffer
    (insert org-table-str)
    (org-babel-read-table)))
