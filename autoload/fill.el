;;; autoload/fill.el --- -*- lexical-binding: t -*-

;;; Comment

;; The `ace-fill.el' library (reproduced below) is a tiny wrapper around
;; some Emacs settings and modes that are scrattered around several files,
;; which control (i) how paragraphs or comments in programming modes should
;; be wrapped to a given column count, and (ii) what constitutes a
;; sentence.  I put them all together here to make things easier to track.
;; - With regard to paragraphs, I find that a double space is the best way
;;   to delimit sentences in source form, where a monospaced typeface is
;;   customary.  There is no worry that this will be shown on a website or
;;   rendered version of a document, because processors know how to handle
;;   spacing.  We do this to make phrases easier to tell apart, but also to
;;   render unambiguous commands like `forward-sentence'.
;; - `ale-fill-fill-mode' sets my desired default column width for all
;;   buffers, while it applies another value for programming modes (in case
;;   there is a need to control the two cases separately).  Those values
;;   are stored in the variables `ale-fill-default-column' and
;;   `ale-fill-prog-mode-column' respectively.  My minor mode also enables
;;   `auto-fill-mode' in `text-mode' and `prog-mode' buffers through the
;;   appropriate hooks.

;;; Code
(defgroup ale-fill ()
  "Tweak for filling paragraphs."
  :group 'fill)

(defcustom ale-fill-default-column 80
  "Default width for `fill-column'."
  :type 'integer
  :group 'ale-fill)

(defcustom ale-fill-prog-mode-column 100
  "`prog-mode' width for `fill-column'.
Also see `ale-fill-default-column'."
  :type 'integer
  :group 'ale-fill)

(defun ale-fill--fill-prog ()
  "Set local value of `fill-column' for programming modes.
Meant to be called via `prog-mode-hook'."
  (setq-local fill-column ale-fill-prog-mode-column))

;;;###autoload
(define-minor-mode ale-fill-fill-mode
  "Set up fill-mode and relevant variable."
  :init-value nil
  :global t
  (if ale-fill-fill-mode
      (progn
        (setq-default fill-column ale-fill-default-column)
        (add-hook 'prog-mode-hook #'ale-fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 80)
    (remove-hook 'prog-mode-hook #'ale-fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))
