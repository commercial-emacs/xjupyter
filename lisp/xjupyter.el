;;; xjupyter.el --- notebooks  -*- lexical-binding:t -*-

;; Copyright (C) 2025 commandlinesystems.com

;; Authors: dickmao <github id: dickmao>
;; URL: https://github.com/commercial-emacs/xjupyter
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires:

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mode-overlay)
(require 'rx)

(defcustom xjupyter-image-height-in-lines 11
  "Aesthetics."
  :group 'xjupyter
  :type 'natnum)

(defconst xjupyter--prompt-properties
  '(read-only t front-sticky t font-lock-face header-line))

(defconst xjupyter--unvisible-properties
  '(read-only t display ""))

(defconst xjupyter--toggled-properties
  '(read-only t display ".\n"))

(defconst xjupyter--writable-after
  '(read-only t rear-nonsticky t))

(defconst xjupyter--in-regexp (rx bol "In [" (one-or-more digit) "]:" eol))

(defconst xjupyter--image-regexp
  (rx bol
      "[" (group (= 7 xdigit) "." (one-or-more (not "]"))) "]"
      "(" (zero-or-more nonl) ")"
      eol))

(defconst xjupyter--out-regexp (rx bol "Out [" (one-or-more digit) "]:" eol))

(defsubst xjupyter--new-id ()
  (substring (md5 (format "%s" (random t))) 0 7))

(defvar-local xjupyter--receive-cursor nil
  "Keep place in receive buffer.")

;; Concede this extra state as reliably mapping buffer-name or
;; buffer-file-name to a fixed string is impossible (buffer-names can
;; lose their <1> suffix, and buffer-file-names do not exist yet for
;; new files).
(defvar-local xjupyter--rpc-buffer nil
  "Go from jpr buffer to its rpc buffer.")

(defvar-local xjupyter--annotations nil
  "Alist of (UNDO-SUBLIST . (CELL-BEGIN . CELL-ID)).")

(defvar-local xjupyter--pre-undo-list nil)

(defvar-local xjupyter--cells nil
  "Ordered alist of (CELL-MARKER . CELL-ID).")

(defvar-local xjupyter--purgatory nil
  "Unordered alist of (CELL-ID . CELL-PLIST).
Specifically for undo, and as such cannot be loosy-goosey like
xjupyter--kill-ring which you might think would generalize.")

(defvar-local xjupyter--kill-ring nil
  "Circular stack of (CELL-ID . CELL-PLIST).")

(defvar-local xjupyter--yank-index nil
  "Prevailing yank index into kill ring.")

(defvar edebug-active)
(defun xjupyter-cell-focus (pos)
  "Only a command (versus function eval) triggers proximity update.
Vagaries of `execute-kbd-macro' and switching buffers prevents us from
simply let'ing `last-command'."
  (let ((restore-last-command last-command)
	(restore-this-command this-command))
    (when-let ((desired (xjupyter-input-beg pos))
	       (edebug-active t)) ;don't enter debugger
      (execute-kbd-macro (read-kbd-macro (format "M-x goto-char RET %d" desired))))
    (setq last-command restore-last-command
	  this-command restore-this-command)))

(defun xjupyter-cell-floor (pos)
  (save-match-data
    (save-excursion
      (when (< pos (point-max))
	(goto-char pos)
	(let* (case-fold-search
	       (anchor (or (re-search-backward xjupyter--in-regexp nil :noerror)
			   (point-min)))
	       (maybe (progn (goto-char (min (1+ anchor) (point-max)))
			     (when (re-search-forward xjupyter--in-regexp nil :noerror)
			       (re-search-backward xjupyter--in-regexp)))))
	  (if (and maybe (<= maybe pos))
	      maybe
	    (if (= anchor (point-min))
		(when (progn (goto-char anchor)
			     (looking-at-p xjupyter--in-regexp))
		  anchor)
	      anchor)))))))

(defun xjupyter-cell-ceiling (pos)
  "The problem with `mode-require-final-newline' is it's only on-save."
  (save-match-data
    (save-excursion
      (when-let ((floor (xjupyter-cell-floor pos)))
	(goto-char floor)
	(or (when (zerop (forward-line 1)) ;past In
	      (let (case-fold-search)
		(when (re-search-forward xjupyter--in-regexp nil :noerror)
		  (re-search-backward xjupyter--in-regexp))))
	    (point-max))))))

(defun xjupyter-cell-up (pos)
  (interactive "d")
  (when-let ((input-end (or (xjupyter-input-end pos)
			    (when-let ((prev (xjupyter-cell-prev pos)))
			      (setq pos (xjupyter-input-end prev))))))
    (if (>= pos input-end)
	(goto-char (xjupyter-input-beg pos))
      (when-let ((beg (xjupyter-input-beg (or (xjupyter-cell-prev pos) pos))))
	(goto-char beg)))))

(defun xjupyter-cell-down (pos)
  (interactive "d")
  (when-let ((beg (xjupyter-input-beg (or (xjupyter-cell-next pos) pos))))
    (goto-char beg)))

(defun xjupyter-cell-prev (pos)
  (let* ((floor (or (xjupyter-cell-floor pos) pos))
	 (back (1- floor)))
    (when (> back (point-min))
      (xjupyter-cell-floor back))))

(defun xjupyter-cell-next (pos)
  (if (xjupyter-cell-floor pos)
      (when-let ((past (xjupyter-cell-ceiling pos)))
	(when (< past (point-max))
	  (xjupyter-cell-floor past)))
    (save-match-data
      (save-excursion
	(goto-char pos)
	(let (case-fold-search)
	  (when-let ((next (re-search-forward xjupyter--in-regexp nil :noerror)))
	    (xjupyter-cell-floor next)))))))

(defmacro xjupyter--base-buffer (&rest body)
  (declare (debug (&rest form)))
  `(with-current-buffer (or (buffer-base-buffer) (current-buffer))
     ,@body))

(defun xjupyter--collapse-undo (end)
  (unless (cl-tailp end buffer-undo-list)
    (error "(%S...) not in buffer-undo-list" (car end)))
  (let ((curr buffer-undo-list)
	(prev nil))
    (when (null (car curr)) ;initial undo boundary
      (setq prev curr
	    curr (cdr curr)))
    (while (not (eq curr end))
      (if (and (null (car curr)) prev)
	  (setcdr prev (cdr curr))
	(setq prev curr))
      (setq curr (cdr curr)))))

(defun xjupyter--undo-push (fun &rest args)
  (push `(apply ,fun ,@args) buffer-undo-list))

(defun xjupyter-cell-evaluate (pos)
  (interactive "d")
  (when-let ((mode-ov (seq-find #'mode-overlay-p (overlays-at pos)))
	     (floor (xjupyter-cell-floor pos))
	     (kwargs (make-hash-table))
	     (ix (xjupyter--insertion-index floor))
	     (cell-id (cdr (nth ix (xjupyter--base-buffer xjupyter--cells)))))
    (with-current-buffer (overlay-buffer mode-ov)
      (puthash 'code (buffer-substring
		      (xjupyter-input-beg floor)
		      (xjupyter-input-end floor))
	       kwargs)
      (puthash 'allow_stdin t
	       kwargs)
      (xjupyter-rpc-request cell-id kwargs "execute"))))

(defun xjupyter-kernel-interrupt ()
  (interactive)
  (xjupyter-rpc-request nil nil "interrupt_request"))

(defun xjupyter-cell-insert (pos cell-id cell-plist &optional interactive-p)
  (interactive "d\ni\ni\np")
  (unless cell-id
    (setq cell-id (xjupyter--new-id)))
  (xjupyter--base-buffer
   (let* ((floor (xjupyter--cell-textualize
		  pos (plist-get cell-plist :input)
		  (plist-get cell-plist :output)))
	  (pair (xjupyter--cell-render-then-register floor cell-id)))
     (xjupyter--undo-push #'xjupyter--id-kill (cdr pair))
     (when interactive-p
       (xjupyter-cell-focus (car pair))))))

(defsubst xjupyter-cell-append (pos cell-id cell-plist &optional interactive-p)
  (interactive "d\ni\ni\np")
  (xjupyter-cell-insert (or (xjupyter-cell-next pos) (point-max))
			cell-id cell-plist interactive-p))

(defun xjupyter--bump-indices (floor op)
  (xjupyter--base-buffer
   (let ((current floor)
	 (buffer-undo-list t)
	 (inhibit-read-only t))
     (while-let ((next (xjupyter-cell-next current)))
       (goto-char (setq current next))
       (let* ((index (xjupyter--extract-index current))
	      (index+ (funcall op index))
	      (ceiling (xjupyter-cell-ceiling (point)))
	      case-fold-search)
	 (replace-string-in-region (number-to-string index)
				   (apply #'propertize (number-to-string index+)
					  xjupyter--prompt-properties)
				   (point)
				   (line-end-position))
	 (when (re-search-forward xjupyter--out-regexp ceiling :noerror)
	   (re-search-backward xjupyter--out-regexp)
	   (replace-string-in-region (number-to-string index)
				     (apply #'propertize (number-to-string index+)
					    xjupyter--unvisible-properties)
				     (point)
				     (line-end-position))))))))

(defun xjupyter--cell-detextualize (floor)
  (xjupyter--base-buffer
   (let ((inhibit-read-only t)
	 (buffer-undo-list t)
	 (ceiling (xjupyter-cell-ceiling floor)))
     (prog1 (buffer-substring-no-properties floor ceiling)
       (delete-region floor ceiling)))))

(defun xjupyter--id-kill (cell-id)
  (xjupyter--base-buffer
   (if-let ((pair (rassoc cell-id xjupyter--cells)))
       (xjupyter-cell-kill (car pair))
     (error "xjupyter--id-kill: No such cell %s" cell-id))))

(defun xjupyter--extract-io (cell-text)
  (with-temp-buffer
    (save-excursion (insert cell-text))
    (when-let ((floor (xjupyter-cell-floor (point))))
      (list :input (buffer-substring (xjupyter-input-beg floor)
				     (xjupyter-input-end floor))
	    :output (buffer-substring (xjupyter-output-beg floor)
				      (xjupyter-cell-ceiling floor))))))

(defun xjupyter--id-unkill (cell-id index)
  (xjupyter--base-buffer
   (let ((floor (if (< index (length xjupyter--cells))
		    (marker-position (car (nth index xjupyter--cells)))
		  (point-max))))
     (xjupyter-cell-insert floor cell-id (xjupyter--cell-unshelve cell-id)))))

(defun xjupyter--cell-shelve (cell-id cell-plist)
  (xjupyter--base-buffer
   (setf (alist-get cell-id xjupyter--purgatory nil nil #'equal)
	 cell-plist)))

(defun xjupyter--cell-unshelve (cell-id)
  (xjupyter--base-buffer
   (let ((elem (assoc cell-id xjupyter--purgatory)))
     (prog1 (cdr elem)
       (setq xjupyter--purgatory (delq elem xjupyter--purgatory))))))

(defun xjupyter-cell-yank (pos &optional interactive-p)
  (interactive "d\np")
  (xjupyter--base-buffer
   (if-let ((id-plist (ignore-errors
			(ring-ref xjupyter--kill-ring xjupyter--yank-index))))
       (cl-destructuring-bind (cell-id . cell-plist)
	   id-plist
	 (when (rassoc cell-id xjupyter--cells)
	   (setq cell-id (xjupyter--new-id)))
	 (xjupyter-cell-insert pos cell-id cell-plist interactive-p))
     (user-error "Empty kill ring"))))

(defun xjupyter-cell-yank-pop (pos &optional interactive-p)
  (interactive "d\np")
  (cl-assert (not (ring-empty-p
		   (xjupyter--base-buffer xjupyter--kill-ring))))
  (let ((restore-buffer-undo-list buffer-undo-list))
    (when (memq last-command '(xjupyter-cell-yank
			       xjupyter-cell-yank-pop))
      (xjupyter-cell-kill pos interactive-p)
      (xjupyter--base-buffer
       (cl-incf xjupyter--yank-index)))
    (xjupyter-cell-yank pos interactive-p)
    (xjupyter--collapse-undo restore-buffer-undo-list)))

(defun xjupyter-cell-kill (pos &optional interactive-p)
  (interactive "d\np")
  (when-let ((floor (xjupyter-cell-floor pos)))
    (let* ((id-ix (progn (xjupyter--bump-indices floor #'1-)
			 (xjupyter--cell-deregister floor)))
	   (cell-id (car id-ix))
	   (cell-index (cdr id-ix))
	   (cell-plist (xjupyter--extract-io (xjupyter--cell-detextualize floor))))
      (xjupyter--cell-shelve cell-id cell-plist)
      (xjupyter--undo-push #'xjupyter--id-unkill cell-id cell-index)
      (when (eq this-command 'xjupyter-cell-kill)
	(ring-insert (xjupyter--base-buffer xjupyter--kill-ring)
		     (cons cell-id cell-plist)))
      (when interactive-p
	(xjupyter-cell-focus floor))
      (unless (xjupyter--base-buffer xjupyter--cells)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (when (buffer-base-buffer)
	    (read-only-mode 1))))
      id-ix)))

(defun xjupyter-cell-split (pos &optional interactive-p)
  (interactive "d\np")
  (when-let ((floor (xjupyter-cell-floor pos))
	     (beg (xjupyter-input-beg floor))
	     (end (xjupyter-input-end floor))
	     (input-p (and (>= pos beg) (< pos end))))
    (let* ((restore-buffer-undo-list buffer-undo-list)
	   (latter (buffer-substring-no-properties pos end))
	   (cell-plist (list :input latter :output "\n")))
      (when (and (> pos beg) (eq (char-before pos) 10))
	(cl-decf pos)) ;1- newline
      (let ((inhibit-read-only t))
	(delete-region pos (1- end))) ;1+ newline
      (xjupyter-cell-append pos nil cell-plist interactive-p)
      (xjupyter--collapse-undo restore-buffer-undo-list))))

(defun xjupyter-cell-merge (pos &optional interactive-p)
  (interactive "d\np")
  (if-let ((floor (xjupyter-cell-floor pos))
	   (next (xjupyter-cell-next floor))
	   (next (set-marker (make-marker) next)))
      (let ((restore-buffer-undo-list buffer-undo-list)
	    (latter (buffer-substring-no-properties
		     (xjupyter-input-beg next)
		     (1- (xjupyter-input-end next))))) ;1- newline
	(save-excursion
	  (goto-char (1- (xjupyter-input-end floor)))
	  (let ((inhibit-read-only t))
	    (insert "\n" latter)))
	(xjupyter-cell-kill next interactive-p)
	(xjupyter--collapse-undo restore-buffer-undo-list)
	(when interactive-p
	  (xjupyter-cell-focus floor)))   ;1+ newline
    (user-error "Merge down, not up.")))

(defun xjupyter--cell-busy (pos)
  (when-let ((floor (xjupyter-cell-floor pos)))
    (save-excursion
      (goto-char floor)
      (save-match-data
	(when (re-search-forward "[0-9]+" (line-end-position) :noerror)
	  (get-text-property 0 'display (match-string 0)))))))

(defun xjupyter-cell-toggle-output (pos &optional interactive-p)
  (interactive "d\np")
  (when-let ((floor (xjupyter-cell-floor pos))
	     (beg (xjupyter-output-beg floor))
	     (end (xjupyter-cell-ceiling floor))
	     (free-p (not (xjupyter--cell-busy floor)))
	     (output (buffer-substring-no-properties beg end))
	     (output-p (not (equal "\n" output)))
	     (buffer-undo-list t)
	     (inhibit-read-only t))
    (with-silent-modifications
      (save-excursion
	(let ((toggled-p (get-text-property
			  0 'display (buffer-substring beg end))))
	  (delete-region beg end)
	  (goto-char beg)
	  (insert (if toggled-p
		      output
		    (apply #'propertize output xjupyter--toggled-properties)))
	  (when toggled-p ;now untoggled
	    (xjupyter--render-images beg)))))
    (when interactive-p
      (xjupyter-cell-focus floor))))

(defun xjupyter--cell-move (pos func interactive-p)
  (let* ((restore-buffer-undo-list buffer-undo-list)
	 (restore-modified-p (buffer-modified-p))
	 (id-ix (xjupyter-cell-kill pos interactive-p))
	 (begs (append (mapcar (lambda (c) (marker-position (car c)))
			       (xjupyter--base-buffer xjupyter--cells))
		       (list (point-max))))
	 (id (car id-ix))
	 (ix (cdr id-ix))
	 (ix* (and (fixnump ix) (funcall func ix)))
	 (valid-p (and ix* (<= 0 ix*) (< ix* (length begs)))))
    (if valid-p
	(progn
	  (xjupyter-cell-insert (nth ix* begs)
				id
				(xjupyter--cell-unshelve id)
				interactive-p)
	  (xjupyter--collapse-undo restore-buffer-undo-list)
	  (when interactive-p
	    (xjupyter-cell-focus (nth ix* begs)))
	  (cons id ix*))
      (when id-ix
	(let ((inhibit-modification-hooks t)
	      (inhibit-message t))
	  (undo)
	  (when (memq restore-modified-p '(nil autosaved))
	    (restore-buffer-modified-p restore-modified-p))))
      (setq buffer-undo-list restore-buffer-undo-list)
      (when interactive-p
	(xjupyter-cell-focus pos))
      nil)))

(defun xjupyter-cell-move-up (pos &optional interactive-p)
  (interactive "d\np")
  (xjupyter--cell-move pos #'1- interactive-p))

(defun xjupyter-cell-move-down (pos &optional interactive-p)
  (interactive "d\np")
  (xjupyter--cell-move pos #'1+ interactive-p))

(defun xjupyter--undo-n (n list)
  "`primitive-undo' hacked up for cell displacement."
  (dotimes (_i (+ n (if (null (car list)) 1 0))) ;1+ for initial boundary
    (while-let ((next (car list)))     ;inner loop per undo boundary
      (let* ((inhibit-read-only t)
	     (relocator (alist-get list (xjupyter--base-buffer
					 xjupyter--annotations)))
	     (former-beg (car relocator))
	     (cell-id (cdr relocator))
	     (pair (when cell-id (rassoc cell-id (xjupyter--base-buffer
						  xjupyter--cells))))
	     (delta (if pair
			(- (car pair) former-beg)
		      0)))
	(pcase next
          ;; Element INTEGER sets point.
          ((pred integerp)
	   (goto-char (+ next delta)))
          ;; Element (t . TIME) records previous modtime.
          (`(t . ,time)
           ;; If we've rewound back to the maiden undo, and TIME matches
           ;; what's on disk, then clear the modified indicator.
           (let ((visited-file-time (visited-file-modtime)))
	     (if (and (numberp visited-file-time)
		      (= visited-file-time 0)
		      (buffer-base-buffer))
		 (setq visited-file-time
		       (with-current-buffer (buffer-base-buffer)
			 (visited-file-modtime))))
	     (when (time-equal-p time visited-file-time)
	       (unlock-buffer)
	       (set-buffer-modified-p nil))))
          ;; Element (nil PROP VAL BEG . END) is property change.
          (`(nil . ,(or `(,prop ,val ,beg . ,end) pcase--dontcare))
           (put-text-property (+ beg delta) (+ end delta) prop val))
          ;; Element (BEG . END) means range was inserted.
          (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
           (goto-char (+ delta beg))
           (delete-region (+ delta beg) (+ delta end)))
          ;; Element (apply FUN . ARGS) means call FUN to undo.
          (`(apply . ,fun-args)
           (save-current-buffer
	     (if (integerp (car fun-args))
		 ;; Long format: (apply DELTA START END FUN . ARGS).
		 (pcase-let* ((`(,delta ,start ,end ,fun . ,args) fun-args)
			      (start-mark (copy-marker start nil))
			      (end-mark (copy-marker end t)))
                   (apply fun args)
                   (when (/= start-mark start)
		     (error "Post-undo start %s not %s" start-mark start))
                   (when (/= end-mark (+ delta end))
		     (error "Post-undo end %s not %s+%s" end-mark delta end)))
	       (apply fun-args))))
          ;; Element (STRING . POS) means STRING was deleted.
          (`(,(and string (pred stringp)) . ,(and pos (pred integerp)))
	   (if (> pos 0)
	       (setq pos (+ pos delta))
	     (setq pos (- (+ (abs pos) delta))))
           (goto-char (abs pos))
	   (insert string)
	   (when (> pos 0)
	     ;; A negative POS leaves point at insertion's end.
	     (goto-char pos)))
          (_ (error "Unrecognized entry in undo list %S" next))))
      (pop list)
      (while (and (markerp (car-safe (car list)))
                  (integerp (cdr-safe (car list))))
	;; burn troublesome markers
	(pop list)))
    (pop list))
  list)

(declare-function xjupyter--unadvise-undo "xjupyter")
(declare-function xjupyter--advise-undo "xjupyter")

;;;###autoload
(define-minor-mode jpr-mode "Notebook." :lighter " JPR" :group 'jpr
  :keymap (let ((map (make-sparse-keymap)))
	    (dolist (seq (where-is-internal 'undo))
	      (define-key map seq #'xjupyter-undo))
            (define-key map [(control ?c) (control ?c)] #'xjupyter-cell-evaluate)
            (define-key map [(control ?c) (control ?a)] #'xjupyter-cell-insert)
            (define-key map [(control ?c) (control ?b)] #'xjupyter-cell-append)
            (define-key map [(control ?c) (control ?k)] #'xjupyter-cell-kill)
            (define-key map [(control ?c) (control ?y)] #'xjupyter-cell-yank)
            (define-key map [(control ?c) (meta ?y)] #'xjupyter-cell-yank-pop)
            (define-key map [(control ?c) (control meta ?y)] #'xjupyter-cell-yank-pop)
            (define-key map [(control ?c) (control up)] #'xjupyter-cell-move-up)
            (define-key map [(control ?c) (control down)] #'xjupyter-cell-move-down)
	    (define-key map [(control up)] #'xjupyter-cell-up)
	    (define-key map [(control ?c) (control ?p)] #'xjupyter-cell-up)
	    (define-key map [(control down)] #'xjupyter-cell-down)
	    (define-key map [(control ?c) (control ?n)] #'xjupyter-cell-down)
            (define-key map [(control ?c) (control ?/)] #'xjupyter-kernel-interrupt)
            (define-key map [(control ?c) (control ?l)] #'xjupyter-cell-toggle-output)
	    (define-key map [(control ?c) (control ?s)] #'xjupyter-cell-split)
	    (define-key map [(control ?c) (control ?m)] #'xjupyter-cell-merge)
	    (define-key map [(control ?c) ?\r] #'xjupyter-cell-merge)
            map)
  ;; cannot jpr-mode on top of already jpr-mode,
  ;; so clear everything first.
  (when (and global-hl-line-mode
	     (boundp 'hl-line-mode)
	     (not hl-line-mode))
    (hl-line-mode))
  (setq buffer-undo-list nil)
  (mapc #'kill-local-variable '(before-save-hook
				pre-command-hook
				post-command-hook
				xjupyter--cells
				xjupyter--purgatory
				xjupyter--annotations
				xjupyter--pre-undo-list
				kill-buffer-hook
				xjupyter--yank-index
				xjupyter--kill-ring
				))

  (unless (buffer-base-buffer) ;do just in base buffer
    (read-only-mode 0)
    (save-excursion
      (save-restriction
	(widen)
	(with-silent-modifications
	  (let ((inhibit-read-only t)
		(buffer-undo-list t))
	    (delete-all-mode-overlays)
	    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
	      (erase-buffer)
	      (insert text))))))
    (when jpr-mode
      (read-only-mode 1)
      (setq-local xjupyter--kill-ring (make-ring 128)
		  xjupyter--yank-index 0)
      (when (bound-and-true-p hl-line-mode)
	(hl-line-mode -1))
      (setq-local before-save-hook before-save-hook)
      (remove-hook 'before-save-hook #'delete-trailing-whitespace :local)
      (cl-do* ((shiva (point-min) (xjupyter-cell-ceiling floor))
	       (floor (or (xjupyter-cell-floor (point-min))
			  (xjupyter-cell-next (point-min)))
		      (xjupyter-cell-next floor)))
	  ((null floor)
	   (delete-region shiva (point-max)))
	(let ((pair (xjupyter--cell-render-then-register floor (xjupyter--new-id))))
	  (delete-region shiva floor)
	  (setq floor (car pair))))
      (unless xjupyter--cells
	(erase-buffer)
	(let ((buffer-undo-list t))
	  (call-interactively #'xjupyter-cell-insert)))))

  (when jpr-mode ;done in both base and indirect buffers
    (add-hook 'pre-command-hook #'xjupyter--pre-undo-list nil :local)
    (add-hook 'pre-command-hook #'xjupyter--advise-undo nil :local)
    (add-hook 'post-command-hook #'xjupyter--annotate-undo nil :local)
    (add-hook 'post-command-hook #'xjupyter--unadvise-undo nil :local)
    (add-hook 'kill-buffer-hook
	      (lambda ()
		(when-let ((proc (xjupyter-rpc-get nil 'no-create)))
		  (kill-process proc)))
	      nil
	      :local)
    (xjupyter-cell-focus (point))))

(defface xjupyter-input-area-face
  `((((class color) (background light))
     :background "honeydew1" ,@(when (>= emacs-major-version 27) '(:extend t)))
    (((class color) (background dark))
     :background "#383838" ,@(when (>= emacs-major-version 27) '(:extend t))))
  "Face for cell input area"
  :group 'xjupyter)

(defun xjupyter-input-beg (pos)
  (save-excursion
    (when-let ((floor (xjupyter-cell-floor pos)))
      (goto-char floor)
      (when (zerop (forward-line 1))
	(point)))))

(defun xjupyter-input-end (pos)
  (save-excursion
    (when-let ((output-beg (xjupyter-output-beg pos)))
      (goto-char (xjupyter-output-beg pos))
      (when (zerop (forward-line -1))
	(point)))))

(defun xjupyter-output-beg (pos)
  (save-excursion
    (when-let ((floor (xjupyter-cell-floor pos)))
      (goto-char floor)
      (when (re-search-forward xjupyter--out-regexp nil :noerror)
	(when (zerop (forward-line 1))
	  (point))))))

(defun xjupyter--overlay-image (pos file)
  (when (display-images-p)
    (let ((image (create-image file nil nil
			       :max-width (* fill-column
					     (frame-char-width
					      (selected-frame)))
			       :max-height (* xjupyter-image-height-in-lines
					      (frame-char-height
					       (selected-frame)))
			       :scale 1))
	  (ol (make-overlay (set-marker (make-marker) pos)
			    (save-excursion
			      (goto-char pos)
			      (set-marker (make-marker) (line-end-position))))))
      (when (fboundp 'image-flush)
	(image-flush image))
      (overlay-put ol 'category 'xjupyter)
      (overlay-put ol 'evaporate t)
      (overlay-put ol 'display image)
      (overlay-put ol 'face 'default))))

(defun xjupyter--render-images (pos)
  (save-excursion
    (cl-do ((end (xjupyter-cell-ceiling pos))
	    (pos (xjupyter-output-beg pos)
		 (save-excursion
		   (goto-char pos)
		   (when (zerop (forward-line))
		     (point)))))
	((or (not pos) (>= pos end)))
      (goto-char pos)
      (save-match-data
	(when-let ((line (buffer-substring-no-properties
			  (line-beginning-position)
			  (line-end-position)))
		   (file-base
		    (progn (string-match xjupyter--image-regexp line)
			   (match-string 1 line)))
		   (file (expand-file-name
			  (format "images/%s" file-base)
			  (xjupyter-elpa-dir)))
		   (exists-p (file-exists-p file)))
	  (xjupyter--overlay-image pos file))))))

(defun xjupyter--cell-render-then-register (pos cell-id)
  "Incur auxiliary state penalty.
Undo's require keeping a restoration closure.  An explicit id (as
opposed to a cell's last known ordinal) will make debugging tractable.

Render first since a delete-insert precisely on top of marker
corrupts (despite marker insertion type being t)."
  (xjupyter--base-buffer
   (save-excursion
     ;; Render
     (with-silent-modifications
       (let* (case-fold-search
	      (inhibit-read-only t)
	      (buffer-undo-list t)
	      (floor (xjupyter-cell-floor pos)))
	 (when-let ((input-beg (xjupyter-input-beg floor))
		    (prompt-beg (progn (goto-char input-beg)
				       (when (zerop (forward-line -1))
					 (point))))
		    (prompt (buffer-substring-no-properties
			     prompt-beg
			     input-beg)))
	   (delete-region prompt-beg input-beg)
	   (goto-char prompt-beg)
	   (insert (apply #'propertize (string-trim-right prompt)
			  xjupyter--prompt-properties)
		   (apply #'propertize "\n" xjupyter--writable-after)))
	 (when-let ((output-beg (xjupyter-output-beg floor))
		    (prompt-beg (progn (goto-char output-beg)
				       (when (zerop (forward-line -1))
					 (point))))
		    (input-newline (1- prompt-beg))
		    (prompt (buffer-substring-no-properties
			     prompt-beg
			     output-beg))
		    (output (buffer-substring-no-properties
			     output-beg
			     (xjupyter-cell-ceiling floor)))
		    ;; for whatever reason, newline missing
		    (output (if (string-empty-p output)
				"\n"
			      output)))
	   (delete-region input-newline (xjupyter-cell-ceiling floor))
	   (goto-char input-newline)
	   (insert (propertize "\n" 'read-only t)
		   (apply #'propertize prompt xjupyter--unvisible-properties)
		   (propertize output 'read-only t))
	   (xjupyter--render-images floor))
	 (let ((ol (make-mode-overlay (xjupyter-input-beg floor)
				      (xjupyter-input-end floor)
				      'python-mode)))
	   (with-current-buffer (overlay-buffer ol)
	     (unless jpr-mode
	       (jpr-mode))
	     (when buffer-read-only
	       (read-only-mode 0)))
	   (overlay-put ol 'face 'xjupyter-input-area-face)
	   (overlay-put ol 'evaporate t) ;self cleaning
	   (overlay-put ol 'category 'xjupyter))))

     ;; Register
     (let* ((marker (set-marker (make-marker) pos))
	    (pair `(,(progn
		       (set-marker-insertion-type marker t)
		       marker)
		    . ,cell-id))
	    (ix (xjupyter--insertion-index (car pair))))
       (if (zerop ix)
	   (push pair xjupyter--cells)
	 (let ((predecessor (nthcdr (1- ix) xjupyter--cells)))
	   ;; splice in
	   (setcdr predecessor (cons pair (cdr predecessor)))))
       pair))))

(defun xjupyter--cell-textualize (pos input output)
  (setq input (if (stringp input)
		  (concat input (unless (string-suffix-p "\n" input) "\n"))
		"\n")
	output (if (stringp output)
		   (concat output (unless (string-suffix-p "\n" output) "\n"))
		 "\n"))
  (xjupyter--base-buffer
   (save-excursion
     (let* ((inhibit-read-only t)
	    (buffer-undo-list t)
	    (index (if (xjupyter-cell-floor pos)
		       (xjupyter--extract-index pos)
		     (if-let ((prev (xjupyter-cell-prev pos)))
			 (1+ (xjupyter--extract-index prev))
		       1)))
	    (floor (or (xjupyter-cell-floor pos)
		       (point-max))))
       (goto-char floor)
       (insert
					;In []
	(format "In [%d]:\n" index)
					;input
	input
					;Out []
	(format "Out [%d]:\n" index)
					;footer
	output)

       (xjupyter--bump-indices floor #'1+)
       floor))))

(defun xjupyter--extract-index (pos)
  "Return bracketed number."
  (save-match-data
    (save-excursion
      (when-let (floor (xjupyter-cell-floor pos))
	(goto-char floor)
	(string-to-number
	 (buffer-substring-no-properties
	  (re-search-forward (regexp-quote "[") (line-end-position))
	  (re-search-forward (regexp-quote "]") (line-end-position))))))))

(defun xjupyter--cell-deregister (pos)
  "Return (REMOVED-ID . INDEX)."
  (xjupyter--base-buffer
   (let ((ix (xjupyter--insertion-index pos)))
     (cons (if (zerop ix)
	       (cdr (pop xjupyter--cells))
	     (let* ((predecessor (nthcdr (1- ix) xjupyter--cells))
		    (removed-pair (car (cdr predecessor)))
		    (removed-id (cdr removed-pair)))
	       (prog1 removed-id
		 ;; splice out
		 (setcdr predecessor (cddr predecessor)))))
	   ix))))

(defun xjupyter--insertion-index (pos)
  (xjupyter--base-buffer
   (let* ((left 0)
	  (right (1- (length xjupyter--cells)))
	  (len (1+ (- right left)))
	  (mid (/ len 2))
	  marker)
     (catch 'done
       (while (<= left right)
	 (setq marker (car (nth mid xjupyter--cells)))
	 (cond ((= pos (marker-position marker))
		(throw 'done nil))
	       ((< pos (marker-position marker))
		(setq right (1- mid)
		      len (1+ (- right left))
		      mid (+ left (/ len 2))))
	       (t
		(setq left (1+ mid)
		      len (1+ (- right left))
		      mid (+ left (/ len 2)))))))
     (max mid 0))))

(defun xjupyter--pre-undo-list ()
  (xjupyter--base-buffer
   (setq xjupyter--pre-undo-list buffer-undo-list)))

(defun xjupyter--relocator (beg)
  (when-let ((floor (xjupyter-cell-floor beg))
	     (ix (xjupyter--insertion-index floor))
	     (pair (nth ix xjupyter--cells)))
    (cons (marker-position (car pair)) (cdr pair))))

(defun xjupyter--annotate-undo ()
  (xjupyter--base-buffer
   (when (cl-tailp xjupyter--pre-undo-list buffer-undo-list)
     (cl-do ((sublist buffer-undo-list (cdr sublist)))
	 ((eq xjupyter--pre-undo-list sublist))
       (pcase (car sublist)
         ;; Element INTEGER sets point.
	 ((pred integerp)
	  (setf (alist-get sublist xjupyter--annotations)
		(xjupyter--relocator (car sublist))))
	 ;; Element (nil PROP VAL BEG . END) is property change.
	 (`(nil ,_prop ,_val ,beg . ,_end)
	  (setf (alist-get sublist xjupyter--annotations)
		(xjupyter--relocator beg)))
	 ;; Element (BEG . END) means range was inserted.
	 (`(,(and beg (pred integerp)) . ,(and _end (pred integerp)))
	  (setf (alist-get sublist xjupyter--annotations)
		(xjupyter--relocator beg)))
	 ;; Element (STRING . POS) means STRING was deleted.
	 (`(,(and _string (pred stringp)) . ,(and pos (pred integerp)))
	  (setf (alist-get sublist xjupyter--annotations)
		(xjupyter--relocator pos))))))))

(defun xjupyter-rpc-sentinel (proc event)
  "Do what now?"
  (unless (string= "open" (substring event 0 4))
    (ignore proc)))

(defun xjupyter-rpc-receive (_beg _end _old-len)
  "Pretty sure execute_reply precedes stream."
  (while-let ((json-message
	       (condition-case nil
		   ;; Moves point to EOM if parsed.
		   (save-excursion
		     (goto-char xjupyter--receive-cursor)
		     (prog1 (json-parse-buffer :object-type 'plist
					       :null-object nil
					       :false-object :json-false)
		       (setq xjupyter--receive-cursor (point))))
		 ;; presumably incomplete message
		 (json-error))))
    (let ((id (plist-get json-message :id))
	  (result (plist-get json-message :result))
	  (buffer (process-get (get-buffer-process (current-buffer))
			       :parent-buffer)))
      (if (null id) ;control channel
	  (let ((inhibit-message t))
	    (message "xjupyter-rpc-receive (no id): %s" result))
	(with-current-buffer buffer
	  (let* ((buffer-undo-list t)
		 (pair (or (rassoc id xjupyter--cells)
			   (user-error "Cell %S killed" id)))
		 (marker (car pair))
		 (extant (buffer-substring (xjupyter-output-beg marker)
					   (xjupyter-cell-ceiling marker))))
	    (cl-case (intern (plist-get result :msg_type))
	      (execute_input)
	      (execute_reply ;shell channel
	       (let ((inhibit-read-only t))
		 (save-excursion
		   ;; restore prompt
		   (goto-char marker)
		   (save-match-data
		     (when (save-excursion (re-search-forward
					    "[0-9]+" (line-end-position) :noerror))
		       (replace-match (apply #'propertize (match-string-no-properties 0)
					     xjupyter--prompt-properties)))))))
	      ((display_data execute_result) ;iopub channel
	       (let* ((inhibit-read-only t)
		      (content (plist-get result :content))
		      (data (plist-get content :data))
		      image-file text)
		 (while data
		   (let* ((kw (car data))
			  (kw-name (substring (symbol-name kw) 1))
			  (splits (split-string kw-name "[/+]"))
			  (prefix (car splits)))
		     (if (equal prefix "image")
			 (let ((buffer-file-coding-system 'binary)
			       (require-final-newline nil))
			   (setq image-file (expand-file-name
					     (format "images/%s.%s" id (cadr splits))
					     (xjupyter-elpa-dir)))
			   (mkdir (file-name-directory image-file) t)
			   (with-temp-file image-file
			     (insert (condition-case nil
					 (base64-decode-string (cadr data))
				       (error (cadr data))))))
		       (when (equal kw-name "text/plain")
			 (setq text (cadr data)))))
		   (setq data (cddr data)))
		 (when (get-text-property 0 'placeholder extant)
		   (delete-region (xjupyter-output-beg marker)
				  (xjupyter-cell-ceiling marker)))
		 (when (or text image-file)
		   (save-excursion
		     (goto-char (xjupyter-cell-ceiling marker))
		     (save-excursion
		       (insert (propertize
				(if image-file
				    (concat "[" (file-name-nondirectory image-file) "]"
					    "(" (or text "") ")"
					    "\n")
				  (concat text (unless (string-suffix-p "\n" text) "\n")))
				'read-only t)))
		     (when image-file
		       (xjupyter--overlay-image (point) image-file))))))
	      (stream ;iopub channel
	       (let* ((inhibit-read-only t)
		      (content (plist-get result :content))
		      (text (plist-get content :text)))
		 (when (get-text-property 0 'placeholder extant)
		   (delete-region (xjupyter-output-beg marker)
				  (xjupyter-cell-ceiling marker)))
		 (save-excursion
		   (goto-char (xjupyter-cell-ceiling marker))
		   (insert (propertize
			    (concat text (unless (string-suffix-p "\n" text) "\n"))
			    'read-only t)))))
	      (error ;iopub channel
	       (let* ((inhibit-read-only t)
		      (content (plist-get result :content))
		      (traceback (plist-get content :traceback)))
		 (when (get-text-property 0 'placeholder extant)
		   (delete-region (xjupyter-output-beg marker)
				  (xjupyter-cell-ceiling marker)))
		 (save-excursion
		   (goto-char (xjupyter-cell-ceiling marker))
		   (mapc (lambda (s)
			   (insert (propertize
				    (concat (ansi-color-apply s) "\n")
				    'read-only t)))
			 traceback))))
	      (status
	       (let* ((inhibit-read-only t)
		      (content (plist-get result :content))
		      (state (plist-get content :execution_state)))
		 (save-excursion
		   ;; restore prompt
		   (goto-char marker)
		   (when (equal state "idle")
		     (save-match-data
		       (when (save-excursion (re-search-forward
					      "[0-9]+" (line-end-position) :noerror))
			 (replace-match (apply #'propertize (match-string-no-properties 0)
					       xjupyter--prompt-properties))))
		     (when (get-text-property 0 'placeholder extant)
		       (goto-char (xjupyter-output-beg marker))
		       (save-excursion (delete-char 1))
		       (insert (propertize "\n" 'read-only t)))))))
	      (t
	       (let ((inhibit-message t))
		 (message "xjupyter-rpc-receive: %s" result))))))))))

(defun xjupyter-elpa-dir ()
  (let ((elpa-dir (directory-file-name (file-name-directory
					(locate-library "xjupyter")))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun xjupyter-rpc-get (&optional buffer no-create)
  "Return pipe of BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (setq buffer (or (buffer-base-buffer buffer) buffer))
  (let* ((proc-buffer (or (buffer-local-value 'xjupyter--rpc-buffer buffer)
			  (with-current-buffer buffer
			    (setq-local xjupyter--rpc-buffer
					(get-buffer-create
					 (format " *%s*" (buffer-name buffer)))))))
	 (proc (get-buffer-process proc-buffer)))
    (when (and (not no-create) (not (process-live-p proc)))
      (with-current-buffer proc-buffer
	(special-mode)
	(let ((inhibit-read-only t))
          (erase-buffer))
	(add-hook 'after-change-functions #'xjupyter-rpc-receive nil :local)
	(setq xjupyter--receive-cursor (point-min)))
      (let* ((elpa-dir (xjupyter-elpa-dir))
             (command (format "%s --log %s"
			      (expand-file-name "bin/app" elpa-dir)
			      (expand-file-name "xjupyter-rpc-log."
						temporary-file-directory)))
	     (stderr-buffer-name (format " *%s-stderr*" (buffer-name buffer))))
	(setq proc (make-process :name (buffer-name buffer)
				 :buffer proc-buffer
				 :command (split-string command)
				 :connection-type 'pipe
				 :noquery t
				 :sentinel #'xjupyter-rpc-sentinel
				 :stderr stderr-buffer-name))
	(process-put proc :parent-buffer buffer)
	(with-current-buffer stderr-buffer-name
	  (special-mode)
	  (let ((inhibit-read-only t))
            (erase-buffer)))))
    proc))

(defun xjupyter-undo (&optional interactive-p)
  "Indirection to skirt the *P interactive spec of undo."
  (interactive "p")
  (when interactive-p
    ;; Stateful undo keys off this-command
    (setq this-command 'undo))
  (undo))

(defun xjupyter-rpc-request (cell-id kwargs method &rest args)
  "Request with generator KWARGS calling METHOD ARGS."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (if cell-id
      (xjupyter--base-buffer
       (let ((inhibit-read-only t)
	     (buffer-undo-list t)
	     (marker (car (rassoc cell-id xjupyter--cells))))
	 (save-excursion ;change prompt to asterisk
	   (goto-char marker)
	   (save-match-data
	     (when (save-excursion (re-search-forward
				    "[0-9]+" (line-end-position) :noerror))
	       (replace-match (apply #'propertize (match-string-no-properties 0)
				     (append '(display "*")
					     xjupyter--prompt-properties))))))
	 (save-excursion
	   ;; delete extant output.
	   (delete-region (xjupyter-output-beg marker)
			  (xjupyter-cell-ceiling marker))
	   (goto-char (xjupyter-output-beg marker))
	   (insert (propertize "\n" 'read-only t 'placeholder t)))))
    (setq cell-id "0"))
  (let ((pipe (xjupyter-rpc-get))
        (request `(:jsonrpc "2.0"
		   :method ,method
		   :id ,cell-id
		   :params (:args ,(apply json-array-type args)
			    :kwargs ,kwargs))))
    (with-current-buffer (process-buffer pipe)
      (process-send-string pipe (concat (json-encode request) "\n")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:jpr\\)\\'" . jpr-mode))

(let* ((focus (lambda (&rest _args) (xjupyter-cell-focus (point))))
       (nah (lambda (&optional beg end)
	      (when (or beg end)
		(setq this-command nil)
		(user-error "undo-in-region: don't make me do that"))))
       (deactivate
	(lambda ()
	  (remove-function (symbol-function 'primitive-undo) #'xjupyter--undo-n)
	  (remove-function (symbol-function 'undo) focus)
	  (remove-function (symbol-function 'undo-start) nah))))
  (defalias 'xjupyter--unadvise-undo deactivate)
  (defalias 'xjupyter--advise-undo
    (lambda ()
      (funcall deactivate)
      (add-function :override (symbol-function 'primitive-undo) #'xjupyter--undo-n)
      (add-function :before (symbol-function 'undo-start) nah)
      (add-function :after (symbol-function 'undo) focus))))

(provide 'xjupyter)

;;; xjupyter.el ends here
