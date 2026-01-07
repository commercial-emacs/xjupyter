;;; test-xjupyter.el --- Tests for xjupyter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 commandlinesystems.com

;;; Commentary:

;; Test stuff.

;;; Code:

(require 'xjupyter)

(defun do-command (command)
  "eval-expression prin1 results of undo-more."
  (cl-letf (((symbol-function 'prin1) #'ignore))
    (execute-kbd-macro (read-kbd-macro command))
    (undo-boundary)))

(defun wait-for ()
  (save-excursion
    (goto-char (xjupyter-output-beg (point)))
    (while (looking-at-p (regexp-quote "\n"))
      (accept-process-output nil 0.5))))

(ert-deftest basic ()
  (let ((file (concat (make-temp-name "foo") ".jpr")))
    (with-current-buffer (find-file file)
      (should jpr-mode))
    (set-buffer file)
    (should-not (file-exists-p file))
    (should (= 1 (length xjupyter--cells)))
    (should-error (do-command "M-x undo RET"))
    (should (buffer-base-buffer)) ;should be in python
    (call-interactively #'xjupyter-cell-kill)
    (should (buffer-base-buffer)) ;should remain in python
    (should buffer-read-only)
    (call-interactively #'xjupyter-cell-append)
    (should (buffer-base-buffer)) ;should remain in python
    (should-not buffer-read-only)
    (call-interactively #'xjupyter-cell-append)
    (do-command "M-: (insert \"'b'\")")
    (call-interactively #'xjupyter-cell-up)
    (do-command "M-: (insert \"print('a\\\\n')\")")
    (call-interactively #'xjupyter-cell-evaluate)
    (wait-for)
    (set-mark (line-beginning-position))
    (goto-char (line-end-position))
    (activate-mark)
    (should (region-active-p))
    (should (equal "print('a\\n')"
		   (buffer-substring-no-properties (region-beginning)
						   (region-end))))
    (should-error (do-command "M-x undo RET"))
    (deactivate-mark)
    (do-command "M-x undo RET")
    (do-command "M-: (undo-more SPC 1)")
    (should (and (looking-at-p (regexp-quote "\n"))
		 (= 1 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (let (kill-buffer-query-functions)
      (kill-buffer))
    (should-not (file-exists-p file))))

(ert-deftest move ()
  (let ((file (concat (make-temp-name "foo") ".jpr")))
    (with-current-buffer (find-file file)
      (should jpr-mode))
    (set-buffer file)
    (should-not (file-exists-p file))
    (should (= 1 (xjupyter--base-buffer (length xjupyter--cells))))
    (do-command "M-: (insert \"'a'\")")
    (call-interactively #'xjupyter-cell-evaluate)
    (wait-for)
    (call-interactively #'xjupyter-cell-append)
    (do-command "M-: (insert \"'b'\")")
    (call-interactively #'xjupyter-cell-evaluate)
    (wait-for)
    (move-beginning-of-line nil)
    (should (and (looking-at-p (regexp-quote "'b'"))
		 (= 1 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (call-interactively #'xjupyter-cell-move-up)
    (should (and (looking-at-p (regexp-quote "'b'"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (do-command "M-x undo RET")
    (should (and (looking-at-p (regexp-quote "'b'"))
		 (= 1 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (do-command "M-x undo RET")
    (should (and (looking-at-p (regexp-quote "'b'"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (let (kill-buffer-query-functions)
      (kill-buffer))
    (should-not (file-exists-p file))))

(ert-deftest split-merge ()
  (let ((file (concat (make-temp-name "foo") ".jpr")))
    (with-current-buffer (find-file file)
      (should jpr-mode))
    (set-buffer file)
    (should-not (file-exists-p file))
    (do-command "M-: (insert \"'a'\")")
    (do-command "M-x xjupyter-cell-evaluate RET")
    (wait-for)
    (do-command "M-x xjupyter-cell-append RET")
    (do-command "M-: (insert \"'b'\")")
    (do-command "M-x xjupyter-cell-evaluate RET")
    (wait-for)
    (should-error (do-command "M-x xjupyter-cell-merge RET"))
    (do-command "M-x xjupyter-cell-up RET")
    (do-command "M-x xjupyter-cell-merge RET")
    (should-error (do-command "M-x xjupyter-cell-merge RET"))
    (re-search-forward "'b'")
    (call-interactively #'move-beginning-of-line)
    (do-command "M-x xjupyter-cell-split RET")
    (should (and (looking-at-p (regexp-quote "'b'"))
		 (= 1 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (do-command "M-x undo RET")
    (should (and (looking-at-p (regexp-quote "'a'"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))
		 (= 1 (xjupyter--base-buffer (length xjupyter--cells)))))
    (do-command "M-: (undo-more SPC 1)") ;undo merge
    (should (and (looking-at-p (regexp-quote "'a'"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))
		 (= 2 (xjupyter--base-buffer (length xjupyter--cells)))))
    (do-command "M-: (undo-more SPC 1)") ;undo 'b'
    (should (and (looking-at-p (regexp-quote "\n"))
		 (= 1 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))
		 (= 2 (xjupyter--base-buffer (length xjupyter--cells)))))
    (do-command "M-: (undo-more SPC 1)") ;undo append
    (should (and (eq (point) (point-max))
		 (= 1 (xjupyter--base-buffer (length xjupyter--cells)))))
    (do-command "M-: (undo-more SPC 1)") ;undo 'a'
    (should (and (looking-at-p (regexp-quote "\n"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))
		 (= 1 (xjupyter--base-buffer (length xjupyter--cells)))))
    (let (kill-buffer-query-functions)
      (kill-buffer))
    (should-not (file-exists-p file))))

(ert-deftest kill-yank ()
  (let ((file (concat (make-temp-name "foo") ".jpr")))
    (with-current-buffer (find-file file)
      (should jpr-mode))
    (set-buffer file)
    (should-not (file-exists-p file))
    (do-command "M-: (insert \"'a'\")")
    (do-command "M-x xjupyter-cell-evaluate RET")
    (wait-for)
    (do-command "M-x xjupyter-cell-append RET")
    (do-command "M-: (insert \"'b'\")")
    (do-command "M-x xjupyter-cell-evaluate RET")
    (wait-for)
    (should-error (do-command "M-x xjupyter-cell-yank RET"))
    (do-command "M-x xjupyter-cell-kill RET")
    (should (= 1 (xjupyter--base-buffer (length xjupyter--cells))))
    (do-command "M-x xjupyter-cell-up RET")
    (do-command "M-x xjupyter-cell-kill RET")
    (should (= 0 (xjupyter--base-buffer (length xjupyter--cells))))
    (do-command "M-x xjupyter-cell-yank RET")
    (should (and (looking-at-p (regexp-quote "'a'"))
		 (= 0 (xjupyter--insertion-index
		       (xjupyter-cell-floor (point))))))
    (let ((advice (lambda (&rest _args)
		    (setq last-command 'xjupyter-cell-yank-pop))))
      (add-function :before (symbol-function 'xjupyter-cell-yank-pop) advice)
      (unwind-protect
	  (progn
	    (do-command "M-x xjupyter-cell-yank-pop RET")
	    (should (and (looking-at-p (regexp-quote "'b'"))
			 (= 0 (xjupyter--insertion-index
			       (xjupyter-cell-floor (point))))))
	    (do-command "M-x xjupyter-cell-yank-pop RET")
	    (should (and (looking-at-p (regexp-quote "'a'"))
			 (= 0 (xjupyter--insertion-index
			       (xjupyter-cell-floor (point))))))
	    (do-command "M-x undo RET")
	    (should (and (looking-at-p (regexp-quote "'b'"))
			 (= 0 (xjupyter--insertion-index
			       (xjupyter-cell-floor (point)))))))
	(remove-function (symbol-function 'xjupyter-cell-yank-pop) advice)))
    (let (kill-buffer-query-functions)
      (kill-buffer))
    (should-not (file-exists-p file))))

(ert-deftest image ()
  (let ((file (concat (make-temp-name "foo") ".jpr")))
    (with-current-buffer (find-file file)
      (should jpr-mode))
    (set-buffer file)
    (should-not (file-exists-p file))
    (let* ((code "import matplotlib.pyplot as plt
X = range(10)
plt.plot(X, [x*x for x in X])
plt.show()")
	   (kbd-macro (replace-regexp-in-string
		       (regexp-quote "\n")
		       "\\\\n"
		       (replace-regexp-in-string
			(regexp-quote " ") " SPC " code))))
      (do-command (format "M-: (insert \"%s\")" kbd-macro)))
    (call-interactively #'xjupyter-cell-evaluate)
    (wait-for)
    (save-excursion
      (goto-char (xjupyter-output-beg (point)))
      (should-not (get-text-property (point) 'display))
      (should (looking-at-p xjupyter--image-regexp)))
    (call-interactively #'xjupyter-cell-toggle-output)
    (should (get-text-property (xjupyter-output-beg (point)) 'display))
    (call-interactively #'xjupyter-cell-toggle-output)
    (should-not (get-text-property (xjupyter-output-beg (point)) 'display))
    (let (kill-buffer-query-functions)
      (kill-buffer))
    (should-not (file-exists-p file))))

(provide 'test-xjupyter)

;;; test-xjupyter.el ends here
