;;; xjupyter-package.el --- because package.el sucks ass  -*- lexical-binding:t -*-

(require 'package)
(require 'project)

(defsubst xjupyter-package-where ()
  (directory-file-name (expand-file-name (project-root (project-current)))))

(defsubst xjupyter-package-desc ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      "lisp/xjupyter.el"
      (xjupyter-package-where)))
    (package-buffer-info)))

(defun xjupyter-package-name ()
  (concat "xjupyter-" (package-version-join
		       (package-desc-version
			(xjupyter-package-desc)))))

(defun xjupyter-package-inception ()
  "To get a -pkg.el file, you need to run `package-unpack'.
To run `package-unpack', you need a -pkg.el."
  (let ((pkg-desc (xjupyter-package-desc))
	(pkg-dir (expand-file-name (xjupyter-package-name)
				   (xjupyter-package-where))))
    (ignore-errors (delete-directory pkg-dir t))
    (make-directory pkg-dir t)
    (copy-file (expand-file-name
		"lisp/xjupyter.el"
		(xjupyter-package-where))
	       (expand-file-name "xjupyter.el" pkg-dir))
    (package--make-autoloads-and-stuff pkg-desc pkg-dir)))
