(defun python-split-arguments (arg-string)
  "Split a python argument string into ((name, default)..) tuples."
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-arguments-to-sphinx-doc-string ()
  "Format arguments into a Sphinx (reST) style Python docstring."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (arguments (python-split-arguments yas-text))
         (text (mapconcat
                (lambda (x)
                  (concat ":arg XXX " (nth 0 x) ": XXX"))
                arguments
                indent)))
    (unless (string= text "")
      (mapconcat 'identity (list "" text) indent))))

(defun python-arguments-to-numpy-doc-string ()
  "Format arguments into a NumPy style Python docstring."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (arguments (python-split-arguments yas-text))
         (text (mapconcat
                (lambda (x)
                  (concat (nth 0 x) " : XXX" (if (nth 1 x) ", optional")
                          (concat indent "    XXX." )))
                arguments
                indent)))
    (unless (string= text "")
      (mapconcat 'identity
                 (list "" "Parameters" "----------" text) indent))))

(defun python-arguments-to-google-doc-string ()
  "Format arguments into a Google style Python docstring."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (arguments (python-split-arguments yas-text))
         (text (mapconcat
                (lambda (x)
                  (concat "   " (nth 0 x)
                          (if (nth 1 x) " (Optional[XXX]): XXX."
                            " (XXX): XXX.")))
                arguments
                indent)))
    (unless (string= text "")
       (mapconcat 'identity
                  (list "" "Arguments:" text) indent))))

(defun python-arguments-to-doc-string ()
  (python-arguments-to-numpy-doc-string))

(defun python-arguments-to-self-init-string ()
  "Format each argument into 'self.argument = argument' statements."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (arguments (python-split-arguments yas-text))
         (text (mapconcat
                (lambda (x)
                  (concat "self." (nth 0 x) " = " (nth 0 x)))
                arguments
                indent)))
    (unless (string= text "")
      (mapconcat 'identity (list text "") indent))))

;;; Begin Variables:
;;; mode: emacs-lisp
;;; End:
