;;; phantom.el --- handle phantomjs

;;; Commentary:

;; Deal with phantomjs.

;;; Code:

(require 'cl)

(defgroup phantomjs nil
  "Customizations for phantomjs handling."
  :group 'applications)
(makunbound 'phantomjs-home)

(defcustom phantomjs-home (expand-file-name "~/phantomjs")
  "The location of phantomjs."
  :group 'phantomjs
  :type 'string
  :set (lambda (name value)
         (setq name (expand-file-name value))))

(defvar phantomjs--proc
  (make-hash-table)
  "Hash of name->process object.")

(defun phantomjs--sentinel (proc status)
  "The sentinel for the phantomjs stuff.

Mostly exists for calling the end process callback."
  (cond
   ((member status '("killed"
                     "finished\n"
                     "exited abnormally with code 255\n"))
    (when (functionp (process-get proc :phantomjs-end-callback))
      (funcall (process-get proc :phantomjs-end-callback))))
   (t
    (message "phantom unexpected status: %s" status))))


(defun phantomjs-run (name callback &rest scripts)
  (interactive)
  "Run phantomjs process with NAME.

CALLBACK is called when it completes.

SCRIPTS is a list of scripts to be passed to the process."
  ;; need to check phantomjs--proc for name clash
  (let ((proc
         (apply
          'start-process
          (append
           (list
            (symbol-name name)
            (concat "* phantomjs-" (symbol-name name) " *")
            (expand-file-name "~/phantomjs.git/bin/phantomjs"))
           (loop for script in scripts
                 collect (if (string-match "^http:.*" script)
                             script
                           (expand-file-name script)))))))
    (process-put proc :phantomjs-end-callback callback)
    (set-process-sentinel proc 'phantomjs--sentinel)
    (puthash name proc phantomjs--proc)))

(provide 'phantom)

;;; phantom.el ends here
