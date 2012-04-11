;; run the tests for plaintalk

(require 'elnode)
;;(require 'phantom)

(defun plaintalk-handler (httpcon)
  (let ((webserver (elnode-webserver-handler-maker "~/work/plaintalk/")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("[^/]+/talk/stuff/\\(.*\\)" . ,webserver))
     :log-name "plaintalk")))

(progn
  (elnode-start 'plaintalk-handler 8005)
  (phantomjs-run
   'test
   (lambda ()
     (elnode-stop 8005)
     (message "test run ended!"))
   "~/work/plaintalk/test/talk.js"))

;; End
