;; run the tests for plaintalk

(require 'elnode)
(require 'phantom)

;; for now
(load-file "~/work/plaintalk/lisp/talk.el")

(defun plaintalk-handler (httpcon)
  (let ((webserver (elnode-webserver-handler-maker "~/work/plaintalk/")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("[^/]+/talk/to/$" . talk-handler)
       ("[^/]+/talk/stuff/\\(.*\\)" . ,webserver))
     :log-name "plaintalk")))

(progn
  (elnode-start 'plaintalk-handler :port 8005 :defer-mode :immediate)
  (let* ((p1 (make-talk-person :id "u33223"))
         (p2 (make-talk-person :id "u243619"))
         (p3 (make-talk-person :id "u2421313"))
         (c1 (make-talk-conversation
              :people (talk-hash
                       (talk-person-id p1) p1
                       (talk-person-id p2) p2
                       (talk-person-id p3) p3)))
         (let-talk-s (talk-hash "1" c1)))
    (setq talk-s let-talk-s)
    (talk--distrib-pending "1" "u243619" "test text")
    (phantomjs-run
     'test
     ;; this is the phantom completion handler
     ;; it's only called when phantom is done
     (lambda ()
       (elnode-stop 8005)
       (message "test run ended!"))
     "~/work/plaintalk/test/talk.js")))

;; (elnode--deferred-processor)

;; (gethash "1" talk-s)
;; (gethash
;;  "u33223"
;;  (talk-conversation-people (gethash "1" talk-s)))
;; (talk--pending-p "1" "u33223")

;; End
