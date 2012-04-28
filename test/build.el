;;; build.el --- testing for plaintalk  -*- lexical-binding: t -*-

(require 'elnode)
(require 'phantomjs)

;; for now
(load-file "~/work/plaintalk/lisp/talk.el")

(defun plaintalk-handler (httpcon)
  (let ((webserver (elnode-webserver-handler-maker "~/work/plaintalk/")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("[^/]+/talk/to/$" . talk-handler)
       ("[^/]+/talk/stuff/\\(.*\\)" . ,webserver))
     :log-name "plaintalk")))

(defun plaintalk-test-phantom-complete ()
  (interactive)
  (elnode-stop 8005)
  (message "test run done"))

(defun plaintalk--cb (status arg)
  (unless (equal (string-to-number status) 200)
    (message "some error handling page - status: %s" status)))

(defun plaintalk-test-run ()
  (interactive)
  (elnode-start 'plaintalk-handler :port 8005 :defer-mode :immediate)
  (let* (web1
         web2
         (page "http://localhost:8005/talk/stuff/html/index.html")
         ;; Setup an environment ... 3 people ...
         (p1 (make-talk-person :id "u33223"))
         (p2 (make-talk-person :id "u243619"))
         (p3 (make-talk-person :id "u2421313"))
         ;; ... one conversation ....
         (c1 (make-talk-conversation
              :people (talk-hash
                       (talk-person-id p1) p1
                       (talk-person-id p2) p2
                       (talk-person-id p3) p3)))
         ;; ... added to the list of global convs ....
         (let-talk-s (talk-hash "1" c1)))
    (setq talk-s let-talk-s)
    ;; ... distribute a pending update from one user ...
    (talk--distrib-pending "1" "u243619" "test text")
    ;; ... and now test with phantom
    (setq web1
          (phantomjs-server
           'servertest 6101
           'plaintalk-test-phantom-complete))
    (sleep-for 2)
    (phantomjs-open web1 page)
    (phantomjs-call web1 "plaintalk.init('u33223', '1')")
    (phantomjs-call web1 "plaintalk.comm()")
    (phantomjs-exit web1)))

;; Actually phantomjs does NOT support flash... so this does not work properly
(defun plaintalk-test-video-run ()
  (interactive)
  (elnode-start 'plaintalk-handler :port 8005 :defer-mode :immediate)
  (let* (web1
         web2
         (page "http://localhost:8005/talk/stuff/html/videotest.html"))
    ;; ... and now test with phantom
    (setq web1
          (phantomjs-server
           'servertest 6101
           'plaintalk-test-phantom-complete))
    (sleep-for 2)
    (phantomjs-open web1 page)
    (sleep-for 2)
    (phantomjs-call
     web1
     (concat
      "swfobject.getObjectById('video')"
      ".camera_select('Webcam (V4L2)')"))
    (phantomjs-exit web1)))

;; End
