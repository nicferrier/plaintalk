;;; talk.el --- elnode handler for plaintalk  -*- lexical-binding: t -*-

;;; Commentary:

;; The plaintalk chat service.

;;; Code:

(require 'elnode)
(require 'cl)
(require 'ert)
(require 'json)

(defmacro talk-hash (&rest keyval-pairs)
  "Constructuor for hashes because the read syntax is bad."
  (declare (debug (&rest form)))
  (let ((hashv (make-symbol "hash")))
    `(let ((,hashv (make-hash-table :test 'equal)))
       (loop for p in (loop for (k v)
                            on (list ,@keyval-pairs)
                            by 'cddr
                            collecting (list k v))
             do
             (puthash (car p) (cadr p) ,hashv))
       ,hashv)))

(ert-deftest talk-hash-init ()
  "Test the `talk-hash' macro for initing hashes."
  (let ((h (talk-hash "u123" "nic" "u21312" "tim")))
    (should (equal "nic" (gethash "u123" h)))
    (should (equal "tim" (gethash "u21312" h)))))


;; talk-conversation-people contain these types
(defstruct talk-person
  id
  (pending (list)))

;; The type of a what is in `talk-s'
(defstruct talk-conversation
  (people (talk-hash :test 'equal)))

(defvar talk-s (talk-hash :test 'equal)
  "The list of conversations.")

(defun talk--pending-p (conversation-id user-id)
  "Is anything pending on the COVERSATION-ID for the USER-ID?"
  (let* ((talk (gethash conversation-id talk-s))
         (person (gethash user-id talk)))
    (talk-person-pending person)))

(defun talk--pending-add (to-user from-user text)
  "Add the pending TEXT to TO-USER from FROM-USER."
  (setf (talk-person-pending to-user)
        (list
         (cons (talk-person-id from-user)
               text))))

(ert-deftest talk-person-fields ()
  "Test the `talk-person' structure."
  (let ((p1 (make-talk-person :id "u72131"))
        (p2 (make-talk-person :id "u3522")))
    (should (equal "u72131" (talk-person-id p1)))
    (should (equal nil (talk-person-pending p1)))
    (talk--pending-add p1 p2 "this is an update")
    (should (equal "u3522" (caar (talk-person-pending p1))))
    (pop (talk-person-pending p1))
    (should-not (talk-person-pending p1))))

(defun talk--pending-get (conversation-id user)
  "JSON encode the pending for the USER in CONVERSATION-ID."
  (let* ((talk (gethash conversation-id talk-s))
         (person
          (gethash
           user
           (talk-conversation-people talk)))
         to-send)
    (while (talk-person-pending person)
      (setq
       to-send
       (append
        (list (pop (talk-person-pending person)))
        to-send)))
    (json-encode-list to-send)))

(ert-deftest talk--pending-get ()
  "Test the JSON produced by `talk--pending-get'."
  (let* ((p1 (make-talk-person :id "u33223"))
         (p2 (make-talk-person :id "u243619"))
         (c1 (make-talk-conversation
              :people (talk-hash
                       (talk-person-id p1) p1
                       (talk-person-id p2) p2)))
         (talk-s (talk-hash "1" c1)))
    ;; Add something pending
    (talk--pending-add p1 p2 "moar text moar")
    (should
     (equal
      (cons 'u243619 "moar text moar")
      (car (json-read-from-string (talk--pending-get "1" "u33223")))))
    ;; Now with no pending, what happens?
    (should
     (equal
      '()
      (car (json-read-from-string (talk--pending-get "1" "u33223")))))))

(defun talk--distrib-pending (conversation from text)
  "Add TEXT from FROM to all the waiting people in CONVERSATION"
  (let* ((talk (gethash conversation talk-s))
         (sender (gethash from (talk-conversation-people talk))))
    (maphash
     (lambda (id person)
       (unless (equal person sender)
         (talk--pending-add person sender text)))
     (talk-conversation-people talk))))

(ert-deftest talk--distrib-pending ()
  "Test the distribution of pendings."
  (let* ((p1 (make-talk-person :id "u33223"))
         (p2 (make-talk-person :id "u243619"))
         (p3 (make-talk-person :id "u2421313"))
         (c1 (make-talk-conversation
              :people (talk-hash
                       (talk-person-id p1) p1
                       (talk-person-id p2) p2
                       (talk-person-id p3) p3)))
         (talk-s (talk-hash "1" c1)))
    (talk--distrib-pending "1" "u243619" "test text")
    ;; Check directly.
    (should
     (equal '("u243619" . "test text")
            (car (talk-person-pending p1))))
    (should
     (equal '("u243619" . "test text")
            (car (talk-person-pending p3))))
    ;; p2 is the sender
    (should-not
     (equal '("u243619" . "test text")
            (car (talk-person-pending p2))))
    ;; Check with get-pending
    (should
     (equal
      (cons 'u243619 "test text")
      (car
       (json-read-from-string
        (talk--pending-get "1" (talk-person-id p1))))))))

(defun talk-handler (httpcon)
  "Handle the plaintalk web service.

From the HTTPCON we get: `conversation-id' from the path match
and `user-id' and `text' from the parameters."
  (let ((submit (elnode-http-param httpcon "submit")))
    (if submit
        (let* ((converation-id (elnode-http-pathinfo httpcon))
               (user (elnode-http-param httpcon "userid"))
               (text (elnode-http-param httpcon "text")))
          (gethash conversation-id talk-s)
          (talk--distrib-pending conversation-id user text)))
    (elnode-defer-or-do
      (talk--pending-p conversation-id user)
      (let ((json-to-send (talk--pending-get conversation-id user)))
        (elnode-send-json httpcon json-to-send)))))

(provide 'talk)

;;; talk.el ends here
