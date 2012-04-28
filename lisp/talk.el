;;; talk.el --- elnode handler for plaintalk  -*- lexical-binding: t -*-

;;; Commentary:

;; The plaintalk chat service.

;; The basic idea is that people register their name, another name
;; (their friend) and get back an id for each of them and a conversation id.

;; The conversation id is used as the unique record of the
;; conversation.  The other ids are used to refer to users.

;; Conversations keep the pending list of chat which users have sent
;; but has not been delivered to other users yet.

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
  (pending (list))
  (name ""))

;; The type of a what is in `talk-s'
(defstruct talk-conversation
  (people (talk-hash :test 'equal)))

(defvar talk-s (talk-hash :test 'equal)
  "The list of conversations.")

(defvar talk-people (talk-hash :test 'equal)
  "List of people registered.")

(defun talk--pending-p (conversation-id user-id)
  "Is anything pending on the COVERSATION-ID for the USER-ID?"
  (let* ((talk (gethash conversation-id talk-s))
         (person
          (gethash
           user-id
           (talk-conversation-people talk))))
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

From the HTTPCON we get parameters: `conversation-id', `user-id'
and `text'."
  (elnode-error "talk-handler got %s" httpcon)
  (let ((submit (elnode-http-param httpcon "submit")))
    (if submit
        (let* ((conversation-id (elnode-http-pathinfo httpcon))
               (user (elnode-http-param httpcon "userid"))
               (text (elnode-http-param httpcon "text")))
          (gethash conversation-id talk-s)
          (talk--distrib-pending conversation-id user text)))
    (let* ((conversation-id (elnode-http-param httpcon "c"))
           (user (elnode-http-param httpcon "userid")))
      (elnode-defer-or-do
        (talk--pending-p conversation-id user)
        (let ((json-to-send (talk--pending-get conversation-id user)))
          (elnode-send-json httpcon json-to-send))))))

(defun talk--uuid ()
  "Get a UUID from linux."
  (elnode-trim (shell-command-to-string "cat /proc/sys/kernel/random/uuid")))

(defun talk-init-handler (httpcon)
  "Initiate state for conversations and users.

Makes unique ids for a 'me' and a 'them' user and for their
conversation."
  (let* ((me (elnode-http-param httpcon "me"))
         (them (elnode-http-param httpcon "them"))
         (me-person (make-talk-person :id (talk--uuid)
                                      :name me))
         (them-person (make-talk-person :id (talk--uuid)
                                        :name them))
         (conversation-id (talk--uuid))
         (tbl (make-hash-table :test 'equal)))
    ;; Store the 2 people objects
    (puthash (talk-person-id me-person) me-person talk-people)
    (puthash (talk-person-id them-person) them-person talk-people)
    ;; Store the conversation
    (puthash conversation-id
             (make-talk-conversation
              :people (talk-hash
                       (talk-person-id me-person) me-person
                       (talk-person-id them-person) them-person))
             talk-s)
    ;; Make the response
    (puthash me (talk-person-id me-person) tbl)
    (puthash them (talk-person-id them-person) tbl)
    (puthash "_id" conversation-id tbl)
    (elnode-send-json httpcon tbl)))

(provide 'talk)

;;; talk.el ends here
