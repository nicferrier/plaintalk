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
(require 'eieio)

(defun talk-filter-hash-2-list (fn hash)
  "Filter a HASH to a list.

Execute the function FN with `key' and `value' for each entry in
the HASH and return a list of all the `key', `value' pairs which
returned non-nil."
  (let ((reslst '()))
    (maphash
     (lambda (key data)
       (let ((ret (funcall fn key data)))
         (if ret
             (if (not reslst)
                 (setq reslst (list (cons key data)))
               (nconc reslst (list (cons key data)))))))
     hash)
    reslst))

(defun talk--uuid ()
  "Get a UUID from linux."
  (elnode-trim (shell-command-to-string "cat /proc/sys/kernel/random/uuid")))

(defclass talk-personc nil
  ((id
    :initarg :id
    :initform (lambda () (talk--uuid))
    :documentation "The id of the person")
   (pending
    :type list
    :initform '()
    :documentation "The list of pending chat messages")
   (name
    :type string
    :initform ""
    :documentation "The name of the person"))
  "A person record.")

(defclass talk-conversationc nil
  ((people
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "The hash of people in the conversation")
   (joined
    :type hash-table
    :initform (make-hash-table :test 'equal)
    :documentation "The hash of people who have joined"))
  "Conversation type.")

(defvar talk-s (make-hash-table :test 'equal)
  "The list of conversations.")

(defvar talk-people (make-hash-table :test 'equal)
  "List of people registered.

When a person is created in a conversation we also register them
here.  Could be a useful way to serialize everyone to a
database.")

(defun talk-s (id)
  "Retrieve the conversation with ID from the collection.

If the conversation for ID does not exist then make it."
  (or (gethash id talk-s)
      (puthash id (talk-conversationc id) talk-s)))

(defun* talk--person (conversation
                      &key
                      person-id
                      key value)
  "Get or make a person from the CONVERSATION.

CONVERSATION is either an id or a conversation object.

If PERSON-ID is specified then use that ID to make or set a
person.

If the person associated with PERSON-ID does not exist then
create them.

If KEY is supplied then just return that value, if VALUE is
supplied then set KEY to that VALUE."
  (let* ((conv (if (talk-conversationc-p conversation)
                   conversation
                 (talk-s conversation)))
         (person
          (or (gethash person-id (oref conversation people))
              (let ((person-id (or person-id (talk--uuid))))
                (puthash person-id
                         (talk-personc person-id :id person-id)
                         (oref conversation people))))))
    (if (not key)
        person
      (if (not value)
          (slot-value person key)
        (set-slot-value person key value)
        person))))

(ert-deftest talk--person ()
  "Test the person convieniance method."
  (let ((conv (talk-conversationc "talks")))
    ;; Making
    (should
     (equal
      "1213"
      (oref (talk--person conv :person-id "1213") id)))
    ;; Getting
    (should
     (equal
      "1213"
      (talk--person conv :person-id "1213" :key 'id)))
    ;; Setting
    (let ((person (talk--person conv :person-id "1213")))
      (should
       (equal
        "1213"
        (oref person id)))
      (talk--person conv :person-id "1213" :key 'id :value "2314")
      (should
       (equal
        "2314"
        (oref person id))))
    ;; Making without an id
    (let ((person (talk--person conv :key 'name :value "nic")))
      (should
       (oref person id))
      (should
       (equal
        "nic"
        (oref person name))))))

(defun talk--pending-p (conversation-id user-id)
  "Is anything pending on the COVERSATION-ID for the USER-ID?"
  (talk--person (talk-s conversation-id) :person-id user-id :key 'pending))

(defmethod talk--pending-add ((to-user talk-personc) from-user text)
  "Add the pending TEXT to TO-USER from FROM-USER.

FROM-USER can either be a string or a `talk-personc'."
  (oset to-user pending
        (list
         (cons
          (cond
           ((talk-personc-p from-user)
            (oref from-user id))
           (t from-user))
          text))))

(ert-deftest talk--pending-add ()
  (let* ((talk-s (make-hash-table :test 'equal))
         (person (talk--person (talk-s "10") :person-id "1123")))
    (should-not (oref person pending))
    (should-not (talk--pending-p "10" "1123"))
    (talk--pending-add person "1125" "hello there")
    (should (talk--pending-p "10" "1123"))
    (should (talk--person (talk-s "10") :person-id "1123" :key 'pending))
    (pop (slot-value person 'pending))
    (should-not (talk--pending-p "10" "1123"))))

(defun talk--pending-get (conversation-id user-id)
  "JSON encode the pending for the USER-ID in CONVERSATION-ID."
  (let ((person (talk--person (talk-s conversation-id) :person-id user-id))
        to-send)
    (while (oref person pending)
      (setq
       to-send
       (append
        (list (pop (slot-value person 'pending)))
        to-send)))
    (json-encode-list to-send)))

(ert-deftest talk--pending-get ()
  "Test the JSON produced by `talk--pending-get'."
  (let* ((json-key-type 'string)
         (talk-s (make-hash-table :test 'equal))
         (p1 (talk--person (talk-s "10") :person-id "u33223"))
         (p2 (talk--person (talk-s "10") :person-id "u243619")))
    ;; Add something pending
    (talk--pending-add p1 p2 "moar text moar")
    (should
     (equal
      (cons "u243619" "moar text moar")
      (car (json-read-from-string (talk--pending-get "10" "u33223")))))
    ;; Now with no pending, what happens?
    (should
     (equal
      '()
      (car (json-read-from-string (talk--pending-get "10" "u33223")))))))

(defun talk-other (conversation-id my-id)
  "Return the ids of people in the conversation who are not MY-ID.

The conversation is identified by CONVERSATION-ID."
  (let* ((conversation (talk-s conversation-id))
         (not-me
          (talk-filter-hash-2-list
           (lambda (key value)
             (not (equal key my-id)))
           (oref conversation people)))
         (other-user (car not-me)))
    other-user))

(ert-deftest talk-other ()
  "Test that we get the other person back."
  (let* ((talk-s (make-hash-table :test 'equal))
         (p1 (talk--person (talk-s "10") :person-id "u33223"))
         (p2 (talk--person (talk-s "10") :person-id "u243619")))
    (should
     (equal "u243619" (car (talk-other "10" "u33223"))))))

(defun talk--distrib-pending (conversation-id from-id text)
  "Add TEXT from FROM-ID to all the waiting people.

The CONVERSATION-ID identifies the conversation."
  (let* ((talk (talk-s conversation-id)))
    (maphash
     (lambda (id person)
       (unless (equal (oref person id) from-id)
         (talk--pending-add person from-id text)))
     (oref talk people))))

(ert-deftest talk--distrib-pending ()
  "Test the distribution of pendings."
  (let* ((json-key-type 'string)
         (talk-s (make-hash-table :test 'equal))
         (p1 (talk--person (talk-s "10") :person-id "u33223"))
         (p2 (talk--person (talk-s "10") :person-id "u243619"))
         (p3 (talk--person (talk-s "10") :person-id "u2421313")))
    (talk--distrib-pending "10" "u243619" "test text")
    ;; Check directly.
    (should
     (equal '("u243619" . "test text")
            (car (oref p1 pending))))
    (should
     (equal '("u243619" . "test text")
            (car (oref p3 pending))))
    ;; p2 is the sender
    (should-not
     (equal '("u243619" . "test text")
            (car (oref p2 pending))))
    ;; Check with get-pending
    (should
     (equal
      (cons "u243619" "test text")
      (car
       (json-read-from-string
        (talk--pending-get "10" "u33223")))))))

(defun talk-handler (httpcon)
  "Handle the plaintalk web service.

From the HTTPCON we get parameters: `conversation-id', `user-id'
and `text'."
  (elnode-error "talk-handler got %s" httpcon)
  (let ((submit (elnode-http-param httpcon "submit"))
        (conversation-id (elnode-http-mapping httpcon 1))
        (user-id (elnode-http-param httpcon "userid")))
    (if submit
        (let* ((text (elnode-http-param httpcon "text")))
          (talk--distrib-pending conversation-id user-id text))
      (elnode-defer-or-do
        (talk--pending-p conversation-id user-id)
        (let ((json-to-send (talk--pending-get conversation-id user-id)))
          (elnode-send-json httpcon json-to-send))))))


;; This stuff is more specific to a particular UI

(defun talk-init-handler (httpcon)
  "Initiate state for conversations and users.

Makes unique ids for a 'me' and a 'them' user and for their
conversation.

It also cookie decorates the response with the user-id."
  (let* ((me (elnode-http-param httpcon "me"))
         (them (elnode-http-param httpcon "them"))
         (conversation-id (talk--uuid))
         (me-person
          (talk--person
           (talk-s conversation-id)
           :key 'name :value me))
         (them-person
          (talk--person
           (talk-s conversation-id)
           :key 'name :value them))
         (tbl (make-hash-table :test 'equal)))
    ;; Make the response
    (puthash me (oref me-person id) tbl)
    (puthash them (oref them-person id) tbl)
    (puthash "_id" conversation-id tbl)
    (elnode-send-json httpcon tbl)))

(ert-deftest talk-init-handler ()
  "Full stack test for init."
  (with-elnode-mock-server
    'talk-handler
    (let ((r
           (elnode-test-call
            (format "/talk/make/?me=%s&them=%s" "nic" "caroline")
            :parameters
            '(("me" . "nic")
              ("them" . "caroline")))))
      (should
       (equal
        200
        (plist-get r :status)))
      (let ((json-list (json-read-from-string
                        (plist-get r :result-string))))
        (should
         (equal
          (list 'nic 'caroline '_id)
          (loop for i in json-list collect (car i))))))))


(defun talk--conversation-to (httpcon)
  "Read the conversation details from the HTTPCON.

Return the person to whom we are talking."
  ;; What if you don't have a cookie? you must be the other person
  ;; so we need to record who has a cookie?
  (let* ((my-id (elnode-http-cookie
                 httpcon
                 "plaintalk_user"))
         (conversation-id (elnode-http-mapping httpcon 1))
         (other-id (car (talk-other conversation-id my-id))))
    (talk--person (talk-s conversation-id) other-id 'name)))

(defun talk-user-conversation (httpcon)
  "Build the template for landing or the conversation detail.

HTTPCON is the request containing either a conversation specific
url or just the main page.  If it's a conversation specific url
the party is expected to have a cookie."
  (elnode-send-file
   httpcon
   ;; FIXME - needs rooting
   (expand-file-name "~/work/plaintalk/html/template.html")
   :replacements
   (let ((path (elnode-http-pathinfo httpcon)))
     (cond
      ((equal path "/talk/")
       '(("top-right" .
          "<div><ul>
<li><a href='javascript:;'>talk to a friend</a></li>
<li><a href='/about/'>about</a></li>
<li><a href='/blog/'>blog</a></li>
</ul></div>")
         ("start-button" .
          "<a href='javascript:;' class='button nice radius'>
talk to a friend now &gt;
</a>")))
      (t
       `(("top-right" .
          ,(format
            "<div>you are talking to %s</div>"
            (talk--conversation-to httpcon)))
         ("start-button" .
          "<a href='javascript:;' class='hidden'></a>")))))))

(defun talk-handler (httpcon)
  "Frontend talk router."
  ;; FIXME - webserver needs rooting
  (let ((webserver (elnode-webserver-handler-maker "~/work/plaintalk/")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("[^/]+/talk/conversation/\\(.*\\)/$" . talk-user-conversation)
       ("[^/]+/talk/$" . talk-user-conversation)
       ;; These are kinda generic and could form a separate elnode chat service
       ("[^/]+/talk/con/\\(.*\\)/$" . talk-handler)
       ("[^/]+/talk/make/$" . talk-init-handler)
       ;; And the webserver, which is obviously not very generic
       ;;
       ;; certain files, like the JS, are kinda necessary to any chat service.
       ("[^/]+/talk/stuff/\\(.*\\)" . ,webserver))
     :log-name "plaintalk")))

(provide 'talk)

;;; talk.el ends here
