(defpackage :elizabot
  (:use :cl)
  (:import-from :dexador)
  (:import-from :jonathan)
  (:import-from :elizabot/eliza
   :*translation-rules*
   :add-translation)
  (:export
   :*token*
   :run
   
   ;; Re-exports.
   :*translation-rules*
   :add-translation))
(in-package :elizabot)

(defconstant +telegram-base-url+ "https://api.telegram.org/bot")

(defparameter *token* nil
  "The bot's token.")

(defun get-me ()
  "Call the get-me Telegram method."
  (let* ((url (concatenate 'string +telegram-base-url+ *token* "/getMe"))
	 (response (dex:get url))
	 (data (jonathan:parse response)))
    (getf data :|result|)))

(defun greet ()
  "Obtain information about the bot via the get-me Telegram method and print a
   greeting."
  (let ((me (get-me)))
    (format t "Succesfully connected to bot ~A [username: @~A]~%"
	    (getf me :|first_name|)
	    (getf me :|username|))))

(defun get-updates (&optional (offset nil))
  "Get and returns the updates."
  (let* ((url (concatenate 'string +telegram-base-url+ *token* "/getUpdates"))
	 (content (jonathan:to-json (when offset (list :|offset| offset))))
	 (response (dex:post url
			     :headers '(("Content-Type" . "application/json"))
			     :content content))
	 (data (jonathan:parse response)))
    (getf data :|result|)))

(defun get-new-offset (updates)
  "Extracts the biggest update_id."
  (apply #'max
	 (mapcar (lambda (u) (getf u :|update_id|)) updates)))

(defun reply-to (message text)
  "Reply to `message' with `text'."
  (let* ((url (concatenate 'string +telegram-base-url+ *token* "/sendMessage"))
	 (content (list :|chat_id| (getf (getf message :|chat|) :|id|)
			:|text| text
			:|reply_to_message_id| (getf message :|message_id|))))

    (dex:post url
	      :headers '(("Content-Type" . "application/json"))
	      :content (jonathan:to-json content))))

(defun eliza-reply (message)
  "Reply (or not reply) to message."
  (multiple-value-bind (text err) (elizabot/eliza:eliza (getf message :|text|))
    (unless err
      (reply-to message text))))

(defun eliza-bot (&optional (offset nil))
  "The core of Elizabot. Read messages and run them through ELIZA, then return
   the output."
  (let* ((updates (get-updates offset)))
    (mapcar #'(lambda (u)
		(let ((m (getf u :|message|)))
		  (when m (eliza-reply m))))
	    updates)
    (if updates
	(eliza-bot (1+ (get-new-offset updates)))
	(eliza-bot))))

(defun run ()
  "Start Elizabot."
  (handler-case (greet)
    (error ()
      (error "Coudn't connect with the given bot token.")))
  (eliza-bot))
