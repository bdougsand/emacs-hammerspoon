;; hammerspoon.el
;;
;;; Commentary:

;; hammerspoon.el establishes a connection to a remote port opened by
;; [Hammersoon](http://www.hammerspoon.org), an automation tool for macOS,
;; allowing, for example, event information to be sent.

(require 'json)
(require 'dash)
(require 'org-pomodoro)

(defcustom hammerspoon-cl-exec "hs"
  "The name of the Hammerspoon command line utility"
  :group 'hammerspoon)

(defcustom hammerspoon-port-name "emacsInterop"
  "The name of the remote port created by Hammerspoon."
  :group 'hammerspoon)

(defcustom hammerspoon-receive-hook nil
  "Hook run when emacs receives an object from Hammerspoon."
  :group 'hammerspoon
  :type 'hook)

(defcustom hammerspoon-connect-on-start t
  "Start hammerspoon on load?"
  :group 'hammerspoon)

(defvar hammerspoon--subprocess nil)

(defun hammerspoon--receive-json (object)
  ;; TODO More interesting handling of input
  (run-hook-with-args 'hammerspoon-receive-hook object))

(defvar hammerspoon--parse-marker (make-marker))

(defun hammerspoon--subprocess-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))

      ;; Now try to parse more JSON:
      (goto-char hammerspoon--parse-marker)
      (search-forward ">>" nil 't)
      ;; Keep reading in JSON until we encounter a parse error
      (condition-case err
          (progn
            (hammerspoon--receive-json (json-read))
            (set-marker hammerspoon--parse-marker (point-max)))

        (json-error nil)))))

(defun hammerspoon--sentinel (proc status)
  (message (concat "Hammerspoon: " status))
  (kill-buffer (process-buffer proc))
  (delete-process proc))

(defun hammerspoon-connect ()
  (let ((proc (start-process "hammerspoon" "*hammerspoon*"
                             hammerspoon-cl-exec
                             "-n" "-q"
                             "-m" hammerspoon-port-name)))
    (setq hammerspoon--subprocess proc)
    (set-marker hammerspoon--parse-marker 0 (process-buffer proc))
    (set-process-filter proc 'hammerspoon--subprocess-filter)
    (set-process-sentinel proc 'hammerspoon--sentinel)
    hammerspoon--subprocess))


(defun hammerspoon--cleanup-process ()
  (when hammerspoon--subprocess
    (process-send-eof hammerspoon--subprocess)
    (kill-buffer (process-buffer hammerspoon--subprocess))))

(defun hammerspoon--get-or-connect ()
  (or (if (eq (process-status hammerspoon--subprocess) 'run)
          hammerspoon--subprocess
        (hammerspoon--cleanup-process))
      (hammerspoon--connect)))

(defun hammerspoon--send (message)
  (process-send-string hammerspoon--subprocess (concat message "\n")))

(defun hammerspoon--send-json (object)
  (hammerspoon--send (json-encode object)))

(defun hammerspoon-quit ()
  (hammerspoon--cleanup-process))

(defun hammerspoon--event-name-from-hook (hook-symbol)
  (-> hook-symbol
      (symbol-name)
      (substring 4 -5)
      (s-lower-camel-case)))

(defun hammerspoon--get-clock-path ()
  (when-let ((buff (marker-buffer org-clock-hd-marker)))
    (with-current-buffer buff
      (save-excursion
        (goto-char org-clock-hd-marker)
        (org-get-outline-path 't)))))

(defun hammerspoon--get-buffer-name ()
  (-some-> (marker-buffer org-clock-hd-marker)
           (buffer-name)
           (file-name-sans-extension)))

(defun hammerspoon--get-current-subtree-text ()
  "Returns the full text of the subtree associated with the current clock."
  (with-current-buffer (marker-buffer org-clock-hd-marker)
    (save-excursion
      (goto-char org-clock-hd-marker)
      ; (org-get-entry)
      (save-match-data
        (org-with-limited-levels
         (buffer-substring-no-properties
          (progn (org-back-to-heading t)
                 (org-end-of-meta-data t)
                 (point))
          (progn (org-end-of-subtree t t)
                 (point))))))))

(defun hammerspoon--total-time-today-files (files)
    (-reduce-from (lambda (total agenda-file)
                    (let* ((buff (or (get-file-buffer agenda-file)
                                     (create-file-buffer agenda-file))))
                      (+ total (with-current-buffer buff
                                 (org-clock-sum-today)))))
                  0
                  files))

(defun hammerspoon--total-time-today ()
  (hammerspoon--total-time-today-files org-agenda-files))

(defun hammerspoon--make-pomodoro-event (hook-symbol)
  (let ((event (make-hash-table)))
    (puthash :type (hammerspoon--event-name-from-hook hook-symbol) event)
    (puthash :timeRemaining (org-pomodoro-remaining-seconds) event)
    (puthash :endTime (float-time org-pomodoro-end-time) event)
    (puthash :count org-pomodoro-count event)
    (puthash :bufferName (hammerspoon--get-buffer-name) event)
    (puthash :taskPath (hammerspoon--get-clock-path) event)
    event))

(defun hammerspoon--get-pomodoro-state ()
  (hammerspoon--make-pomodoro-event
   (case org-pomodoro-state
     (:pomodoro 'org-pomodoro-started-hook)
     ((:break :longbreak) 'org-pomodoro-finished-hook)
     (t 'org-pomodoro-killed-hook))))

(defun hammerspoon--attach-pomodoro-hook (hook-symbol)
  (lexical-let ((hook-symbol hook-symbol))
    (add-hook hook-symbol (lambda () (hammerspoon--send-json
                                      (hammerspoon--make-pomodoro-event hook-symbol))))))

(add-hook 'hammerspoon-receive-hook
          (lambda (message-object)
            (print (cond
                    ((arrayp message-object) "Got an array response from Hammerspoon")
                    ((listp message-object) (concat "Got an object with response: "
                                                    (alist-get 'response message-object "-nil-")))))))

(with-eval-after-load 'org-pomodoro
  (--each '(org-pomodoro-started-hook
            org-pomodoro-finished-hook
            org-pomodoro-killed-hook
            org-pomodoro-break-finished-hook)
    (hammerspoon--attach-pomodoro-hook it))

  (add-hook 'org-clock-in-hook (lambda ()
                                 (when (eq org-pomodoro-state :pomodoro)
                                   (hammerspoon--send-json
                                    (hammerspoon--make-pomodoro-event 'org-clock-in-hook))))))


(when hammerspoon-connect-on-start
  (hammerspoon-connect))

(provide 'hammerspoon)



