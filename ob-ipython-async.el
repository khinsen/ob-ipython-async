(require 'org)
(require 'ob-ipython)
(require 'dash)
(require 'cl)

;;
;; Configuration
;;

;; Set this to nil to deactivate asynchronous execution. The main reason I can imagine
;; is interdependence of code blocks (IPython or other) via header arguments, which is
;; not supported in asynchronous mode.
(defcustom org-babel-async-ipython t
  "If non-nil run ipython asynchronously.")

;; In the unlikely case that you use files or directories named
;; "ipython-inline-images", you can change this definition.
(defcustom org-babel-ipython-inline-image-directory "ipython-inline-images"
  "Name of the directory holding inlined images returned as results from IPython")

;;
;; End of configuration
;;

(defvar *ob-ipython-async-queue* '()
  "Queue of tasks to run.")

(defvar *ob-ipython-async-running-task* nil
  "The currently running task, defined by (buffer body params name).")

;;
;; Management of inline images
;;

(defun ob-ipython-inline-image (b64-string)
  "Write the B64-STRING to a file.
Returns an org-link to the file."
  (let* ((f (md5 b64-string))
	 (d org-babel-ipython-inline-image-directory)
	 (tfile (concat d "/ob-ipython-" f ".png"))
	 (link (format "[[file:%s]]" tfile)))
    (unless (file-directory-p d)
      (make-directory d))
    (ob-ipython--write-base64-string tfile b64-string)
    link))

(defun ob-ipython--delete-inline-image-files ()
  "Remove inline image files referenced in the current result block."
  (let ((location (org-babel-where-is-src-block-result))
        current-results)
    (when location
      (save-excursion
        (goto-char location)
        (when (looking-at (concat org-babel-result-regexp ".*$"))
          (setq current-results (buffer-substring-no-properties
                                 location
                                 (save-excursion
                                   (forward-line 1) (org-babel-result-end)))))))
    (when current-results
      (with-temp-buffer
        (insert current-results)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "\\[\\[file:\\(" org-babel-ipython-inline-image-directory "/ob-ipython-.*?\\)\\]\\]")
                nil t)
          (let ((f (match-string 1)))
            (when (file-exists-p f)
              (delete-file f))))))))

;;
;; Format IPython results/output
;;
;; The result of an IPython code evaluation is a data structure containing
;; multiple results, each of which is described by a MIME type tag.
;; By default, all results are converted to an appropriate representation:
;; plain text is copied as-is, images (PNG only so far) as stored in
;; files and replaced by links to those files, HTML and LaTeX are
;; stored in export blocks.
;; A header argument :ob-ipython-results permits to select only one
;; result type.
;;

(defun ob-ipython--format-result (result result-type)
  "Format a RESULT from an ipython cell.
Return RESULT-TYPE if specified. This comes from a header argument :ob-ipython-results"
  (cl-flet ((format-result (type value)
             (case type
               ('text/plain (concat value "\n"))
               ('text/html (format
                            "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
                            value))
               ('text/latex (format
                             "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
                             values))
               ('image/png (concat (ob-ipython-inline-image value) "\n"))))
            (select-result-type (type result)
               (if type
                   (--filter (eq (car it) (intern type)) result)
                 result)))
    (->> result
         (select-result-type result-type)
         (--map (format-result (car it) (cdr it)))
         (apply #'concat "\n"))))

;;
;; Automatic name generation
;;

;; Asynchronous execution requires each source code block to
;; be named. For blocks that have no name, an automatically
;; generated name is added. This variable holds the function
;; that generates the name.
(defvar org-babel-ipython-name-generator 'ob-ipython-uuid-generator
  "Function to generate a name for a src block.
The default is `ob-ipython-uuid-generator'.")

(defun ob-ipython-uuid-generator ()
  (org-id-new 'none))

(defun org-babel-get-name-create ()
  "Get the name of a src block or add a uuid as the name."
  (if-let (name (fifth (org-babel-get-src-block-info)))
      name
    (save-excursion
      (let ((el (org-element-context))
	    (id (funcall org-babel-ipython-name-generator)))
	(goto-char (org-element-property :begin el))
	(insert (format "#+NAME: %s\n" id))
	id))))

;;
;; Source code block execution
;;

;; org-babel-execute:ipython is the entry point to source code
;; block execution if the language is "ipython". This version replaces
;; the one from ob-ipython and adds the asynchronous execution mode.
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel."
  (let* ((name (org-babel-get-name-create))
         (session (cdr (assoc :session params)))
         (expanded-body (org-babel-expand-body:generic
                         (encode-coding-string body 'utf-8)
                         params
                         (org-babel-variable-assignments:python params))))
    ;; Start a session if necessary
    (org-babel-ipython-initiate-session session params)
    ;; Clean up obsolete results
    (ob-ipython--delete-inline-image-files)
    (if org-babel-async-ipython
        ;; Queue the code for execution and return a link informing
        ;; about the task's status (queued/running).
        (ob-ipython--queue-src-block body params name)
      ;; Execute code and return results
      (ob-ipython--execute-src-block body params))))

;; Synchronous execution works much like the original code in
;; ob-ipython. However, IPython results are handled in a more
;; sophisticated way, in particular for images.  This is complete
;; unrelated to asynchronous execution, it just happens to be located
;; in the same part of the code.
(defun ob-ipython--execute-src-block (body params)
  "Synchronous execution of a source code block."
  ;; Execute the source block...
  (-when-let (ret (ob-ipython--eval
                   (ob-ipython--execute-request body
                                                (ob-ipython--normalize-session
                                                 (cdr (assoc :session params))))))
    ;; ... grab the result and text output
    (let* ((result (cdr (assoc :result ret)))
           (output (cdr (assoc :output ret)))
           (result-type (cdr (assoc :result-type params)))
           (formatted-result (ob-ipython--format-result
                              result
                              (cdr (assoc :ob-ipython-results params)))))
      ;; If 'output is requested, behave like Jupyter: append the formatted
      ;; result to the output produced on stdout.
      (if (eq result-type 'output)
          (concat output formatted-result)
        ;; If 'result is requested, keep 'output in a different buffer...
        (ob-ipython--create-stdout-buffer output)
        ;; ... and put the formatted result into the result block.
        formatted-result))))

;; Asynchronous execution starts with adding a source code block to
;; the task queue and then calling ob-ipython--process-queue.  The
;; latter starts the first task in the queue, specifying
;; ob-ipython--async-callback as the callback that is executed at the
;; end of that task. The callback inserts the results of the execution
;; at the right place in the buffer, and then resumes queue processing.
;; The task queue is thus processed by alternating calls to
;; ob-ipython--process-queue and ob-ipython--async-callback.

(defun ob-ipython--queue-src-block (body params name)
  "Add a source code block to the execution queue. Start execution if no other block
   is being execution. Return a link indicating the state (queued/running)."
  (let* ((result-type (cdr (assoc :result-type params))))
    (add-to-list '*ob-ipython-async-queue*
                 (list (current-buffer) body params name)
                 t)
    (if (ob-ipython--process-queue nil)
        (format "[[async-running: %s]]" name)
      (format "[[async-queued: %s]]" name))))

(defun ob-ipython--process-queue (update-link)
  "If no task is running, start the first one on the waiting list and
return t. Otherwise, do nothing and return nil. If update-link is non-nil, 
change the task's async link from 'queued' to 'running'."
  (-when-let* ((not-running (not *ob-ipython-async-running-task*))
               (task (pop *ob-ipython-async-queue*))
               ((buffer body params name) task)
               (session (ob-ipython--normalize-session (cdr (assoc :session params)))))
    (when (and update-link
               (not (member "silent" (cdr (assoc :result-params params)))))
      (with-current-buffer buffer
        (save-excursion
          (org-babel-goto-named-src-block name)
          (search-forward (format "[[async-queued: %s]]" name))
          (replace-match (format "[[async-running: %s]]" name)))))
    (setq *ob-ipython-async-running-task* task)
    (message "Running IPython task %s" name)
    (let ((url-request-data body)
          (url-request-method "POST"))
      (url-retrieve
       (format "http://%s:%d/execute/%s"
               ob-ipython-driver-hostname
               ob-ipython-driver-port
               session)
       'ob-ipython--async-callback))
    t))

(defun ob-ipython--async-callback (status &rest args)
  "Callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results."
  (-let* ((current-task *ob-ipython-async-running-task*)
          ((buffer body params name) current-task)
          (ret (ob-ipython--eval (ob-ipython--parse-retrieved-data buffer name)))
          (result (cdr (assoc :result ret)))
          (output (cdr (assoc :output ret)))
          (result-type (cdr (assoc :result-type params)))
          (formatted-result (ob-ipython--format-result
                              result
                              (cdr (assoc :ob-ipython-results params)))))
    ;; Insert the result in the right place
    (unless (member "silent" (cdr (assoc :result-params params)))
      (with-current-buffer buffer
        (save-excursion
          (org-babel-goto-named-result name)
          (search-forward (format "[[async-running: %s]]" name))
          (replace-match
           (if (eq result-type 'output)
               (concat output formatted-result)
             formatted-result))
          (org-redisplay-inline-images)
          (when (member "drawer" (cdr (assoc :result-params params)))
            ;; open the results drawer
            (org-babel-goto-named-result name)
            (forward-line)
            (org-flag-drawer nil)))))
    (setq *ob-ipython-async-running-task* nil)
    ;; Process the rest of the task queue.
    (ob-ipython--process-queue t)))

(defun ob-ipython--parse-retrieved-data (buffer name)
  "Parse the JSON data structure returned by IPython."
  (when (>= (url-http-parse-response) 400)
    (ob-ipython--dump-error (buffer-string)))
  (goto-char url-http-end-of-headers)
  (let* ((json-array-type 'list)
         (json (json-read)))
    (when (string= "error" (cdr (assoc 'msg_type (elt json 0))))
      (with-current-buffer buffer
        (org-babel-goto-named-src-block name)
        (org-babel-remove-result))
      (ob-ipython-clear-async-queue)) 
    json))

(defun ob-ipython-clear-async-queue ()
  "Clear the queue and all pending results."
  (interactive)
  (loop for (buffer body params name) in *ob-ipython-async-queue*
	do
	(save-window-excursion
	  (with-current-buffer buffer
	    (org-babel-goto-named-src-block name)
	    (org-babel-remove-result))))
  (setq *ob-ipython-async-running-task* nil
	*ob-ipython-async-queue* '()))

;;
;; REPL setup
;;
;; The following function replaces the version from ob-ipython.
;; It adds the session name to the REPL buffer name, making it
;; possible to have multiple named sessions, each with its own REPL.
(defun ob-ipython--create-repl (name)
  ;; TODO: hack while we wait on
  ;; https://github.com/jupyter/jupyter_console/issues/93
  (let ((proc-name (format "IPython-%s" name))
        (prev (getenv "JUPYTER_CONSOLE_TEST"))
        buffer-name)
    (setenv "JUPYTER_CONSOLE_TEST" "1")
    (setq buffer-name
          (python-shell-make-comint
           (s-join " " (ob-ipython--kernel-repl-cmd name))
           proc-name nil))
    (setenv "JUPYTER_CONSOLE_TEST" prev)
    buffer-name))

;;
;; Reinitialize everything
;;
(defun ob-ipython-kill-all ()
  "Kill all processes and buffers used for IPython evaluation,
including the REPL buffers."
  (interactive)
  (let ((ipython-buffers
           (--filter (or (string-prefix-p "*ob-ipython-" (buffer-name it))
                         (string-prefix-p "*IPython-" (buffer-name it)))
                     (buffer-list)))
        (prev kill-buffer-query-functions))
    (setq kill-buffer-query-functions
          (remq 'process-kill-buffer-query-function
                kill-buffer-query-functions))
    (--map (progn
             (message "Killing %s" (buffer-name it))
             (kill-buffer it))
           ipython-buffers)
    (setq kill-buffer-query-functions prev)
    (ob-ipython-clear-async-queue)))


(provide 'ob-ipython-async)
