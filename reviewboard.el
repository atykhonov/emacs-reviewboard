(require 'epc)

(defvar rb-epc (epc:start-epc "python" '("rb-server.py")))
(defvar diff-comments "")
(defvar review-id 0)


(define-derived-mode reviewboard-mode tabulated-list-mode "ReviewBoard"
  "Review Board major mode."
  (setq tabulated-list-format [("Id" 4 nil) ("Summary" 60 nil)
                               ("Submiter" 12 nil)
                               ("Posted"  12 nil)
                               ("Status" 8 nil)])

  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "Id" nil))
  (tabulated-list-init-header)
)

(defun reviewboard-outgoing ()
  (interactive)
  (pop-to-buffer "*ReviewBoard*" nil)
  (reviewboard-mode)
  (setq rows (list))
  (setq tabulated-list-entries nil)
  (add-to-list 'tabulated-list-entries rows)
  (setq i 0)
; (message "Return : %S" (epc:call-sync my-epc 'echo '(10 40)))
  (dolist (info (epc:call-sync rb-epc 'outgoing_requests '()))
      (progn
        (setq review-id (int-to-string (nth 0 info)))
        (setq summary (nth 1 info))
        (setq submitter (nth 2 info))
        (setq time-added (nth 3 info))
        (setq status (nth 4 info))
        (setq rows (list i
                         (vector (list review-id 'action 'reviewboard-request)
                                 summary
                                 submitter
                                 time-added
                                 status)))

        (add-to-list 'tabulated-list-entries rows)
        (setq i (+ i 1))
      )
  )

  (tabulated-list-print t)
)

(defun view-details (button)
  (interactive)
  (message "Test message")
)

(define-derived-mode rb-details-mode special-mode "rb-details-mode"
  "ReviewBoard details major mode."
  ; (read-only-mode)
  (define-key rb-details-mode-map (kbd "C-c C-c") 'rb-show-diff)
)

(defun append-comments (curline)
  (setq result " ")
  (let*
    (
     (offset 6)
     (curline (- curline offset))
     (comments (assoc-default curline diff-comments))
    )
    (when comments
      (progn
        (let ((comment (nth 0 comments)))
          (save-excursion
            (goto-line (+ curline offset))
            (let ((bol (point-at-bol)) (eol (point-at-eol)))
              ; (put-text-property bol eol 'point-entered 'popup-comment)
              (put-text-property bol eol 'help-echo comment)
              ; (set-text-properties bol eol '(point-entered popup-comment))
              ; (put-text-property bol eol 'comment-hint comment)
              ; (put-text-property bol eol 'face 'annot-highlighter-face)
            )
          )
        )
        (setq result "C")
      )
    )
  )
  result
)
; apropos point motion


(defun popup-comment ()
  (let*
    (
     (offset 6)
     (curline (- (line-number-at-pos) offset))
     (comments (assoc-default curline diff-comments))
    )
    (when comments
      (message (nth 0 comments))
    )
  )
)

(defun reviewboard-diff (button)
  (interactive)
  (let
    (
     (filename (button-label button))
     (review-id (button-get 'review-id))
     (review-id (get-text-property button 'review-id))
     (buffer-name "*RB-Diff*")
    )
    (pop-to-buffer buffer-name nil)
    (diff-mode)
    (with-current-buffer buffer-name
      (erase-buffer)
      (let
        (
          (data (epc:call-sync rb-epc 'get_diff `(12 ,filename)))
          (comments (epc:call-sync rb-epc 'get_comments `(12 ,filename)))
        )
        (setq diff-comments comments)
        (add-to-list 'post-command-hook 'popup-comment)
        (print diff-comments)
        (setq linum-format 'append-comments)
        (linum-mode)
        (insert data)
      )
      (goto-char (point-min))
    )
  )
)

(defun reviewboard-request (button)
  (interactive)
  (setq review-id (button-label button))
  (let
    (
      (details (epc:call-sync rb-epc 'review `(,review-id)))
      (buffer-name "*RB-Request*")
    )
    (let
      (
        (summary (nth 0 details))
        (branch (nth 1 details))
        (target_people (nth 2 details))
        (target_groups (nth 3 details))
        (repository (nth 4 details))
        (description (nth 5 details))
        (testing_done (nth 6 details))
      )
      (pop-to-buffer buffer-name nil)
      (with-current-buffer buffer-name
        (erase-buffer)
        (insert (format "Summary: %s\n" summary))
        (insert (format "Branch: %s\n" branch))
        (insert (format "People: %s\n" target_people))
        (insert (format "Groups: %s\n" target_groups))
        (insert (format "Repository: %s\n" repository))
        (insert (format "Description: %s\n" description))
        (insert (format "Testing Done: %s\n\n" testing_done))
        (insert "Files:\n")

        (let ((files (epc:call-sync rb-epc 'get_files `(,review-id))))
          (dolist (file files)
            (let
              (
               (id (nth 0 file))
               (name (nth 1 file))
              )
              (insert-text-button name 'action 'reviewboard-diff 'review-id review-id)
              (insert "\n")
            )
          )
        )
      )
    )
  )
)

; TODO replace direct epc calls to deffered calls

; Example:
; (defun test()
;  (deferred:$
;    (epc:call-deferred my-epc 'outgoing_requests)
;    (deferred:nextc it
;      (lambda (x) (message "Return : %s" x))))
;  )

; TODO take care about (read-only-mode)
