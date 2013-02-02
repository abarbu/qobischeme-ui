(module qobischeme-ui *
(import chicken scheme foreign)
(use  srfi-1 posix lolevel extras traversal
     nondeterminism define-structure linear-algebra image-processing
     scheme2c-compatibility)
(use posix lolevel foreigners xlib)

(define stalin? #f)

(define (char-alphanumeric? char)
 (or (char-alphabetic? char) (char-numeric? char)))

(define (beginning-of-word? string position)
 (or (zero? position)
     (and (not (= position (string-length string)))
	  (not (char-alphanumeric? (string-ref string (- position 1))))
	  (char-alphanumeric? (string-ref string position)))))

(define (end-of-word? string position)
 (or (= position (string-length string))
     (and (not (zero? position))
	  (char-alphanumeric? (string-ref string (- position 1)))
	  (not (char-alphanumeric? (string-ref string position))))))

(define (string-backward-word string position)
 (when (zero? position) (abort-gui))
 (let loop ((position (- position 1)))
  (if (beginning-of-word? string position)
      (list string position)
      (loop (- position 1)))))

(define (string-kill-word string position)
 (when (= position (string-length string)) (abort-gui))
 (list (string-append (substring string 0 position)
		      (substring string
				 (second (string-forward-word string position))
				 (string-length string)))
       position))

(define (string-forward-word string position)
 (when (= position (string-length string)) (abort-gui))
 (let loop ((position (+ position 1)))
  (if (end-of-word? string position)
      (list string position)
      (loop (+ position 1)))))

(define (string-backward-kill-word string position)
 (when (zero? position) (abort-gui))
 (let ((new-position (second (string-backward-word string position))))
  (list (string-append (substring string 0 new-position)
		       (substring string position (string-length string)))
	new-position)))

(define (string-insert-character character)
 (lambda (string position)
  (list (string-append (substring string 0 position)
		       (list->string (list character))
		       (substring string position (string-length string)))
	(+ position 1))))

(define (string-beginning-of-line string position)
 (list string 0))

(define (string-backward-char string position)
 (when (zero? position) (abort-gui))
 (list string (- position 1)))

(define (string-delete-char string position)
 (when (= position (string-length string)) (abort-gui))
 (list (string-append
	(substring string 0 position)
	(substring string (+ position 1) (string-length string)))
       position))

(define (string-end-of-line string position)
 (list string (string-length string)))

(define (string-forward-char string position)
 (when (= position (string-length string)) (abort-gui))
 (list string (+ position 1)))

(define (string-kill-line string position)
 (list (substring string 0 position) position))

(define (string-backward-delete-char string position)
 (when (zero? position) (abort-gui))
 (list (string-append (substring string 0 (- position 1))
		      (substring string position (string-length string)))
       (- position 1)))

;;; Sclim

(define return-key (integer->char 13))
(define escape-key (integer->char 27))
(define delete-key (integer->char 127))
(define *display* '(0 0))
(define *screen* #f)
(define *root-window* 0)
(define *button-width* 0)
(define *button-height* 0)
(define *background-color* "White")
(define *foreground-color* "Black")
(define *background* '(0 0))
(define *foreground* '(0 0))
(define *white-pixel* 0)
(define *black-pixel* 0)
(define *roman-font* 0)
(define *bold-font* 0)
(define *roman-height* 0)
(define *bold-height* 0)
(define *text-height* 0)
(define *roman-baseline* 0)
(define *bold-baseline* 0)
(define *text-baseline* 0)
(define *display-pane-width* 0)
(define *display-pane-height* 0)
(define *transcript-pane-height* 0)
(define *echo-pane-height* 0)
(define *who-line-height* 0)
(define *status-pane-width* 0)
(define *window* 0)
(define *buttons* 0)
(define *regions* '())
(define *display-pane* 0)
(define *transcript-pane* 0)
(define *echo-pane* 0)
(define *status-pane* 0)
(define *message-pane* 0)
(define *thin-gc* 0)
(define *thin-flipping-gc* 0)
(define *medium-gc* 0)
(define *medium-flipping-gc* 0)
(define *thick-gc* 0)
(define *thick-flipping-gc* 0)
(define *dashed-gc* 0)
(define *dashed-flipping-gc* 0)
(define *roman-gc* 0)
(define *bold-gc* 0)
(define *bold-flipping-gc* 0)
(define *light-gray* 0)
(define *light-gray-gc* 0)
(define *gray* 0)
(define *gray-gc* 0)
(define *red* 0)
(define *red-gc* 0)
(define *dark-red* 0)
(define *dark-red-gc* 0)
(define *green* 0)
(define *green-gc* 0)
(define *dark-green* 0)
(define *dark-green-gc* 0)
(define *blue* 0)
(define *blue-gc* 0)
(define *yellow* 0)
(define *yellow-gc* 0)
(define *violet* 0)
(define *violet-gc* 0)
(define *orange* 0)
(define *orange-gc* 0)
(define *dark-orange* 0)
(define *dark-orange-gc* 0)
(define *color-gc* 0)
(define *window-methods* 0)
(define *transcript* 0)
(define *input* 0)
(define *input-position* 0)
(define *abort-button* 0)
(define *abort-key* 0)
(define *comtab* '#())
(define *help-comtab* 0)
(define *prefix* 0)
(define *status* 0)
(define *message* 0)
(define *pause?* 0)
(define *redraw-procedure* (lambda () #f))
(define *quit-continuation* 0)
(define *abort-continuation* 0)
(define *color-cube* 0)
(define *reds* 4)
(define *greens* 8)
(define *blues* 4)
(define *dither?* #t)
(define *help?* 0)
(define *help* 0)
(define *first-help-line* 0)
(define *clear-display-pane?* 0)
(define *display-name* "")
(define *roman-font-name* "9x15")
(define *bold-font-name* "9x15bold")
(define *window-position?* #f)
(define *window-position-x* 0)
(define *window-position-y* 0)
(define *post-initialize-procedure* #f)
(define *enable-background-task* (lambda () #f))
(define *disable-background-task* (lambda () #f))

(define (say string)
 (set! *transcript* (cons (list 'system string) *transcript*))
 (redraw-transcript-pane))

(define (status string)
 (set! *status* string)
 (redraw-status-pane))

(define message
 (lambda (string)
  (set! *message* string)
  (redraw-message-pane)))

(define (set-pause! p)
 (set! *pause?* p)
 (redraw-buttons))

(define (character->pretty-name character)
 (let ((i (char->integer character)))
  (cond ((= i 0) "C-@")			;also C-SPC
	((= i 9) "TAB")			;also C-i
	((= i 10) "LFD")		;also C-j
	((= i 13) "RET")		;also C-m
	((= i 27) "ESC")		;also C-[
	((= i 28) "C-\\")
	((= i 29) "C-]")
	((= i 30) "C-^")
	((= i 31) "C-_")		;also C-/
	((= i 32) "SPC")
	((= i 127) "DEL")
	((= i 128) "M-C-@")		;also M-C-SPC
	((= i 137) "M-TAB")		;also M-C-i
	((= i 138) "M-LFD")		;also M-C-j
	((= i 141) "M-RET")		;also M-C-m
	((= i 155) "M-ESC")		;also M-C-[
	((= i 156) "M-C-\\")
	((= i 157) "M-C-]")
	((= i 158) "M-C-^")
	((= i 159) "M-C-_")		;also M-C-/
	((= i 160) "M-SPC")
	((= i 255) "M-DEL")
	(else (if (>= i 128)
		  (let ((i (- i 128)))
		   (if (< i 32)
		       (format #f "M-C-~a" (integer->char (+ i 96)))
		       (format #f "M-~a" (integer->char i))))
		  (if (< i 32)
		      (format #f "C-~a" (integer->char (+ i 96)))
		      (string character)))))))

(define (prefix-string prefix)
 (if (null? (rest prefix))
     (character->pretty-name (first prefix))
     (format #f "~a ~a"
	     (prefix-string (rest prefix))
	     (character->pretty-name (first prefix)))))

(define (set-window-method! window event-type method)
 (set! *window-methods*
       (cons (cons (list window event-type) method) *window-methods*)))

(define (send window event-type . &rest)
 (let ((x (assoc (list window event-type) *window-methods*)))
  (when x (apply (cdr x) &rest))))

(define (redraw-buttons)
 (for-each (lambda (button) (send button 'expose)) *buttons*)
 (xflush *display*))

(define (redraw-display-pane)
 (when *display-pane*
  (when *clear-display-pane?* (xclearwindow *display* *display-pane*))
  (set! *regions* '())
  (if *help?*
      (let* ((character-strings
	      (map (lambda (help-entry)
		    (if (list? (first help-entry))
			(qmap-reduce (lambda (s t) (string-append s " " t))
				    ""
				    character->pretty-name (first help-entry))
			(character->pretty-name (first help-entry))))
		   (reverse *help*)))
	     (n (+ (qmap-reduce max 0 string-length character-strings) 1)))
       (let loop ((character-strings character-strings)
		  (documentation-strings (map second (reverse *help*)))
		  (y (+ *text-height* 2))
		  (skip *first-help-line*))
	(unless (null? character-strings)
	 (if (zero? skip)
	     (let ((line
		    (format #f "~a~a~a"
			    (first character-strings)
			    (make-string
			     (- n (string-length (first character-strings)))
			     #\space)
			    (first documentation-strings))))
	      (xdrawstring *display* *display-pane* *roman-gc*
			   5 (- y (+ *roman-baseline* 2))
			   line (string-length line))
	      (loop (rest character-strings)
		    (rest documentation-strings)
		    (+ y *text-height*)
		    skip))
	     (loop (rest character-strings)
		   (rest documentation-strings)
		   y
		   (- skip 1))))))
      (*redraw-procedure*))
  (xflush *display*)))

(define (redraw-transcript-pane)
 (when *transcript-pane*
  (xclearwindow *display* *transcript-pane*)
  (let loop ((transcript *transcript*) (y (- *transcript-pane-height* 2)))
   (unless (null? transcript)
    (let* ((line (first transcript))
	   (text-height
	    (if (eq? (first line) 'user) *roman-height* *bold-height*)))
     (when (>= y (- text-height 1))
      (xdrawstring
       *display* *transcript-pane*
       (if (eq? (first line) 'user) *roman-gc* *bold-gc*)
       5
       (- y (if (eq? (first line) 'user) *roman-baseline* *bold-baseline*))
       (second line) (string-length (second line)))
      (loop (rest transcript) (- y text-height))))))
  (xflush *display*)))

(define (redraw-echo-pane)
 (when *echo-pane*
  (xclearwindow *display* *echo-pane*)
  (let* ((n (quotient (- *display-pane-width* 10)
		      (xtextwidth *roman-font* "m" (string-length "m"))))
	 (m (max 0 (- *input-position* n)))
	 (input (substring *input* m (string-length *input*)))
	 (input (substring input 0 (min (string-length input) n))))
   (xdrawstring *display* *echo-pane* *roman-gc*
		5 (- *echo-pane-height* (+ *roman-baseline* 2))
		input (string-length input))
   (xdrawline *display* *echo-pane* *thin-gc*
	      (+ 5 (xtextwidth *roman-font* *input* (- *input-position* m)))
	      2
	      (+ 5 (xtextwidth *roman-font* *input* (- *input-position* m)))
	      (- *echo-pane-height* 3)))
  (xflush *display*)))

(define (redraw-status-pane)
 (when *status-pane*
  (xclearwindow *display* *status-pane*)
  (xdrawstring
   *display* *status-pane* *roman-gc*
   (quotient (- *status-pane-width*
		(xtextwidth *roman-font* *status* (string-length *status*)))
	     2)
   (- *who-line-height* (+ *roman-baseline* 2))
   *status* (string-length *status*))
  (xflush *display*)))

(define (redraw-message-pane)
 (when *message-pane*
  (xclearwindow *display* *message-pane*)
  (if (null? *prefix*)
      (xdrawstring *display* *message-pane* *roman-gc*
		   5
		   (- *who-line-height* (+ *roman-baseline* 2))
		   *message* (string-length *message*))
      (let ((string (prefix-string *prefix*)))
       (xdrawstring *display* *message-pane* *roman-gc*
		    5
		    (- *who-line-height* (+ *roman-baseline* 2))
		    string (string-length string))))
  (xflush *display*)))

(define-structure region button state-mask state x y width height method)

(define (define-region x y width height method)
 (set! *regions*
       (cons (make-region #f 0 0 x y width height method) *regions*)))

(define (define-button-specific-region
	 button state-mask state x y width height method)
 (set! *regions*
       (cons (make-region button state-mask state x y width height method)
	     *regions*)))

(define (region-handler x y button state)
 (let ((region
	(find-if (lambda (region)
		  (and (<= (region-x region) x)
		       (<  x (+ (region-x region) (region-width region)))
		       (<= (region-y region) y)
		       (<  y (+ (region-y region) (region-height region)))
		       (or (not (region-button region))
			   (= button (region-button region)))
		       (= (bit-and state (region-state-mask region))
			  (region-state region))))
		 *regions*)))
  (when region
   (let ((old-status *status*))
    (xselectinput *display*
		  *window*
		  (bit-or EXPOSUREMASK
			  BUTTONPRESSMASK
			  BUTTONRELEASEMASK
			  KEYPRESSMASK))
    (xselectinput *display*
		  *display-pane*
		  (bit-or EXPOSUREMASK
			  BUTTONPRESSMASK
			  BUTTONRELEASEMASK
			  KEYPRESSMASK))
    (status "Run")
    (call-with-current-continuation
     (lambda (abort-continuation)
      (set! *abort-continuation* abort-continuation)
      ((region-method region) x y)
      #f))
    (xselectinput *display*
		  *window*
		  (bit-or EXPOSUREMASK
			  POINTERMOTIONMASK
			  BUTTONPRESSMASK
			  BUTTONRELEASEMASK
			  KEYPRESSMASK))
    (xselectinput *display*
		  *display-pane*
		  (bit-or EXPOSUREMASK
			  POINTERMOTIONMASK
			  BUTTONPRESSMASK
			  BUTTONRELEASEMASK
			  KEYPRESSMASK))
    (status old-status)))))

(define (set-background-task-enabler! procedure)
 (set! *enable-background-task* procedure))

(define (set-background-task-disabler! procedure)
 (set! *disable-background-task* procedure))

(define (abort?)
 (let loop ((events '()))
  (cond
   ((> (xpending *display*) 0)
    (let ((event (make-xevent)))
     (*enable-background-task*)
     (xnextevent *display* event)
     (*disable-background-task*)
     (cond
      ((or
	(and *abort-button*
	     (= (xevent-xany-type event) BUTTONPRESS)
	     (eq? (xevent-xany-window event) *abort-button*))
	(and
	 *abort-key*
	 (= (xevent-xany-type event) KEYPRESS)
	 (= (string-length (ylookupstring event)) 1)
	 (if (>= (char->integer *abort-key*) 128)
	     (and (char=?
		   (string-ref (ylookupstring event) 0)
		   (integer->char (- (char->integer *abort-key*) 128)))
		  (not (zero? (bit-and (xevent-xkey-state event) MOD1MASK))))
	     (and (char=? (string-ref (ylookupstring event) 0) *abort-key*)
		  (zero? (bit-and (xevent-xkey-state event) MOD1MASK))))))
       (for-each (lambda (event) (xputbackevent *display* event)) events)
       #t)
      (else (loop (cons event events))))))
   (else (for-each (lambda (event) (xputbackevent *display* event)) events)
	 #f))))

(define (process-events)
 (call-with-current-continuation
  (lambda (quit-continuation)
   (set! *quit-continuation* quit-continuation)
   (let ((event (make-xevent))
	 (comtab (if *help?* *help-comtab* *comtab*))
	 (n 0))
    (let loop ()
     (*enable-background-task*)
     (xnextevent *display* event)
     (*disable-background-task*)
     (let ((event-type (xevent-xany-type event))
	   (window (xevent-xany-window event)))
      (define (execute-key character)
       (let ((command (vector-ref comtab (char->integer character))))
	(cond ((vector? command)
	       (set! *prefix* (cons character *prefix*))
	       (redraw-message-pane)
	       (set! comtab command))
	      ((procedure? command)
	       (set! *prefix* '())
	       (redraw-message-pane)
	       (set! comtab (if *help?* *help-comtab* *comtab*))
	       (let ((old-status *status*))
		(xselectinput *display*
			      *window*
			      (bit-or EXPOSUREMASK
				      BUTTONPRESSMASK
				      BUTTONRELEASEMASK
				      KEYPRESSMASK))
		(xselectinput *display*
			      *display-pane*
			      (bit-or EXPOSUREMASK
				      BUTTONPRESSMASK
				      BUTTONRELEASEMASK
				      KEYPRESSMASK))
		(status "Run")
		(message "")
		(call-with-current-continuation
		 (lambda (abort-continuation)
		  (set! *abort-continuation* abort-continuation)
		  (command)
		  #f))
		(xselectinput *display*
			      *window*
			      (bit-or EXPOSUREMASK
				      POINTERMOTIONMASK
				      BUTTONPRESSMASK
				      BUTTONRELEASEMASK
				      KEYPRESSMASK))
		(xselectinput *display*
			      *display-pane*
			      (bit-or EXPOSUREMASK
				      POINTERMOTIONMASK
				      BUTTONPRESSMASK
				      BUTTONRELEASEMASK
				      KEYPRESSMASK))
		(status old-status))
	       (set! comtab (if *help?* *help-comtab* *comtab*)))
	      (else (set! *prefix* '())
		    (redraw-message-pane)
		    (set! comtab *comtab*)
		    (call-with-current-continuation
		     (lambda (abort-continuation)
		      (set! *abort-continuation* abort-continuation)
		      (abort-command)
		      #f))))))
      (cond ((= event-type MAPPINGNOTIFY) (xrefreshkeyboardmapping event))
	    ((= event-type EXPOSE)
	     (set! n (+ n 1))
	     (set! *prefix* '())
	     (redraw-message-pane)
	     (set! comtab (if *help?* *help-comtab* *comtab*))
	     (send window 'expose)
	     (when (and (= n (if (eq? *display-pane* *window*)
				 1
				 (+ (length *buttons*)
				    4
				    (if *transcript-pane* 1 0)
				    (if *echo-pane* 1 0))))
			*post-initialize-procedure*)
	      (*post-initialize-procedure*)
	      (set! *post-initialize-procedure* #f)))
	    ;; I don't know why these happen or what they mean.
	    ((= event-type NOEXPOSE) #f)
	    ((= event-type MOTIONNOTIFY) #f)
	    ((= event-type BUTTONPRESS)
	     (when *help?*
	      (set! *help?* #f)
	      (redraw-buttons)
	      (redraw-display-pane))
	     (set! *prefix* '())
	     (redraw-message-pane)
	     (set! comtab (if *help?* *help-comtab* *comtab*))
	     (let ((old-status *status*))
	      (xselectinput *display*
			    *window*
			    (bit-or EXPOSUREMASK
				    BUTTONPRESSMASK
				    BUTTONRELEASEMASK
				    KEYPRESSMASK))
	      (xselectinput *display*
			    *display-pane*
			    (bit-or EXPOSUREMASK
				    BUTTONPRESSMASK
				    BUTTONRELEASEMASK
				    KEYPRESSMASK))
	      (status "Run")
	      (call-with-current-continuation
	       (lambda (abort-continuation)
		(set! *abort-continuation* abort-continuation)
		(send window 'BUTTONPRESS
		      (xevent-xbutton-x event)
		      (xevent-xbutton-y event)
		      (xevent-xbutton-button event)
		      (xevent-xbutton-state event))
		#f))
	      (xselectinput *display*
			    *window*
			    (bit-or EXPOSUREMASK
				    POINTERMOTIONMASK
				    BUTTONPRESSMASK
				    BUTTONRELEASEMASK
				    KEYPRESSMASK))
	      (xselectinput *display*
			    *display-pane*
			    (bit-or EXPOSUREMASK
				    POINTERMOTIONMASK
				    BUTTONPRESSMASK
				    BUTTONRELEASEMASK
				    KEYPRESSMASK))
	      (status old-status))
	     (set! comtab (if *help?* *help-comtab* *comtab*)))
	    ((= event-type BUTTONRELEASE) #f)
	    ((= event-type KEYPRESS)
	     (cond
	      ((= (xlookupkeysym event 0) XK_BACKSPACE)
	       (if (zero? (bit-and (xevent-xkey-state event) MOD1MASK))
		   (execute-key delete-key)
		   (execute-key (meta delete-key))))
	      ((= (string-length (ylookupstring event)) 1)
	       (let ((character (string-ref (ylookupstring event) 0)))
		(if (zero? (bit-and (xevent-xkey-state event) MOD1MASK))
		    (execute-key character)
		    (execute-key (meta character)))))
	      ((= (xlookupkeysym event 0) XK_HOME) (execute-key (meta #\<)))
	      ((= (xlookupkeysym event 0) XK_LEFT) (execute-key (control #\b)))
	      ((= (xlookupkeysym event 0) XK_UP) (execute-key (control #\p)))
	      ((= (xlookupkeysym event 0) XK_RIGHT)
	       (execute-key (control #\f)))
	      ((= (xlookupkeysym event 0) XK_DOWN) (execute-key (control #\n)))
	      ((= (xlookupkeysym event 0) XK_PRIOR) (execute-key (meta #\v)))
	      ((= (xlookupkeysym event 0) XK_NEXT) (execute-key (control #\v)))
	      ((= (xlookupkeysym event 0) XK_END) (execute-key (meta #\>)))))
	    (else (panic "Unrecognized event: ~s" event-type)))
      (loop)))))))

(define (quit-gui)
 (unwind-trail)
 (*quit-continuation* #f))

(define abort-gui
 (lambda ()
  (system "xkbbell")
  (set! *help?* #f)
  (redraw-buttons)
  (redraw-display-pane)
  (*abort-continuation* #f)))

(define (control character)
 (if (>= (char->integer character) 128)
     (let ((character (integer->char (- (char->integer character) 128))))
      (cond ((char-alphabetic? character)
	     (if (char-lower-case? character)
		 (integer->char (+ (- (char->integer character) 96) 128))
		 (integer->char (+ (- (char->integer character) 64) 128))))
	    ((char=? character #\space) (integer->char (+ 128 0)))
	    ((char=? character #\@) (integer->char (+ 128 0)))
	    ((char=? character #\[) (integer->char (+ 128 27)))
	    ((char=? character #\\) (integer->char (+ 128 28)))
	    ((char=? character #\]) (integer->char (+ 128 29)))
	    ((char=? character #\^) (integer->char (+ 128 30)))
	    ((char=? character #\_) (integer->char (+ 128 31)))
	    ((char=? character #\/) (integer->char (+ 128 31)))
	    (else (panic "Can't form control character: ~s" character))))
     (cond ((char-alphabetic? character)
	    (if (char-lower-case? character)
		(integer->char (- (char->integer character) 96))
		(integer->char (- (char->integer character) 64))))
	   ((char=? character #\space) (integer->char 0))
	   ((char=? character #\@) (integer->char 0))
	   ((char=? character #\[) (integer->char 27))
	   ((char=? character #\\) (integer->char 28))
	   ((char=? character #\]) (integer->char 29))
	   ((char=? character #\^) (integer->char 30))
	   ((char=? character #\_) (integer->char 31))
	   ((char=? character #\/) (integer->char 31))
	   (else (panic "Can't form control character: ~s" character)))))

(define (meta character)
 (if (>= (char->integer character) 128)
     character
     (integer->char (+ (char->integer character) 128))))

(define (define-key character documentation command)
 (set! *help*
       (cons (list character documentation)
	     (remove-if (lambda (help) (equal? character (first help)))
			*help*)))
 (when (eq? command abort-command) (set! *abort-key* character))
 (if (list? character)
     (let loop ((characters character) (comtab *comtab*))
      (cond
       ((null? (rest characters))
	(vector-set! comtab (char->integer (first characters)) command))
       (else (unless (vector? (vector-ref comtab
					  (char->integer (first characters))))
	      (vector-set! comtab
			   (char->integer (first characters))
			   (make-vector 256 #f)))
	     (loop (rest characters)
		   (vector-ref comtab (char->integer (first characters)))))))
     (vector-set! *comtab* (char->integer character) command)))

(define (define-button x y text-procedure bold?-procedure method)
 (let ((button (xcreatesimplewindow
		*display* *window* (+ (* x (+ *button-width* 4)) 2)
		(+ (* y (+ *button-height* 4)) 2)
		*button-width* *button-height* 1
		(xcolor-pixel (second *foreground*))
		(xcolor-pixel (second *background*)))))
  (when (eq? method abort-command) (set! *abort-button* button))
  (xselectinput
   *display* button (bit-or EXPOSUREMASK BUTTONPRESSMASK KEYPRESSMASK))
  (set-window-method!
   button
   'expose
   (lambda ()
    (let* ((text (if (procedure? text-procedure)
		     (text-procedure)
		     text-procedure))
	   (bold? (if (procedure? bold?-procedure)
		      (bold?-procedure)
		      bold?-procedure))
	   (text-width
	    (xtextwidth
	     (if bold? *bold-font* *roman-font*) text (string-length text)))
	   (text-x (quotient (- *button-width* text-width) 2))
	   (text-y (- *button-height* (+ *text-baseline* 2))))
     (xclearwindow *display* button)
     (xdrawstring *display* button (if bold? *bold-gc* *roman-gc*)
		  text-x text-y text (string-length text)))))
  (set-window-method! button 'BUTTONPRESS (lambda (x y button state) (method)))
  (set! *buttons* (cons button *buttons*))))

(define-syntax define-toggle-button
 (er-macro-transformer
  (lambda (form rename compare)
   (if (not (and (= (length form) 6) (symbol? (fifth form))))
       (error 'define-toggle-button "Improper DEFINE-TOGGLE-BUTTON: ~s" form))
   `(define-button ,(second form) ,(third form) ,(fourth form)
     (lambda () ,(fifth form))
     (lambda ()
      (set! ,(fifth form) (not ,(fifth form)))
      (redraw-buttons)
      (,(sixth form)))))))

(define-syntax define-radio-buttons
 (er-macro-transformer
  (lambda (form rename compare)
   (if (not (and (>= (length form) 3)
               (symbol? (second form))
               (every (lambda (element)
                       (and (list? element)
                          (= (length element) 4)
                          (symbol? (third element))))
                      (cdr (cdr (cdr form))))))
       (error 'define-radio-buttons "Improper DEFINE-RADIO-BUTTONS: ~s" form))
   `(begin
     ,@(map (lambda (element)
             `(define-button ,(first element) ,(second element)
               ,(fourth element)
               (lambda () (eq? ,(second form) ',(third element)))
               (lambda ()
                (set! ,(second form) ',(third element))
                (redraw-buttons)
                (,(third form)))))
            (cdr (cdr (cdr form))))))))

(define-syntax define-cycle-button
 (er-macro-transformer
  (lambda (form rename compare)
   (if (not (and (>= (length form) 6)
               (symbol? (fourth form))
               (every (lambda (element)
                       (and (list? element)
                          (= (length element) 2)
                          (symbol? (first element))))
                      (cdr (cdr (cdr (cdr (cdr form))))))))
       (error 'define-cycle-button "Improper DEFINE-CYCLE-BUTTON: ~s" form))
   (let ((symbols (map first (cdr (cdr (cdr (cdr (cdr form))))))))
    `(define-button ,(second form) ,(third form)
      (lambda ()
       (case ,(fourth form)
        ,@(map (lambda (element) `((,(first element)) ,(second element)))
               (cdr (cdr (cdr (cdr (cdr form))))))
        (else (fuck-up))))
      #f
      (lambda ()
       (set! ,(fourth form)
             (case ,(fourth form)
              ,@(map (lambda (s1 s2) `((,s1) (set! ,(fourth form) ',s2)))
                     symbols
                     (append (cdr symbols) (list (first symbols))))
              (else (fuck-up))))
       (redraw-buttons)
       (,(fifth form))))))))

(define-syntax define-integer-range-buttons
 (er-macro-transformer
  (lambda (form rename compare)
   (if (not (and (= (length form) 11) (symbol? (sixth form))))
    (error 'define-integer-range-buttons
     "Improper DEFINE-INTEGER-RANGE-BUTTONS: ~s" form))
   `(begin (define-button ,(second form) ,(third form) ,(ninth form)
            #f
            (lambda ()
             (when (= ,(sixth form) ,(seventh form)) (abort-gui))
             (set! ,(sixth form) (- ,(sixth form) 1))
             (redraw-buttons)
             (,(scheme2c-compatibility#eleventh form))))
           (define-button ,(fourth form) ,(fifth form) ,(tenth form)
            #f
            (lambda ()
             (when (= ,(sixth form) ,(eighth form)) (abort-gui))
             (set! ,(sixth form) (+ ,(sixth form) 1))
             (redraw-buttons)
             (,(scheme2c-compatibility#eleventh form))))))))

(define (pause)
 (when *pause?*
  (let ((old-status *status*))
   (status "Pause")
   (call-with-current-continuation
    (lambda (continue)
     (let ((event (make-xevent)))
      (let loop ()
       (*enable-background-task*)
       (xnextevent *display* event)
       (*disable-background-task*)
       (let ((event-type (xevent-xany-type event))
	     (window (xevent-xany-window event)))
	(cond ((= event-type MAPPINGNOTIFY) (xrefreshkeyboardmapping event))
	      ((= event-type EXPOSE) (send window 'expose))
	      ;; I don't know why these happen or what they mean.
	      ((= event-type NOEXPOSE) #f)
	      ((= event-type MOTIONNOTIFY) #f)
	      ((= event-type BUTTONPRESS) (continue #f))
	      ((= event-type BUTTONRELEASE) #f)
	      ((= event-type KEYPRESS)
	       (when (= (string-length (ylookupstring event)) 1)
		(let ((character (string-ref (ylookupstring event) 0)))
		 (cond ((char=? character #\space) (continue #f))
		       ((char=? character #\r) (set-pause! #f) (continue #f))
		       ;; needs work: Should do by abort-gui key and button.
		       ((char=? character #\q) (abort-gui))))))
	      (else (panic "Unrecognized event: ~s" event-type)))
	(loop))))))
   (status old-status))))

(define (tracking-pointer twice? press? tracker)
 (let ((old-status *status*)
       (event (make-xevent))
       (x #f)
       (y #f))
  (xselectinput *display*
		*window*
		(bit-or EXPOSUREMASK
			POINTERMOTIONMASK
			BUTTONPRESSMASK
			BUTTONRELEASEMASK
			KEYPRESSMASK))
  (xselectinput *display*
		*display-pane*
		(bit-or EXPOSUREMASK
			POINTERMOTIONMASK
			BUTTONPRESSMASK
			BUTTONRELEASEMASK
			KEYPRESSMASK))
  (status "Track")
  ;; Get pointer position and call tracker to draw.
  (let ((result (xquerypointer2 *display* *display-pane*)))
   (set! x (sixth result))
   (set! y (seventh result)))
  (tracker x y)
  (xflush *display*)
  ;; Event loop
  (let loop ()
   ;; Call tracker to erase.
   (when twice? (tracker x y) (xflush *display*))
   ;; Get pointer position and call tracker to draw.
   (let ((result (xquerypointer2 *display* *display-pane*)))
    (set! x (sixth result))
    (set! y (seventh result)))
   (tracker x y)
   (xflush *display*)
   ;; Wait for next event.
   (*enable-background-task*)
   (xnextevent *display* event)
   (*disable-background-task*)
   ;; needs work: Should allow abort-gui key and button.
   (let ((event-type (xevent-xany-type event)))
    (cond
     ;; If event is MotionNotify, gobble all following MotionNotify events.
     ((= event-type MOTIONNOTIFY)
      (let loop2 ()
       (when (> (xeventsqueued *display* QUEUEDALREADY) 0)
	(*enable-background-task*)
	(xnextevent *display* event)
	(*disable-background-task*)
	;; needs work: Should allow abort-gui key and button.
	(let ((event-type (xevent-xany-type event)))
	 (if (= event-type MOTIONNOTIFY)
	     (loop2)
	     (xputbackevent *display* event)))))
      (loop))
     ;; Ignore other events except BUTTONPRESS/Release, which exits main loop.
     ((not (= event-type (if press? BUTTONPRESS BUTTONRELEASE))) (loop)))))
  ;; Call tracker to erase; restore status; return final pointer position.
  (when twice? (tracker x y) (xflush *display*))
  (status old-status)
  (list x y)))

(define kill-application
 ;; note: KILL-APPLICATION has to be defined as follows to allow
 ;;       (SET-KILL-APPLICATION! ...) to work with separately compiled code.
 (lambda () #f))

(define (set-kill-application! procedure)
 ;; note: You need to set KILL-APPLICATION through a compiled procedure to
 ;;       allow separately compiled and interpreted code to work.
 (set! kill-application procedure))

(define (allocate-color-cube! reds greens blues)
 (unless *color-cube*
  (set! *color-cube* (make-vector (* reds greens blues)))
  (let ((colormap (xdefaultcolormap *display* *screen*))
	(i 0))
   (for-each-n
    (lambda (red)
     (for-each-n
      (lambda (green)
       (for-each-n
	(lambda (blue)
	 (let ((xcolor (make-xcolor)))
	  (set-xcolor-red! xcolor (quotient (* 65536 red) reds))
	  (set-xcolor-green! xcolor (quotient (* 65536 green) greens))
	  (set-xcolor-blue! xcolor (quotient (* 65536 blue) blues))
	  (when (zero? (xalloccolor *display* colormap xcolor))
	   (panic "Cannot allocate sufficient colors"))
	  (vector-set! *color-cube* i xcolor)
	  (set! i (+ i 1))))
	blues))
      greens))
    reds))))

(define (draw-pixmap pixmap x y)
 (let ((geometry (xgetgeometry *display* pixmap)))
  (xcopyarea *display* pixmap *display-pane* *color-gc*
	     0 0 (fifth geometry) (sixth geometry) x y)
  (xflush *display*)))

(define *frame-rate* 30.0)

(define (draw-pixmaps pixmaps x y)
 (for-each-vector
  (lambda (pixmap)
   (draw-pixmap pixmap x y)
   (usleep (inexact->exact (round (/ 1000000.0 *frame-rate*)))))
  pixmaps))

(define (free-pixmap pixmap) (xfreepixmap *display* pixmap))

(define (free-pixmaps pixmaps) (for-each-vector free-pixmap pixmaps))

(define (pnm->pixmap pnm)
 ;; needs work: Why don't I get exact RGB values?
 ;; needs work: Make special PGM version.
 (cond
  ((pbm? pnm) (pnm->pixmap (pbm->ppm pnm)))
  ((pgm? pnm) (pnm->pixmap (pgm->ppm pnm)))
  ((ppm? pnm)
   (let* ((default-visual (xdefaultvisual *display* *screen*))
	  (default-visual-class (visual-class default-visual))
	  (default-depth (xdefaultdepth *display* *screen*)))
    (cond
     ((= default-visual-class STATICGRAY)
      (panic "Cannot (yet) handle STATICGRAY visual"))
     ((= default-visual-class GRAYSCALE)
      (panic "Cannot (yet) handle GRAYSCALE visual"))
     ((= default-visual-class STATICCOLOR)
      (panic "Cannot (yet) handle STATICCOLOR visual"))
     ((= default-visual-class PSEUDOCOLOR)
      (unless (and (= (visual-map_entries default-visual) 256)
		   (= default-depth 8))
       (panic "Can (currently) only handle 8-bit PSEUDOCOLOR visual"))
      (allocate-color-cube! *reds* *greens* *blues*)
      (if *dither?*
	  (let* ((width (pnm-width pnm))
		 (height (pnm-height pnm))
		 (red (ppm-red pnm))
		 (green (ppm-green pnm))
		 (blue (ppm-blue pnm))
		 (maxval (ppm-maxval pnm))
		 (pic24 (make-string (* 3 width height)))
		 (rdisp (make-string (vector-length *color-cube*)))
		 (gdisp (make-string (vector-length *color-cube*)))
		 (bdisp (make-string (vector-length *color-cube*)))
		 (idisp (make-string (vector-length *color-cube*)))
		 (i 0))
	   (do ((y 0 (+ y 1))) ((= y height))
	    (do ((x 0 (+ x 1))) ((= x width))
	     (string-set!
	      pic24 i
	      (integer->char (quotient (* 255 (matrix-ref red y x)) maxval)))
	     (set! i (+ i 1))
	     (string-set!
	      pic24 i
	      (integer->char
	       (quotient (* 255 (matrix-ref green y x)) maxval)))
	     (set! i (+ i 1))
	     (string-set!
	      pic24 i
	      (integer->char (quotient (* 255 (matrix-ref blue y x)) maxval)))
	     (set! i (+ i 1))))
	   (do ((i 0 (+ i 1))) ((= i (vector-length *color-cube*)))
	    (let ((xcolor (vector-ref *color-cube* i)))
	     (string-set!
	      rdisp i (integer->char (quotient (xcolor-red xcolor) 256)))
	     (string-set!
	      gdisp i (integer->char (quotient (xcolor-green xcolor) 256)))
	     (string-set!
	      bdisp i (integer->char (quotient (xcolor-blue xcolor) 256)))
	     (string-set! idisp i (integer->char (xcolor-pixel xcolor)))))
	   (let* ((pic8
		   (c-docolordither
		    pic24 width height
		    rdisp gdisp bdisp idisp (vector-length *color-cube*)))
		  (ximage
		   (cons 'ximagep
			 (xcreateimage
			  (cdr *display*) (cdr default-visual) 8 ZPIXMAP 0
			  pic8 width height 8 0)))
		  (pixmap
		   (xcreatepixmap *display* *display-pane* width height 8)))
	    (xputimage
	     *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	    (xdestroyimage ximage)
	    pixmap))
	  (let* ((width (pnm-width pnm))
		 (height (pnm-height pnm))
		 (red (ppm-red pnm))
		 (green (ppm-green pnm))
		 (blue (ppm-blue pnm))
		 (maxval (+ (ppm-maxval pnm) 1))
		 (data (malloc (* width height)))
		 (ximage
		  (cons 'ximagep
			(xcreateimage
			 (cdr *display*) (cdr default-visual) 8 ZPIXMAP 0
			 data width height 8 0)))
		 (pixmap
		  (xcreatepixmap *display* *display-pane* width height 8)))
	   (do ((y 0 (+ y 1))) ((= y height))
	    (do ((x 0 (+ x 1))) ((= x width))
	     (xputpixel
	      ximage x y
	      (xcolor-pixel
	       (vector-ref
		*color-cube*
		(+ (* *greens*
		      *blues*
		      (min (inexact->exact
			    (round (/ (* *reds* (matrix-ref red y x)) maxval)))
			   (- *reds* 1)))
		   (* *blues*
		      (min
		       (inexact->exact
			(round (/ (* *greens* (matrix-ref green y x)) maxval)))
		       (- *greens* 1)))
		   (min (inexact->exact
			 (round (/ (* *blues* (matrix-ref blue y x)) maxval)))
			(- *blues* 1))))))))
	   (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	   (xdestroyimage ximage)
	   pixmap)))
     ((= default-visual-class TRUECOLOR)
      (cond
       ((and (= (visual-red_mask default-visual) 63488)
	     (= (visual-green_mask default-visual) 2016)
	     (= (visual-blue_mask default-visual) 31)
	     (= default-depth 16))
	(let* ((width (pnm-width pnm))
	       (height (pnm-height pnm))
	       (red (ppm-red pnm))
	       (green (ppm-green pnm))
	       (blue (ppm-blue pnm))
	       (maxval (+ (ppm-maxval pnm) 1))
	       (data (malloc (* 2 width height)))
	       (ximage
		(cons 'ximagep
		      (xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       ZPIXMAP 0 data width height default-depth 0)))
	       (pixmap
		(xcreatepixmap
		 *display* *display-pane* width height default-depth)))
	 (do ((y 0 (+ y 1))) ((= y height))
	  (do ((x 0 (+ x 1))) ((= x width))
	   (xputpixel
	    ximage x y
	    (+ (* (quotient (* 32 (matrix-ref red y x)) maxval) 2048)
	       (* (quotient (* 64 (matrix-ref green y x)) maxval) 32)
	       (quotient (* 32 (matrix-ref blue y x)) maxval)))))
	 (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	 (xdestroyimage ximage)
	 pixmap))
       ((and (= (visual-red_mask default-visual) 16711680)
	     (= (visual-green_mask default-visual) 65280)
	     (= (visual-blue_mask default-visual) 255)
	     (or (= default-depth 24) (= default-depth 32)))
	(let* ((width (pnm-width pnm))
	       (height (pnm-height pnm))
	       (red (ppm-red pnm))
	       (green (ppm-green pnm))
	       (blue (ppm-blue pnm))
	       (maxval (+ (ppm-maxval pnm) 1))
	       (data (malloc (* 4 width height)))
	       (ximage
		(cons 'ximagep
		      (xcreateimage
		       (cdr *display*) (cdr default-visual) default-depth
		       ZPIXMAP 0 data width height 32 0)))
	       (pixmap
		(xcreatepixmap
		 *display* *display-pane* width height default-depth)))
	 (do ((y 0 (+ y 1))) ((= y height))
	  (do ((x 0 (+ x 1))) ((= x width))
	   (xputpixel
	    ximage x y
	    (+ (* 65536 (quotient (* 256 (matrix-ref red y x)) maxval))
	       (* 256 (quotient (* 256 (matrix-ref green y x)) maxval))
	       (quotient (* 256 (matrix-ref blue y x)) maxval)))))
	 (xputimage *display* pixmap *color-gc* ximage 0 0 0 0 width height)
	 (xdestroyimage ximage)
	 pixmap))
       (else
	(panic "Can (currently) only handle 16-bit (5-6-5), 24-bit, and 32-bit TRUECOLOR visuals"))))
     ((= default-visual-class DIRECTCOLOR)
      (panic "Cannot (yet) handle DIRECTCOLOR visual"))
     (else (panic "Unrecognized visual")))))
  (else (panic "Argument not PNM"))))

(define (pnm-movie->pixmaps pnm-movie) (map-vector pnm->pixmap pnm-movie))

(define-structure tree-node width height offset text bold? procedure daughters)

(define (tree->tree-node tree)
 ;; needs work: This doesn't do any error checking.
 (let ((daughters (map tree->tree-node (rest (rest (rest tree)))))
       (bold? (first tree))
       (text (second tree))
       (procedure (third tree)))
  (make-tree-node
   (max (qmap-reduce + 0 tree-node-width daughters)
	(+ (xtextwidth
	    (if bold? *bold-font* *roman-font*) text (string-length text))
	   10))
   (if (null? daughters)
       *text-height*
       (+ (qmap-reduce max 0 tree-node-height daughters) *text-height* 20))
   (if (or (null? daughters)
	   (< (qmap-reduce + 0 tree-node-width daughters)
	      (+ (xtextwidth (if bold? *bold-font* *roman-font*)
			     text (string-length text))
		 10)))
       (quotient
	(+ (xtextwidth
	    (if bold? *bold-font* *roman-font*) text (string-length text))
	   10)
	2)
       (+ (quotient
	   (- (+ (qmap-reduce + 0 tree-node-width (rest (reverse daughters)))
		 (tree-node-offset (last daughters)))
	      (tree-node-offset (first daughters)))
	   2)
	  (tree-node-offset (first daughters))))
   text
   bold?
   procedure
   daughters)))

(define (draw-tree-node tree-node x y)
 (let* ((width (xtextwidth
		(if (tree-node-bold? tree-node) *bold-font* *roman-font*)
		(tree-node-text tree-node)
		(string-length (tree-node-text tree-node))))
	(x0 (- (+ x (tree-node-offset tree-node)) (quotient width 2))))
  (xdrawstring *display* *display-pane*
	       (if (tree-node-bold? tree-node) *bold-gc* *roman-gc*)
	       x0
	       (+ y (- *text-height* (+ *text-baseline* -3)))
	       (tree-node-text tree-node)
	       (string-length (tree-node-text tree-node)))
  (define-region x0 y width *text-height*
   (lambda (x1 y1)
    ((tree-node-procedure tree-node))
    (redraw-display-pane)))
  (let loop ((x1 (+ x (quotient
		       (- (tree-node-width tree-node)
			  (qmap-reduce +
				      0
				      tree-node-width
				      (tree-node-daughters tree-node)))
		       2)))
	     (daughters (tree-node-daughters tree-node)))
   (unless (null? daughters)
    (xdrawline *display* *display-pane* *thin-gc*
	       (+ x (tree-node-offset tree-node))
	       (+ y *text-height* 2)
	       (+ x1 (tree-node-offset (first daughters)))
	       (+ y *text-height* 21))
    (draw-tree-node (first daughters) x1 (+ y *text-height* 20))
    (loop (+ x1 (tree-node-width (first daughters))) (rest daughters))))))

(define (tree-height tree) (tree-node-height (tree->tree-node tree)))

(define (draw-tree tree x y) (draw-tree-node (tree->tree-node tree) x y))

;;; needs work: To add bold face and regions to alist display.

(define-structure alist-node width height offset keys values)

(define (alist->alist-node alist)
 ;; needs work: This doesn't do any error checking.
 (if (string? alist)
     alist
     (let* ((keys (map car alist))
	    (values (map alist->alist-node (map cdr alist)))
	    (offset
	     (qmap-reduce
	      max
	      0
	      (lambda (key) (xtextwidth *roman-font* key (string-length key)))
	      keys)))
      (make-alist-node
       (+ 5
	  offset
	  5
	  (qmap-reduce max
		      0
		      (lambda (value)
		       (if (string? value)
			   (xtextwidth *roman-font* value (string-length value))
			   (alist-node-width value)))
		      values)
	  5)
       (+ 1
	  (max (- *text-height* 6)
	       (qmap-reduce +
			   0
			   (lambda (value)
			    (max *text-height*
				 (if (string? value)
				     *text-height*
				     (alist-node-height value))))
			   values))
	  (* 2 (max (- (length keys) 1) 0))
	  5)
       (+ offset 10)
       keys
       values))))

(define (draw-alist-node alist-node x y)
 (cond
  ((string? alist-node)
   (xdrawstring *display* *display-pane* *roman-gc*
		x
		(+ y (- *roman-height* (+ *roman-baseline* -3)))
		alist-node (string-length alist-node)))
  (else
   (xdrawline *display* *display-pane* *thin-gc* x y (+ x 5) y)
   (xdrawline *display* *display-pane* *thin-gc*
	      x y x (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      x (+ y (alist-node-height alist-node))
	      (+ x 5) (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node)) y
	      (+ x (alist-node-width alist-node) -5) y)
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node))
	      y
	      (+ x (alist-node-width alist-node))
	      (+ y (alist-node-height alist-node)))
   (xdrawline *display* *display-pane* *thin-gc*
	      (+ x (alist-node-width alist-node))
	      (+ y (alist-node-height alist-node))
	      (+ x (alist-node-width alist-node) -5)
	      (+ y (alist-node-height alist-node)))
   (let loop ((keys (alist-node-keys alist-node))
	      (values (alist-node-values alist-node))
	      (y (+ y 1)))
    (unless (null? keys)
     (cond ((string? (first values))
	    (xdrawstring *display* *display-pane* *roman-gc*
			 (+ x 5)
			 (+ y (- *roman-height* (+ *roman-baseline* -3)))
			 (first keys) (string-length (first keys)))
	    (xdrawstring *display* *display-pane* *roman-gc*
			 (+ x (alist-node-offset alist-node))
			 (+ y (- *roman-height* (+ *roman-baseline* -3)))
			 (first values) (string-length (first values)))
	    (loop (rest keys) (rest values) (+ y *text-height* 2)))
	   (else (xdrawstring *display* *display-pane* *roman-gc*
			      (+ x 5)
			      (+ y (- *roman-height* (+ *roman-baseline* -3)))
			      (first keys) (string-length (first keys)))
		 (draw-alist-node
		  (first values) (+ x (alist-node-offset alist-node)) (+ y 2))
		 (loop (rest keys)
		       (rest values)
		       (+ y (alist-node-height (first values)) 2)))))))))

(define (alist-height alist) (alist-node-height (alist->alist-node alist)))

(define (draw-alist alist x y) (draw-alist-node (alist->alist-node alist) x y))

(define abort-command abort-gui)

(define (help-command)
 (set! *help?* #t)
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-up-line-command)
 (set! *help?* #t)
 (set! *first-help-line* (max (- *first-help-line* 1) 0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-down-line-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (min (+ *first-help-line* 1)
		 (- (length *help*)
		    (quotient *display-pane-height* *text-height*)))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-up-page-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max
	(- *first-help-line* (quotient *display-pane-height* *text-height*))
	0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-down-page-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (min (+ *first-help-line*
		    (quotient *display-pane-height* *text-height*))
		 (- (length *help*)
		    (quotient *display-pane-height* *text-height*)))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-beginning-command)
 (set! *help?* #t)
 (set! *first-help-line* 0)
 (redraw-buttons)
 (redraw-display-pane))

(define (help-scroll-end-command)
 (set! *help?* #t)
 (set! *first-help-line*
       (max (- (length *help*) (quotient *display-pane-height* *text-height*))
	    0))
 (redraw-buttons)
 (redraw-display-pane))

(define (echo-pane-command editor)
 ;; needs work: This is not really a command.
 (message "")
 (let ((result (editor *input* *input-position*)))
  (set! *input* (first result))
  (set! *input-position* (second result)))
 (redraw-echo-pane))

(define (echo-pane-insert-character-command character)
 (echo-pane-command (string-insert-character character)))

(define (echo-pane-beginning-of-line-command)
 (echo-pane-command string-beginning-of-line))

(define (echo-pane-backward-char-command)
 (echo-pane-command string-backward-char))

(define (echo-pane-delete-char-command)
 (echo-pane-command string-delete-char))

(define (echo-pane-end-of-line-command)
 (echo-pane-command string-end-of-line))

(define (echo-pane-forward-char-command)
 (echo-pane-command string-forward-char))

(define (echo-pane-kill-line-command)
 (echo-pane-command string-kill-line))

(define (echo-pane-backward-delete-char-command)
 (echo-pane-command string-backward-delete-char))

(define (echo-pane-backward-word-command)
 (echo-pane-command string-backward-word))

(define (echo-pane-kill-word-command)
 (echo-pane-command string-kill-word))

(define (echo-pane-forward-word-command)
 (echo-pane-command string-forward-word))

(define (echo-pane-backward-kill-word-command)
 (echo-pane-command string-backward-kill-word))

(define-syntax define-display-pane-application
 ;; (DEFINE-DISPLAY-PANE-APPLICATION
 ;;  NAME
 ;;  DISPLAY-PANE-WIDTH
 ;;  DISPLAY-PANE-HEIGHT
 ;;  PRE-INITIALIZE-PROCEDURE
 ;;  POST-INITIALIZE-PROCEDURE
 ;;  FINALIZE-PROCEDURE
 ;;  REDRAW-PROCEDURE)
 (er-macro-transformer
  (lambda (form rename compare)
   `(define (,(second form) arguments)
     (let ((stalin? #f)
	   (display-pane-width ,(third form))
	   (display-pane-height ,(fourth form))
	   (pre-initialize-procedure ,(fifth form))
	   (post-initialize-procedure ,(sixth form))
	   (finalize-procedure ,(seventh form))
	   (redraw-procedure ,(eighth form)))
      (set! *post-initialize-procedure* post-initialize-procedure)
      (set! *transcript-pane* #f)
      (set! *echo-pane* #f)
      (set! *status-pane* #f)
      (set! *message-pane* #f)
      (set! *display* (xopendisplay *display-name*))
      (when (null-pointer? *display*)
       (panic "Cannot connect to X server: ~a" (xdisplayname *display-name*)))
      (set! *screen* (xdefaultscreen *display*))
      (set! *root-window* (xrootwindow *display* *screen*))
      (set! *button-width* 0)
      (set! *button-height* 0)
      (cond
       (stalin?
	(set! *white-pixel* (xwhitepixel *display* *screen*))
	(set! *black-pixel* (xblackpixel *display* *screen*)))
       (else
	(set! *background*
	      (xallocnamedcolor3 *display*
				(xdefaultcolormap *display* *screen*)
				*background-color*))
	(unless (= (first *background*) 1)
	 (panic "Can't allocate background colorcell"))
	(set! *foreground*
	      (xallocnamedcolor3 *display*
				(xdefaultcolormap *display* *screen*)
				*foreground-color*))
	(unless (= (first *foreground*) 1)
	 (panic "Can't allocate foreground colorcell"))))
      (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
      (when (null-pointer? *roman-font*)
       (panic "Cannot open font: ~a" *roman-font-name*))
      (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
      (when (null-pointer? *bold-font*)
       (panic "Cannot open font: ~a" *bold-font-name*))
      (set! *roman-height*
	    (+ (xfontstruct-ascent *roman-font*)
	       (xfontstruct-descent *roman-font*)))
      (set! *bold-height*
	    (+ (xfontstruct-ascent *bold-font*)
	       (xfontstruct-descent *bold-font*)))
      (set! *text-height*
	    (+ (max (xfontstruct-ascent *roman-font*)
		    (xfontstruct-ascent *bold-font*))
	       (max (xfontstruct-descent *roman-font*)
		    (xfontstruct-descent *bold-font*))))
      (set! *roman-baseline* (xfontstruct-descent *roman-font*))
      (set! *bold-baseline* (xfontstruct-descent *bold-font*))
      (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
      (set! *display-pane-width* display-pane-width)
      (set! *display-pane-height* display-pane-height)
      (set! *who-line-height* 0)
      (set! *window*
	    (xcreatesimplewindow
	     *display* *root-window*
	     *window-position-x* *window-position-y*
	     *display-pane-width* *display-pane-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xstorename *display* *window* *program*)
      (xseticonname *display* *window* *program*)
      (set! *display-pane* *window*)
      (xselectinput *display*
		    *display-pane*
		    (bit-or EXPOSUREMASK
			    POINTERMOTIONMASK
			    BUTTONPRESSMASK
			    BUTTONRELEASEMASK
			    KEYPRESSMASK))
      (set! *thin-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thin-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thin-gc* 0 LINESOLID CAPROUND JOINROUND)
      (set! *thin-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thin-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thin-flipping-gc* 0 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *thin-flipping-gc* GXXOR)
      (set! *medium-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *medium-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *medium-gc* 2 LINESOLID CAPROUND JOINROUND)
      (set! *medium-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *medium-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *medium-flipping-gc* 2 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *medium-flipping-gc* GXXOR)
      (set! *thick-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thick-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thick-gc* 5 LINESOLID CAPROUND JOINROUND)
      (set! *thick-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thick-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thick-flipping-gc* 5 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *thick-flipping-gc* GXXOR)
      (set! *dashed-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *dashed-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *dashed-gc* 0 LINEONOFFDASH CAPROUND JOINROUND)
      (set! *dashed-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *dashed-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *dashed-flipping-gc* 0 LINEONOFFDASH CAPROUND JOINROUND)
      (xsetfunction *display* *dashed-flipping-gc* GXXOR)
      (set! *roman-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *roman-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *roman-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *roman-gc* (xfontstruct-fid *roman-font*))
      (set! *bold-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *bold-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *bold-gc* (xfontstruct-fid *bold-font*))
      (set! *bold-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *bold-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetfont
       *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
      (xsetlineattributes
       *display* *bold-flipping-gc* 0 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *bold-flipping-gc* GXXOR)
      (unless stalin?
       (set! *light-gray*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Light Gray"))
       (unless (= (first *light-gray*) 1)
	(panic "Can't allocate light gray colorcell"))
       (set! *light-gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *light-gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *light-gray-gc*
		       (xcolor-pixel (second *light-gray*)))
       (xsetlineattributes
	*display* *light-gray-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *gray*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Gray"))
       (unless (= (first *gray*) 1)
	(panic "Can't allocate gray colorcell"))
       (set! *gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *gray-gc*
		       (xcolor-pixel (second *gray*)))
       (xsetlineattributes
	*display* *gray-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *red*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Red"))
       (unless (= (first *red*) 1)
	(panic "Can't allocate red colorcell"))
       (set! *red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *red-gc*
		       (xcolor-pixel (second *red*)))
       (xsetfont
	*display* *red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *red-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-red*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Red"))
       (unless (= (first *dark-red*) 1)
	(panic "Can't allocate dark red colorcell"))
       (set! *dark-red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-red-gc*
		       (xcolor-pixel (second *dark-red*)))
       (xsetfont
	*display* *dark-red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-red-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *green*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Green"))
       (unless (= (first *green*) 1)
	(panic "Can't allocate green colorcell"))
       (set! *green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *green-gc*
		       (xcolor-pixel (second *green*)))
       (xsetfont
	*display* *green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *green-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-green*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Green"))
       (unless (= (first *dark-green*) 1)
	(panic "Can't allocate dark green colorcell"))
       (set! *dark-green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-green-gc*
		       (xcolor-pixel (second *dark-green*)))
       (xsetfont
	*display* *dark-green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-green-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *blue*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Blue"))
       (unless (= (first *blue*) 1)
	(panic "Can't allocate blue colorcell"))
       (set! *blue-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *blue-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *blue-gc*
		       (xcolor-pixel (second *blue*)))
       (xsetfont
	*display* *blue-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *blue-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *yellow*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Yellow"))
       (unless (= (first *yellow*) 1)
	(panic "Can't allocate yellow colorcell"))
       (set! *yellow-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *yellow-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *yellow-gc*
		       (xcolor-pixel (second *yellow*)))
       (xsetfont
	*display* *yellow-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *yellow-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *violet*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Violet"))
       (unless (= (first *violet*) 1)
	(panic "Can't allocate violet colorcell"))
       (set! *violet-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *violet-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *violet-gc*
		       (xcolor-pixel (second *violet*)))
       (xsetfont
	*display* *violet-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *violet-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *orange*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Orange"))
       (unless (= (first *orange*) 1)
	(panic "Can't allocate orange colorcell"))
       (set! *orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *orange-gc*
		       (xcolor-pixel (second *orange*)))
       (xsetfont
	*display* *orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *orange-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-orange*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Orange"))
       (unless (= (first *dark-orange*) 1)
	(panic "Can't allocate dark orange colorcell"))
       (set! *dark-orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-orange-gc*
		       (xcolor-pixel (second *dark-orange*)))
       (xsetfont
	*display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-orange-gc* 0 LINESOLID CAPROUND JOINROUND))
      (set! *color-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *color-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *color-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *color-gc* 0 LINESOLID CAPROUND JOINROUND)
      (set! *window-methods* '())
      (set! *abort-button* #f)
      (set! *abort-key* #f)
      (set! *comtab* (make-vector 256 #f))
      (set! *help* '())
      (define-key (control #\h) "Help" help-command)
      (set! *help* '())
      (define-key (control #\n) "Scroll help window down one line"
       help-scroll-down-line-command)
      (define-key (control #\p) "Scroll help window up one line"
       help-scroll-up-line-command)
      (define-key (control #\v) "Scroll help window down one page"
       help-scroll-down-page-command)
      (define-key (meta #\v) "Scroll help window up one page"
       help-scroll-up-page-command)
      (define-key (meta #\<) "Scroll help window to beginning"
       help-scroll-beginning-command)
      (define-key (meta #\>) "Scroll help window to end"
       help-scroll-end-command)
      (set! *help-comtab* *comtab*)
      (set! *comtab* (make-vector 256 #f))
      (set! *prefix* '())
      (set! *status* "Tyi")
      (set! *message* "")
      (set! *redraw-procedure* redraw-procedure)
      (set! *buttons* '())
      (set! *pause?* #f)
      (set! *help?* #f)
      (set! *clear-display-pane?* #t)
      (let ((hints (make-xwmhints)))
       (set-xwmhints-input! hints 1)
       (set-xwmhints-flags! hints INPUTHINT)
       (xsetwmhints *display* *window* hints))
      (let ((hints (make-xsizehints)))
       (when *window-position?*
	(set-xsizehints-x! hints *window-position-x*)
	(set-xsizehints-y! hints *window-position-y*))
       (set-xsizehints-min_width! hints *display-pane-width*)
       (set-xsizehints-max_width! hints *display-pane-width*)
       (set-xsizehints-min_height! hints *display-pane-height*)
       (set-xsizehints-max_height! hints *display-pane-height*)
       (set-xsizehints-flags! hints
			  (if *window-position?*
			      (+ USPOSITION PPOSITION PMINSIZE PMAXSIZE)
			      (+ PMINSIZE PMAXSIZE)))
       (xsetwmnormalhints *display* *window* hints))
      (pre-initialize-procedure)
      (set-window-method! *display-pane* 'expose redraw-display-pane)
      (set-window-method! *display-pane* 'BUTTONPRESS region-handler)
      (when *transcript-pane*
       (set-window-method!
	*transcript-pane* 'expose redraw-transcript-pane))
      (when *echo-pane*
       (set-window-method! *echo-pane* 'expose redraw-echo-pane))
      (set-kill-application!
       (lambda ()
	(set-kill-application! (lambda () #t))
	(finalize-procedure)
	(when *display*
	 (xfreegc *display* *thin-gc*)
	 (xfreegc *display* *thin-flipping-gc*)
	 (xfreegc *display* *medium-gc*)
	 (xfreegc *display* *medium-flipping-gc*)
	 (xfreegc *display* *thick-gc*)
	 (xfreegc *display* *thick-flipping-gc*)
	 (xfreegc *display* *dashed-gc*)
	 (xfreegc *display* *dashed-flipping-gc*)
	 (xfreegc *display* *roman-gc*)
	 (xfreegc *display* *bold-gc*)
	 (xfreegc *display* *bold-flipping-gc*)
	 (unless stalin?
	  (xfreegc *display* *light-gray-gc*)
	  (xfreegc *display* *gray-gc*)
	  (xfreegc *display* *red-gc*)
	  (xfreegc *display* *dark-red-gc*)
	  (xfreegc *display* *green-gc*)
	  (xfreegc *display* *dark-green-gc*)
	  (xfreegc *display* *blue-gc*)
	  (xfreegc *display* *yellow-gc*)
	  (xfreegc *display* *violet-gc*)
	  (xfreegc *display* *orange-gc*)
	  (xfreegc *display* *dark-orange-gc*)
	  (xfreegc *display* *color-gc*)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *background*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *foreground*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *light-gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *blue*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *yellow*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *violet*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *orange*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-orange*))))
		       1
		       0))
	 (xunloadfont *display* (xfontstruct-fid *roman-font*))
	 (xunloadfont *display* (xfontstruct-fid *bold-font*))
	 (xdestroywindow *display* *window*)
	 (xclosedisplay *display*)
	 (set! *display* #f))
	#t))
      (xmapsubwindows *display* *window*)
      (xmapraised *display* *window*)
      (process-events)
      (kill-application))))))

(define-syntax define-application
 ;; (DEFINE-APPLICATION
 ;;  NAME
 ;;  DISPLAY-PANE-WIDTH
 ;;  DISPLAY-PANE-HEIGHT
 ;;  TRANSCRIPT-LINES
 ;;  BUTTON-ROWS
 ;;  BUTTOM-COLUMNS
 ;;  PRE-INITIALIZE-PROCEDURE
 ;;  POST-INITIALIZE-PROCEDURE
 ;;  FINALIZE-PROCEDURE
 ;;  REDRAW-PROCEDURE
 ;;  LISTENER-PROCEDURE)
 (er-macro-transformer
  (lambda (form rename compare)
   `(define (,(second form) arguments)
     (let* ((stalin? #f)
	    (display-pane-width ,(third form))
	    (display-pane-height ,(fourth form))
	    (transcript-lines ,(fifth form))
	    (button-rows ,(sixth form))
	    (button-columns ,(seventh form))
	    (button-width
	     (if display-pane-width
		 (- (quotient (+ display-pane-width 4) button-columns)
		    4)
		 100))
	    (width (if display-pane-width
		       (+ display-pane-width 6)
		       (+ (* button-columns (+ button-width 4)) 2)))
	    (pre-initialize-procedure ,(eighth form))
	    (post-initialize-procedure ,(ninth form))
	    (finalize-procedure ,(tenth form))
	    (redraw-procedure ,(scheme2c-compatibility#eleventh form))
	    (listener-procedure
	     ,(if (= (length form) 12) (scheme2c-compatibility#twelfth form) '(lambda () #f))))
      (set! *post-initialize-procedure* post-initialize-procedure)
      (set! *transcript-pane* #f)
      (set! *echo-pane* #f)
      (set! *display* (xopendisplay *display-name*))
      (when (null-pointer? *display*)
       (panic "Cannot connect to X server: ~a" (xdisplayname *display-name*)))
      (set! *screen* (xdefaultscreen *display*))
      (set! *root-window* (xrootwindow *display* *screen*))
      (cond
       (stalin?
	(set! *white-pixel* (xwhitepixel *display* *screen*))
	(set! *black-pixel* (xblackpixel *display* *screen*)))
       (else
	(set! *background*
	      (xallocnamedcolor3 *display*
				(xdefaultcolormap *display* *screen*)
				*background-color*))
	(unless (= (first *background*) 1)
	 (panic "Can't allocate background colorcell"))
	(set! *foreground*
	      (xallocnamedcolor3 *display*
				(xdefaultcolormap *display* *screen*)
				*foreground-color*))
	(unless (= (first *foreground*) 1)
	 (panic "Can't allocate foreground colorcell"))))
      (set! *roman-font* (xloadqueryfont *display* *roman-font-name*))
      (when (null-pointer? *roman-font*)
       (panic "Cannot open font: ~a" *roman-font-name*))
      (set! *bold-font* (xloadqueryfont *display* *bold-font-name*))
      (when (null-pointer? *bold-font*)
       (panic "Cannot open font: ~a" *bold-font-name*))
      (set! *roman-height*
	    (+ (xfontstruct-ascent *roman-font*)
	       (xfontstruct-descent *roman-font*)))
      (set! *bold-height*
	    (+ (xfontstruct-ascent *bold-font*)
	       (xfontstruct-descent *bold-font*)))
      (set! *text-height*
	    (+ (max (xfontstruct-ascent *roman-font*)
		    (xfontstruct-ascent *bold-font*))
	       (max (xfontstruct-descent *roman-font*)
		    (xfontstruct-descent *bold-font*))))
      (set! *roman-baseline* (xfontstruct-descent *roman-font*))
      (set! *bold-baseline* (xfontstruct-descent *bold-font*))
      (set! *text-baseline* (max *roman-baseline* *bold-baseline*))
      (set! *button-width* button-width)
      (set! *button-height* (+ *text-height* 4))
      (set! *display-pane-width* (- width 6))
      (set! *display-pane-height* display-pane-height)
      (when transcript-lines
       (unless (zero? transcript-lines)
	(set! *transcript-pane-height*
	      (+ (* transcript-lines *text-height*) 4)))
       (set! *echo-pane-height* (+ *text-height* 4)))
      (set! *who-line-height* (+ *text-height* 4))
      (set! *status-pane-width*
	    (+ (max (xtextwidth *roman-font* "Tyi" 3)
		    (max (xtextwidth *roman-font* "Run" 3)
                         (max (xtextwidth *roman-font* "Pause" 5)
                              (xtextwidth *roman-font* "Track" 5))))
	       4))
      (set! *window*
	    (xcreatesimplewindow
	     *display* *root-window*
	     *window-position-x* *window-position-y*
	     width
	     (if transcript-lines
		 (if (zero? transcript-lines)
		     (+ (* button-rows (+ *button-height* 4))
			*display-pane-height*
			*echo-pane-height*
			*who-line-height*
			14)
		     (+ (* button-rows (+ *button-height* 4))
			*display-pane-height*
			*transcript-pane-height*
			*echo-pane-height*
			*who-line-height*
			18))
		 (+ (* button-rows (+ *button-height* 4))
		    *display-pane-height*
		    *who-line-height*
		    10))
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xstorename *display* *window* *program*)
      (xseticonname *display* *window* *program*)
      (xselectinput *display*
		    *window*
		    (bit-or EXPOSUREMASK
			    POINTERMOTIONMASK
			    BUTTONPRESSMASK
			    BUTTONRELEASEMASK
			    KEYPRESSMASK))
      (set! *display-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     2 (+ (* button-rows (+ *button-height* 4)) 2)
	     *display-pane-width* *display-pane-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput *display*
		    *display-pane*
		    (bit-or EXPOSUREMASK
			    POINTERMOTIONMASK
			    BUTTONPRESSMASK
			    BUTTONRELEASEMASK
			    KEYPRESSMASK))
      (when transcript-lines
       (unless (zero? transcript-lines)
	(set! *transcript-pane*
	      (xcreatesimplewindow
	       *display* *window*
	       2
	       (+ (* button-rows (+ *button-height* 4))
		  *display-pane-height*
		  6)
	       *display-pane-width* *transcript-pane-height* 1
	       (if stalin?
		   *black-pixel*
		   (xcolor-pixel (second *foreground*)))
	       (if stalin?
		   *white-pixel*
		   (xcolor-pixel (second *background*)))))
	(xselectinput
	 *display* *transcript-pane* (bit-or EXPOSUREMASK KEYPRESSMASK)))
       (set! *echo-pane*
	     (xcreatesimplewindow
	      *display* *window*
	      2
	      (if (zero? transcript-lines)
		  (+ (* button-rows (+ *button-height* 4))
		     *display-pane-height*
		     6)
		  (+ (* button-rows (+ *button-height* 4))
		     *display-pane-height* *transcript-pane-height* 10))
	      *display-pane-width* *echo-pane-height* 1
	      (if stalin?
		  *black-pixel*
		  (xcolor-pixel (second *foreground*)))
	      (if stalin?
		  *white-pixel*
		  (xcolor-pixel (second *background*)))))
       (xselectinput
	*display* *echo-pane* (bit-or EXPOSUREMASK KEYPRESSMASK)))
      (set! *status-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     2
	     (+ (* button-rows (+ *button-height* 4))
		*display-pane-height*
		(if transcript-lines
		    (if (zero? transcript-lines)
			(+ *echo-pane-height* 10)
			(+ *transcript-pane-height*
			   *echo-pane-height*
			   14))
		    6))
	     *status-pane-width* *who-line-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput
       *display* *status-pane* (bit-or EXPOSUREMASK KEYPRESSMASK))
      (set! *message-pane*
	    (xcreatesimplewindow
	     *display* *window*
	     (+ *status-pane-width* 6)
	     (+ (* button-rows (+ *button-height* 4))
		*display-pane-height*
		(if transcript-lines
		    (if (zero? transcript-lines)
			(+ *echo-pane-height* 10)
			(+ *transcript-pane-height*
			   *echo-pane-height*
			   14))
		    6))
	     (- width *status-pane-width* 10) *who-line-height*
	     1
	     (if stalin?
		 *black-pixel*
		 (xcolor-pixel (second *foreground*)))
	     (if stalin?
		 *white-pixel*
		 (xcolor-pixel (second *background*)))))
      (xselectinput
       *display* *message-pane* (bit-or EXPOSUREMASK KEYPRESSMASK))
      (set! *thin-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thin-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thin-gc* 0 LINESOLID CAPROUND JOINROUND)
      (set! *thin-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thin-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thin-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thin-flipping-gc* 0 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *thin-flipping-gc* GXXOR)
      (set! *medium-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *medium-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *medium-gc* 2 LINESOLID CAPROUND JOINROUND)
      (set! *medium-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *medium-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *medium-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *medium-flipping-gc* 2 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *medium-flipping-gc* GXXOR)
      (set! *thick-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *thick-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *thick-gc* 5 LINESOLID CAPROUND JOINROUND)
      (set! *thick-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *thick-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *thick-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *thick-flipping-gc* 5 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *thick-flipping-gc* GXXOR)
      (set! *dashed-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *dashed-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *dashed-gc* 0 LINEONOFFDASH CAPROUND JOINROUND)
      (set! *dashed-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *dashed-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *dashed-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetlineattributes
       *display* *dashed-flipping-gc* 0 LINEONOFFDASH CAPROUND JOINROUND)
      (xsetfunction *display* *dashed-flipping-gc* GXXOR)
      (set! *roman-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *roman-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *roman-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *roman-gc* (xfontstruct-fid *roman-font*))
      (set! *bold-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *bold-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetfont
       *display* *bold-gc* (xfontstruct-fid *bold-font*))
      (set! *bold-flipping-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *bold-flipping-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetforeground *display* *bold-flipping-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetfont
       *display* *bold-flipping-gc* (xfontstruct-fid *bold-font*))
      (xsetlineattributes
       *display* *bold-flipping-gc* 0 LINESOLID CAPROUND JOINROUND)
      (xsetfunction *display* *bold-flipping-gc* GXXOR)
      (unless stalin?
       (set! *light-gray*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Light Gray"))
       (unless (= (first *light-gray*) 1)
	(panic "Can't allocate light gray colorcell"))
       (set! *light-gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *light-gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *light-gray-gc*
		       (xcolor-pixel (second *light-gray*)))
       (xsetlineattributes
	*display* *light-gray-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *gray*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Gray"))
       (unless (= (first *gray*) 1)
	(panic "Can't allocate gray colorcell"))
       (set! *gray-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *gray-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *gray-gc*
		       (xcolor-pixel (second *gray*)))
       (xsetlineattributes
	*display* *gray-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *red*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Red"))
       (unless (= (first *red*) 1)
	(panic "Can't allocate red colorcell"))
       (set! *red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *red-gc*
		       (xcolor-pixel (second *red*)))
       (xsetfont
	*display* *red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *red-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-red*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Red"))
       (unless (= (first *dark-red*) 1)
	(panic "Can't allocate dark red colorcell"))
       (set! *dark-red-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-red-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-red-gc*
		       (xcolor-pixel (second *dark-red*)))
       (xsetfont
	*display* *dark-red-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-red-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *green*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Green"))
       (unless (= (first *green*) 1)
	(panic "Can't allocate green colorcell"))
       (set! *green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *green-gc*
		       (xcolor-pixel (second *green*)))
       (xsetfont
	*display* *green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *green-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-green*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Green"))
       (unless (= (first *dark-green*) 1)
	(panic "Can't allocate dark green colorcell"))
       (set! *dark-green-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-green-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-green-gc*
		       (xcolor-pixel (second *dark-green*)))
       (xsetfont
	*display* *dark-green-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-green-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *blue*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Blue"))
       (unless (= (first *blue*) 1)
	(panic "Can't allocate blue colorcell"))
       (set! *blue-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *blue-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *blue-gc*
		       (xcolor-pixel (second *blue*)))
       (xsetfont
	*display* *blue-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *blue-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *yellow*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Yellow"))
       (unless (= (first *yellow*) 1)
	(panic "Can't allocate yellow colorcell"))
       (set! *yellow-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *yellow-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *yellow-gc*
		       (xcolor-pixel (second *yellow*)))
       (xsetfont
	*display* *yellow-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *yellow-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *violet*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Violet"))
       (unless (= (first *violet*) 1)
	(panic "Can't allocate violet colorcell"))
       (set! *violet-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *violet-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *violet-gc*
		       (xcolor-pixel (second *violet*)))
       (xsetfont
	*display* *violet-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *violet-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *orange*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Orange"))
       (unless (= (first *orange*) 1)
	(panic "Can't allocate orange colorcell"))
       (set! *orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *orange-gc*
		       (xcolor-pixel (second *orange*)))
       (xsetfont
	*display* *orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *orange-gc* 0 LINESOLID CAPROUND JOINROUND)
       (set! *dark-orange*
	     (xallocnamedcolor3 *display*
			       (xdefaultcolormap *display* *screen*)
			       "Dark Orange"))
       (unless (= (first *dark-orange*) 1)
	(panic "Can't allocate dark orange colorcell"))
       (set! *dark-orange-gc*
	     (xcreategc *display* *window* 0 (make-xgcvalues)))
       (xsetbackground *display* *dark-orange-gc*
		       (xcolor-pixel (second *background*)))
       (xsetforeground *display* *dark-orange-gc*
		       (xcolor-pixel (second *dark-orange*)))
       (xsetfont
	*display* *dark-orange-gc* (xfontstruct-fid *roman-font*))
       (xsetlineattributes
	*display* *dark-orange-gc* 0 LINESOLID CAPROUND JOINROUND))
      (set! *color-gc*
	    (xcreategc *display* *window* 0 (make-xgcvalues)))
      (xsetbackground *display* *color-gc*
		      (if stalin?
			  *white-pixel*
			  (xcolor-pixel (second *background*))))
      (xsetforeground *display* *color-gc*
		      (if stalin?
			  *black-pixel*
			  (xcolor-pixel (second *foreground*))))
      (xsetlineattributes
       *display* *color-gc* 0 LINESOLID CAPROUND JOINROUND)
      (set! *window-methods* '())
      (set! *abort-button* #f)
      (set! *abort-key* #f)
      (set! *comtab* (make-vector 256 #f))
      (set! *help* '())
      (define-key (control #\h) "Help" help-command)
      (set! *help* '())
      (define-key (control #\n) "Scroll help window down one line"
       help-scroll-down-line-command)
      (define-key (control #\p) "Scroll help window up one line"
       help-scroll-up-line-command)
      (define-key (control #\v) "Scroll help window down one page"
       help-scroll-down-page-command)
      (define-key (meta #\v) "Scroll help window up one page"
       help-scroll-up-page-command)
      (define-key (meta #\<) "Scroll help window to beginning"
       help-scroll-beginning-command)
      (define-key (meta #\>) "Scroll help window to end"
       help-scroll-end-command)
      (set! *help-comtab* *comtab*)
      (set! *comtab* (make-vector 256 #f))
      (when transcript-lines
       (set! *transcript* '())
       (set! *input* "")
       (set! *input-position* 0)
       (let ((help *help*))
	(for-each
          (lambda (character)
           (define-key character
            "Enter the typed character into the echo pane"
            (lambda () (echo-pane-insert-character-command character))))
	 (append
	  (string->list
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	  (string->list
	   "1234567890-=\\`!@#$%^&*()_+|~[]{};':\",./<>? ")))
	(set! *help* help))
       (define-key (control #\a)
	"Move the cursor to the beginning of the echo pane"
	echo-pane-beginning-of-line-command)
       (define-key (control #\b)
	"Move the cursor backward one character in the echo pane"
	echo-pane-backward-char-command)
       (define-key (control #\d)
	"Delete the character after the cursor in the echo pane"
	echo-pane-delete-char-command)
       (define-key (control #\e)
	"Move the cursor to the end of the echo pane"
	echo-pane-end-of-line-command)
       (define-key (control #\f)
	"Move the cursor forward one character in the echo pane"
	echo-pane-forward-char-command)
       (define-key (control #\k)
	"Delete all characters after the cursor in the echo pane"
	echo-pane-kill-line-command)
       (define-key delete-key
	"Delete the character before the cursor in the echo pane"
	echo-pane-backward-delete-char-command)
       (define-key return-key
	"Process the input in the echo pane"
	(lambda ()
	 (set! *transcript* (cons (list 'user *input*) *transcript*))
	 (listener-procedure)
	 (set! *input* "")
	 (set! *input-position* 0)
	 (redraw-transcript-pane)
	 (redraw-echo-pane)))
       (define-key (meta #\b)
	"Move the cursor backward one word in the echo pane"
	echo-pane-backward-word-command)
       (define-key (meta #\d)
	"Delete the word after the cursor in the echo pane"
	echo-pane-kill-word-command)
       (define-key (meta #\f)
	"Move the cursor forward one word in the echo pane"
	echo-pane-forward-word-command)
       (define-key (meta delete-key)
	"Delete the word before the cursor in the echo pane"
	echo-pane-backward-kill-word-command))
      (set! *prefix* '())
      (set! *status* "Tyi")
      (set! *message* "")
      (set! *redraw-procedure* redraw-procedure)
      (set! *buttons* '())
      (set! *pause?* #f)
      (set! *help?* #f)
      (set! *clear-display-pane?* #t)
      (let ((hints (make-xwmhints)))
       (set-xwmhints-input! hints 1)
       (set-xwmhints-flags! hints INPUTHINT)
       (xsetwmhints *display* *window* hints))
      (let ((hints (make-xsizehints))
	    (height (if transcript-lines
			(if (zero? transcript-lines)
			    (+ (* button-rows (+ *button-height* 4))
			       *display-pane-height*
			       *echo-pane-height*
			       *who-line-height*
			       14)
			    (+ (* button-rows (+ *button-height* 4))
			       *display-pane-height*
			       *transcript-pane-height*
			       *echo-pane-height*
			       *who-line-height*
			       18))
			(+ (* button-rows (+ *button-height* 4))
			   *display-pane-height*
			   *who-line-height*
			   10))))
       (when *window-position?*
	(set-xsizehints-x! hints *window-position-x*)
	(set-xsizehints-y! hints *window-position-y*))
       (set-xsizehints-min_width! hints width)
       (set-xsizehints-max_width! hints width)
       (set-xsizehints-min_height! hints height)
       (set-xsizehints-max_height! hints height)
       (set-xsizehints-flags! hints
			  (if *window-position?*
			      (+ USPOSITION PPOSITION PMINSIZE PMAXSIZE)
			      (+ PMINSIZE PMAXSIZE)))
       (xsetwmnormalhints *display* *window* hints))
      (pre-initialize-procedure)
      (set-window-method! *display-pane* 'expose redraw-display-pane)
      (set-window-method! *display-pane* 'BUTTONPRESS region-handler)
      (when *transcript-pane*
       (set-window-method!
	*transcript-pane* 'expose redraw-transcript-pane))
      (when *echo-pane*
       (set-window-method! *echo-pane* 'expose redraw-echo-pane))
      (set-window-method! *status-pane* 'expose redraw-status-pane)
      (set-window-method! *message-pane* 'expose redraw-message-pane)
      (set-kill-application!
       (lambda ()
	(set-kill-application! (lambda () #t))
	(finalize-procedure)
	(when *display*
	 (xfreegc *display* *thin-gc*)
	 (xfreegc *display* *thin-flipping-gc*)
	 (xfreegc *display* *medium-gc*)
	 (xfreegc *display* *medium-flipping-gc*)
	 (xfreegc *display* *thick-gc*)
	 (xfreegc *display* *thick-flipping-gc*)
	 (xfreegc *display* *dashed-gc*)
	 (xfreegc *display* *dashed-flipping-gc*)
	 (xfreegc *display* *roman-gc*)
	 (xfreegc *display* *bold-gc*)
	 (xfreegc *display* *bold-flipping-gc*)
	 (unless stalin?
	  (xfreegc *display* *light-gray-gc*)
	  (xfreegc *display* *gray-gc*)
	  (xfreegc *display* *red-gc*)
	  (xfreegc *display* *dark-red-gc*)
	  (xfreegc *display* *green-gc*)
	  (xfreegc *display* *dark-green-gc*)
	  (xfreegc *display* *blue-gc*)
	  (xfreegc *display* *yellow-gc*)
	  (xfreegc *display* *violet-gc*)
	  (xfreegc *display* *orange-gc*)
	  (xfreegc *display* *dark-orange-gc*)
	  (xfreegc *display* *color-gc*)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *background*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *foreground*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *light-gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *gray*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-red*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-green*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *blue*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *yellow*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *violet*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *orange*))))
		       1
		       0)
	  (xfreecolors *display*
		       (xdefaultcolormap *display* *screen*)
		       (unsigned-list->unsigneda
			(list (xcolor-pixel (second *dark-orange*))))
		       1
		       0))
	 (xunloadfont *display* (xfontstruct-fid *roman-font*))
	 (xunloadfont *display* (xfontstruct-fid *bold-font*))
	 (xdestroywindow *display* *window*)
	 (xclosedisplay *display*)
	 (set! *display* #f))
	#t))
      (xmapsubwindows *display* *window*)
      (xmapraised *display* *window*)
      (process-events)
      (kill-application))))))
)
