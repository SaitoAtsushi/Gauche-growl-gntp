(define-module growl.gntp
  (use math.mt-random)
  (use rfc.md5)
  (use rfc.sha)
  (use srfi-13)
  (use srfi-14)
  (use gauche.net)
  ;;(use rfc.uuid)
  (export <growl-gntp>
          <growl-notification>
          <growl-notify>
          <growl-subscriber>
          register
          notify
          subscribe))

(select-module growl.gntp)

(define *growl-gntp-default-port* 23053)

(define-macro (define-growl-class class-name meta-class slots)
  `(define-class ,class-name ,meta-class
     ,(map (lambda (slot)
              (let ((slot-name (car slot))
                    (init-value (cadr slot)))
                `(,slot-name :init-keyword ,(make-keyword (symbol->string slot-name))
                             :init-value ,init-value
                             :accessor ,(string->symbol
                                         (string-append
                                          (symbol->string slot-name)
                                          "-of")))))
           slots)))

;;;; class
(define-growl-class <growl-header-field> ()
  ((field-name "")
   (field-value "")))

(define-growl-class <growl-gntp> ()
  ((host "localhost")
   (port *growl-gntp-default-port*)
   (app-name "Gauche::Growl::GNTP")
   (app-icon "")
   (password "")
   (password-hash-algorithm "MD5")
   (encrypt-algorithm "NONE")
   (debug #f)))

(define-growl-class <growl-notification> ()
  ((name (string-append "Growl::GNTP::Notification" (symbol->string (gensym))))
   (display-name (string-append "Growl::GNTP::Notification" (symbol->string (gensym))))
   (enabled "True")
   (sticky "False")
   (priority 0)
   (icon "")))

(define-growl-class <growl-notify> ()
  ((app-name "")
   (event-name "")
   (title "")
   (id "")
   (priority 0)
   (sticky "False")
   (text "")
   (icon "")
   (callback-context "")
   (callback-context-type "")
   (callback-target "")))

(define-growl-class <growl-subscriber> ()
  ((id (gensym)) ; (guid)
   (name (sys-gethostname))
   (port *growl-gntp-default-port*)
   (password "")
   (callback (lambda (title text)))))

;; helper functions
(define (empty-string? str)
  (equal? str ""))

(define (crlf->cr str)
  (regexp-replace-all #/\r\n/ str "\n"))

(define (prepend-crlf str)
  (string-append "\r\n" str))

(define (append-crlf str)
  (string-append str "\r\n"))

(define (generate-hashed-string string hash-algorithm)
  (cond
   ((equal? hash-algorithm "MD5") (md5-digest-string string))
   ((equal? hash-algorithm "SHA1") (sha1-digest-string string))
   ((equal? hash-algorithm "SHA256") (sha256-digest-string string))
   ((equal? hash-algorithm "SHA512") (sha512-digest-string string))
   (else (error "no such hash algorithm."))))

(define (generate-salt)
  (let* ((count 10)
         (charset "./0-9abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (size (string-length charset))
         (mt (make <mersenne-twister> :seed (sys-time))))
    (define (iter acc)
      (if (equal? (length acc) count)
          acc
          (iter (cons (string-ref charset (mt-random-integer mt size)) acc))))
    (list->string (iter '()))))

(define (generate-keyhash key salt hash-algorithm)
  (digest-hexify (generate-hashed-string (string-append key salt) hash-algorithm)))

;; TODO
(define (generate-encryption-part growl-gntp)
  "NONE")

(define (generate-keyhash-part growl-gntp)
  (let ((salt (generate-salt))
        (hash-algorithm (password-hash-algorithm-of growl-gntp))
        (password (password-of growl-gntp))
        (encrypt-algorithm (encrypt-algorithm-of growl-gntp)))
    (cond
     ((and (equal? (string-length password) 0)
           (equal? encrypt-algorithm "NONE")) "")
     (else
      #`" ,hash-algorithm:,(generate-keyhash password salt hash-algorithm).,(digest-hexify salt)"))))

(define (growl-header-field->header-field growl-header-field)
  (crlf->cr (string-append (field-name-of growl-header-field)
                           ": "
                           (if (string? (field-value-of growl-header-field))
                               (field-value-of growl-header-field)
                               (x->string (field-value-of growl-header-field))))))

(define (growl-header-fields->header growl-header-fields)
  (string-concatenate
   (map (lambda (growl-header)
          (if (empty-string? (field-value-of growl-header))
              ""
              (append-crlf (growl-header-field->header-field growl-header))))
        growl-header-fields)))

(define (header-fields->growl-header-fields header-fields)
  (map (lambda (header-field)
         (make <growl-header-field>
           :field-name (car header-field)
           :field-value (cadr header-field)))
       header-fields))

(define (growl-header-fields->header-fields growl-header-fields)
  (map (lambda (growl-header-field)
         `(,(field-name-of growl-header-field)
           ,(field-value-of growl-header-field)))
       growl-header-fields))

;;;; header generator functions
(define (generate-information-header message-type encryption-part keyhash-part)
  (append-crlf
   (string-join
    `("GNTP/1.0"
      ,message-type
      ,(string-append encryption-part keyhash-part))
    " ")))

(define (generate-register-header app-name app-icon notifications-count)
  (let ((growl-header-fields (header-fields->growl-header-fields
                              `(("Application-Name" ,app-name)
                                ("Application-Icon" ,app-icon)
                                ("Notifications-Count" ,notifications-count)))))
    (growl-header-fields->header growl-header-fields)))

(define (generate-notification-header notifications)
  (let iterate ((notifications notifications)
             (headers '()))
    (let* ((notification (car notifications))
           (growl-header-fields (header-fields->growl-header-fields
                                 `(("Notification-Name" ,(name-of notification))
                                   ("Notification-Display-Name" ,(display-name-of notification))
                                   ("Notification-Enabled" ,(enabled-of notification))
                                   ("Notification-Sticky" ,(sticky-of notification))
                                   ("Notification-Priority" ,(priority-of notification))
                                   ("Notification-Icon" ,(icon-of notification))))))
      (if (null? (cdr notifications))
          (prepend-crlf
           (string-join
            (cons (growl-header-fields->header growl-header-fields)
                  headers)
            "\r\n"))
          (iterate (cdr notifications)
                   (cons (growl-header-fields->header growl-header-fields)
                         headers))))))

(define (generate-notify-header notify)
  (define (%append! ls . appended)
    (set! ls (append ls appended)))
  (let ((growl-header-fields (header-fields->growl-header-fields
                              `(("Application-Name" ,(app-name-of notify))
                                ("Notification-Name" ,(event-name-of notify))
                                ("Notification-Title" ,(title-of notify))
                                ("Notification-ID" ,(id-of notify))
                                ("Notification-Priority" ,(priority-of notify))
                                ("Notification-Text" ,(text-of notify))
                                ("Notification-Sticky" ,(sticky-of notify))
                                ("Notification-Icon" ,(icon-of notify)))))
        (callback-context-header-field
         (make <growl-header-field>
           :field-name "Notification-Callback-Context"
           :field-value (callback-context-of notify)))
        (callback-context-type-header-field
         (make <growl-header-field>
           :field-name "Notification-Callback-Context-Type"
           :field-value (callback-context-type-of notify)))
        (callback-context-target-header-field
         (make <growl-header-field>
           :field-name "Notification-Callback-Context-Target"
           :field-value (callback-target-of notify)))
        (callback-target-header-field
         (make <growl-header-field>
           :field-name "Notification-Callback-Target"
           :field-value (callback-target-of notify))))
    (when (not (empty-string? (callback-context-of notify)))
      (%append! growl-header-fields
                callback-context-header-field
                callback-context-type-header-field))
    (when (not (empty-string? (callback-target-of notify)))
      (%append! growl-header-fields
                callback-context-target-header-field
                callback-target-header-field))
    (growl-header-fields->header growl-header-fields)))

(define (generate-subscriber-header subscriber)
  (let ((growl-header-fields (header-fields->growl-header-fields
                               `(("Subscriber-ID" ,(id-of subscriber))
                                 ("Subscriber-Name" ,(name-of subscriber))
                                 ("Subscriber-Port" ,(port-of subscriber))))))
    (growl-header-fields->header growl-header-fields)))

(define (build-request . args)
  (string-concatenate args))

;;;; Growl GNTP API
(define (register growl-gntp . notifications)
  (let* ((notifications-count (length notifications))
         (information-header (generate-information-header
                              "REGISTER"
                              (generate-encryption-part growl-gntp)
                              (generate-keyhash-part growl-gntp)))
         (register-header (generate-register-header
                           (app-name-of growl-gntp)
                           (app-icon-of growl-gntp)
                           notifications-count))
         (notification-header (generate-notification-header notifications))
         (request (build-request information-header
                                 register-header
                                 notification-header)))
    (when (debug-of growl-gntp)
      (print request))
    (call-with-client-socket (make-client-socket 'inet (host-of growl-gntp) (port-of growl-gntp))
      (lambda (in out)
        (format out request)
        (flush out)
        (let1 response-lines (port->string-list in)
          (if (debug-of growl-gntp)
              (for-each (lambda (line)
                          (format #t "~S~%" line))
                        response-lines)))))))

(define (notify growl-gntp growl-notify)
  (let* ((information-header (generate-information-header
                             "NOTIFY"
                             (generate-encryption-part growl-gntp)
                             (generate-keyhash-part growl-gntp)))
         (notify-header (generate-notify-header growl-notify))
         (request (string-append information-header notify-header)))
    (when (debug-of growl-gntp)
      (print request))
    (call-with-client-socket (make-client-socket 'inet (host-of growl-gntp) (port-of growl-gntp))
      (lambda (in out)
        (format out request)
        (flush out)
        (let1 response-lines (port->string-list in)
          (if (debug-of growl-gntp)
              (for-each (lambda (line)
                          (format #t "~S~%" line))
                        response-lines)))))))

;; TODO
(define (subscribe growl-gntp subscriber)
  (let* ((information-header (generate-information-header
                              "SUBSCRIBE"
                              (generate-encryption-part growl-gntp)
                              (generate-keyhash-part growl-gntp)))
         (subscriber-header (generate-subscriber-header subscriber))
         (request (build-request information-header subscriber-header))
         (client-socket (make-client-socket 'inet (host-of growl-gntp) (port-of growl-gntp)))
         (server-socket (make-server-socket 'inet (port-of subscriber) :reuse-addr? #t :backlog 10)))
    (when (debug-of growl-gntp)
      (print request))
    (call-with-client-socket client-socket
      (lambda (in out)
        (format out request)
        (flush out)
        (rxmatch-case (read-line in)
          (#/^GNTP\/1\.0 -ERROR (.*)$/ (message)
                         (socket-close server-socket)
                         (print message))
          (#/^GNTP\/1\.0 -OK .*$/ ()
                         (let loop ((client (socket-accept server-socket)))
                           (let ((responses (port->string-list (socket-input-port client)))
                                 (args '("" . "")))
                             (for-each
                              (lambda (line)
                                (rx-match-case line
                                               (#/^Notification-Title: (.*)\r\n/ (title)
                                                                       (set-car! args title))
                                               (#/^Notification-Text: (.*)\r\n/ (text)
                                                                      (set-cdr! args text))))
                              responses)
                             (when (not (eqaul? (car args) ""))
                               (callback (car args) (cdr args)))
                             (socket-close client))
                           (loop (socket-accept server-socket)))))))))
