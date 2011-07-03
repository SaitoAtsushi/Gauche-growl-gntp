(add-load-path "../src")
(use growl.gntp)
(define growl-gntp (make <growl-gntp> :app-name "FOOBAR"))
(define growl-notification (make <growl-notification>
                             :name "hoge"
                             :display-name "Balloon"
                             :enabled "True"))
(define growl-notify1 (make <growl-notify>
                        :app-name "FOOBAR"
                        :event-name "hoge"
                        :title "hello, world"
                        :text "Hello, world!!"))
(define growl-notify2 (make <growl-notify>
                         :app-name "FOOBAR"
                         :event-name "hoge"
                         :title "sample"
                         :text "こんにちは！こんにちは！"))
(register growl-gntp growl-notification)
(notify growl-gntp growl-notify1)
(notify growl-gntp growl-notify2)
