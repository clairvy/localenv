(require 'twittering-mode)
(setq twittering-username "mugijiru")
(setq twittering-password "omakedisk")
(setq twittering-proxy-server "0.0.0.0")

(expectations
   (desc "twittering tmp dir")
   (expect "/tmp/twmode-images-mugijiru"
	   (twittering-tmp-dir-name));;

   (desc "get status url")
   (expect "http://twitter.com/tarou/statuses/57"
	   (twittering-get-status-url "tarou" 57))


   (desc "check proxy")
   (expect nil
	   (twittering-check-use-proxy))
   (expect t
	   (setq twittering-proxy-use t)
	   (twittering-check-use-proxy))


   (desc "set server and port")
   (expect "twitter.com"
	   (setq twittering-proxy-use nil)
	   (twittering-set-server))
   (expect "80"
	   (setq twittering-proxy-use nil)
	   (twittering-set-port))


   (expect "0.0.0.0"
	   (setq twittering-proxy-use t)
	   (twittering-set-server))
   (expect "8080"
	   (setq twittering-proxy-use t)
	   (twittering-set-port))

   (expect "8080"
	   (twittering-proxy-port-string))


   (desc "set url for @mugijiru")
   (expect "http://twitter.com/mugijiru"
	   (twittering-set-url "@\\([_a-zA-Z0-9]+\\)" "mugijiru"))

   (desc "set url for 'http://www.google.co.jp/'")
   (expect "http://www.google.co.jp/"
	   (twittering-set-url "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)" "http://www.google.co.jp/"))


)
