;; The reason we can't put this to the other plugins is, that the
;; dependencies on drakma and closure-html should'nt be required if
;; one doesn't want to use http plugins at all.

(defsystem lispbot-http-plugins
  :name "lispbot-http-plugins"
  :version "0.1"
  :maintainer "Hans-Peter Deifel"
  :description "Plugins for the lispbot that use HTTP"
  :components ((:file "package")
	       (:file "google-plugin"
		      :depends-on ("package")))
  :depends-on (:lispbot :drakma :closure-html :cxml-stp))