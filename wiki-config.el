(defvar dka-wikis
  '(("lathi" "http://www.lathi.net/twiki-bin/view/Main/" "WebHome")
    ("consult" "http://www.lathi.net/twiki-bin/view/Consult/" "WebHome")
    ("emacs"  "http://www.emacswiki.org/cgi-bin/wiki.pl?" "RecentChanges")
    ("zkat"  "http://twiki.z-kat.com/view/ZKat/" "WebChanges")
    ("zkat8080"  "http://twiki.z-kat.com:8080/view/ZKat/" "WebChanges")
    ("google" "http://www.google.com/search?q=" "")
    ("codev" "http://twiki.org/cgi-bin/view/Codev/" "")
    ("debian" "http://wiki.debian.net/" "")
    ("weather" "http://www.weather.com/weather/local/")
    ("c2" "http://c2.com/cgi/wiki?")
    ("ebay" "http://cgi.ebay.com/ws/eBayISAPI.dll?ViewItem&item=" "")
  "List of frequenty visited urls for expansion using sitename:pagename type syntax"))

(defun dka-make-wiki-url (wiki-topic)
  "Create a url out of a sitename:pagename syntax using the dka-wikis list"
  (let (topic wiki wiki-list)
    (if (string-match "\\(\\w+\\):\\(\\w+\\)" wiki-topic)
	(progn
	  (setq wiki (match-string 1 wiki-topic) 
		topic (match-string 2 wiki-topic)
		wiki-list (assoc wiki dka-wikis))
	  (if (and wiki-list topic)
	      (concat (nth 1 wiki-list) topic)
	    wiki-topic))
      wiki-topic)))

(defun dka-expand-wiki-in-string (string)
  "reaplce any identifiable wiki:PageName in STRING with the appropriate url"
  (if (string-match "\\(\\w+:\\w+\\)" string)
    (replace-in-string string 
		       (match-string 1 string)
		       (dka-make-wiki-url (match-string 1 string)))
    string))


