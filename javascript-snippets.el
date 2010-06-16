(require 'snippet)
(define-abbrev-table 'javascript-mode-abbrev-table ())
(snippet-with-abbrev-table
 'javascript-mode-abbrev-table
 ("for" .  "$>for (var $${i}=$${0},$${len}=$${i}.length;$${i}<$${len};++$${i}){\n$>$.\n}$>")
 ("forin" . "$>for (var $${i} in $${var}){\n$>$$;\n}$.$>")
 ("if"  .  "$>if ($${cond}){$>\n$>$.;\n}$>")
 ("ifel" . "$>if ($${cond}){$>\n$>$$;\n} else {$>\n$>$$;\n}$.$>")
 ("wh"  .  "$>while ($${i}){\n$>$.\n}$>")
 ("whinc" . "$>while ($${i}<$${10}){\n$>$.\n$>$${i}++;\n}$>")
 ("trn" . "$${if}?$${then}:$${else}")
 ("var" . "var $${variable} = $${value};")
 ("fun" . "$>function $${name}($${args}){\n$>$.\n}$>")
 ("lambda" . "$>function ($${args}){\n$>$.\n}$>")
 ("df" . "document.forms['$${formname}']")
 ("cl" . "console.log('$${message}');")) ;Firebug logging 

(add-hook 'js2-mode-hook
              (lambda ()
                (abbrev-mode 1)
                ;; This line is not in the documentation of snippet.el, but seems to be
                ;; essential for various modes (not for python-mode though, which serves as
                ;; the example mode in said documentation)
                (setq local-abbrev-table javascript-mode-abbrev-table)))
