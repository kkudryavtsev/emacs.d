;;; dka-ruby-snippets.el

;; Uses snippet.el to set up auto-expanded abbreviations
;; Thanks to TextMate, Pete Kazmier, and Galinsky Dmitry

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
    ;; model
    ("bt" . "belongs_to :$${class}")
    ("hm" . "has_many :$${class}")
    ("ho" . "has_one :$${class}")
		("habtm" . "has_and_belongs_to_many :$${class}")
    ;; controller renders
    ("ra" . "render :action => \"$${action}\"")
    ("ral" . "render :action => \"$${action}\", :layout => \"$${layoutname}\"")
    ("rf" . "render :file => \"$${filepath}\"")
    ("rfu" . "render :file => \"$${filepath}\", :use_full_path => $${false}")
    ("ri" . "render :inline => \"$${<%= 'hello' %>}\"")
    ("ril" . "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }")
    ("rit" . "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})")
    ("rl" . "render :layout => \"$${layoutname}\"")
    ("rn" . "render :nothing => $${true}")
    ("rns" . "render :nothing => $${true}, :status => $${401}")
    ("rp" . "render :partial => \"$${item}\"")
    ("rpc" . "render :partial => \"$${item}\", :collection => $${items}")
    ("rpl" . "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}")
    ("rpo" . "render :partial => \"$${item}\", :object => $${object}")
    ("rps" . "render :partial => \"$${item}\", :status => $${500}")
    ("rt" . "render :text => \"$${Text here...}\"")
    ("rtl" . "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\"")
    ("rtlt" . "render :text => \"$${Text here...}\", :layout => $${true}")
    ("rts" . "render :text => \"$${Text here...}\", :status => $${401}")
    ("rcea" . "render_component :action => \"$${index}\"")
    ("rcec" . "render_component :controller => \"$${items}\"")
    ("rceca" . "render_component :controller => \"$${items}\", :action => \"$${index}\"")
    ;; redirects
    ("rea" . "redirect_to :action => \"$${index}\"")
    ("reai" . "redirect_to :action => \"$${show}\", :id => $${@item}")
    ("rec" . "redirect_to :controller => \"$${items}\"")
    ("reca" . "redirect_to :controller => \"$${items}\", :action => \"$${list}\"")
    ("recai" . "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}")
		;; rspec
		("de" . "describe $${object} do\n$.\nend")
		("it" . "it \"should $${description}\" do\n$.\nend")
    ;; assertions
    ("ae" . "assert_equal $${expected}, $${actual}")
    ("ann" . "assert_not_nil $${object}")
    ("ako" . "assert_kind_of $${class}, $${object}")
    ("ars" . "assert_response :$${success}")
    ("ar" . "assert_raises($${Exception}) { $. }")
    ("at" . "assert_template \"$${file}\"")
    ("art" . "assert_redirected_to :controller => \"$${controller}\", :action => \"$${index}\"")
		("artn" . "assert_redirected_to $${named_route}_url")
    ("asa" . "assert assigns(:$${object})")
		("av" . "assert $${object}.valid?, $${object}.errors.full_messages.join(\"\\n\")")
    ;; validations
    ("va" . "validates_associated :$${attr}")
    ("vc" . "validates_confirmation_of :$${attr}")
    ("ve" . "validates_exclusion_of :$${attr}")
    ("vp" . "validates_presence_of :$${attr}")
    ("vu" . "validates_uniqueness_of :$${attr}")
    ("vn" . "validates_numericality_of :$${attr}")
    ("vf" . "validates_format_of :$${attr}, :with => /$${regex}/")
		("vl" . "validates_length_of :$${attr}")
		;; migrations
		("mccc" . ">t.column :$${title}, :$${string}\nmccc$.")
		("mct" . ">create_table :$${tablename} do |t|\n>$.\n>end")
		("mdt" . ">drop_table :$${tablename}")
    ;; misc
    ("flsh" . "flash[:$${notice}] = \"$${Text here...}\"")
    ("logi" . "logger.info \"$${Text here...}\"")
    ("par" . "params[:$${id}]")
    ("ses" . "session[:$${user}]")

    ;; ruby-stuff
    ("def" . ">def $${method}\n> $.\n>end\n")
    ("if" . ">if $${condition}\n> $.\n>end")
		("ei" . ">elsif $${condition}\n>$.")
    ("ife" . ">if $${condition}\n> $$\n>else\n> $.\n>end")
    ("ifei" . ">if $${condition}\n> $$ \n>elsif $${condition2}\n> $.\n>end"))

;; (snippet-with-abbrev-table 'rhtml-mode-abbrev-table
;;     ;; view
;;     ("%ft" . "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>")
;;     ("%li" . "<%= link_to \"$${title}\", $${something}_url %>")
;;     ("%lia" . "<%= link_to \"$${title}\", :action => \"$${index}\" %>")
;;     ("%liai" . "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>")
;;     ("%lic" . "<%= link_to \"$${title}\", :controller => \"$${items}\" %>")
;;     ("%lica" . "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>")
;;     ("%licai" . "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>")
;;     ("%rp" . "<%= render :partial => \"$${item}\" %>")
;;     ("%rpc" . "<%= render :partial => \"$${item}\", :collection => $${items} %>")
;;     ("%rpl" . "<%= render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"} %>")
;;     ("%rpo" . "<%= render :partial => \"$${item}\", :object => $${object} %>")
;;     ("%rps" . "<%= render :partial => \"$${item}\", :status => $${500} %>")

;;     ("%h" . "<%=h $${@item} %>")
;;     ("%if" . "<% if $${cond} -%>\n$.\n<% end -%>")
;;     ("%ifel" . "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>")
;;     ("%unless" . "<% unless $${cond} -%>\n$.\n<% end -%>")
;;     ("%" . "<%$. -%>")
;;     ("%%" . "<%=$. %>"))

