(defun rails-snip-ra () (interactive)
  (snippet-insert "render :action => \"$${action}\""))

(defun rails-snip-ral () (interactive)
  (snippet-insert "render :action => \"$${action}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rf () (interactive)
  (snippet-insert "render :file => \"$${filepath}\""))

(defun rails-snip-rfu () (interactive)
  (snippet-insert
   "render :file => \"$${filepath}\", :use_full_path => $${false}"))

(defun rails-snip-ri () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\""))

(defun rails-snip-ril () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"))

(defun rails-snip-rit () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"))

(defun rails-snip-rl () (interactive)
  (snippet-insert
   "render :layout => \"$${layoutname}\""))

(defun rails-snip-rn () (interactive)
  (snippet-insert
   "render :nothing => $${true}"))

(defun rails-snip-rns () (interactive)
  (snippet-insert
   "render :nothing => $${true}, :status => $${401}" ))

(defun rails-snip-rp () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\""))

(defun rails-snip-rpc () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :collection => $${items}"))

(defun rails-snip-rpl () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"))

(defun rails-snip-rpo () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :object => $${object}"))

(defun rails-snip-rps () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :status => $${500}"))

(defun rails-snip-rt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\""))

(defun rails-snip-rtl () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rtlt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => $${true}"))

(defun rails-snip-rts () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :status => $${401}"))

(defun rails-snip-rcea () (interactive)
  (snippet-insert
   "render_component :action => \"$${index}\""))

(defun rails-snip-rcec () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\""))

(defun rails-snip-rceca () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\", :action => \"$${index}\""))

(defun rails-snip-rea () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${index}\""))

(defun rails-snip-reai () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-rec () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\""))

(defun rails-snip-reca () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${list}\""))

(defun rails-snip-recai () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-flash () (interactive)
  (snippet-insert
   "flash[:$${notice}] = \"$${Text here...}\""))

(defun rails-snip-logi () (interactive)
  (snippet-insert
   "logger.info \"$${Text here...}\""))

(defun rails-snip-params () (interactive)
  (snippet-insert
   "params[:$${id}]"))

(defun rails-snip-session () (interactive)
  (snippet-insert
   "session[:$${user}]"))

(defun rails-snip-ar-belongs_to () (interactive)
  (snippet-insert
   "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""))

(defun rails-snip-ar-has_many () (interactive)
  (snippet-insert
   "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-has_one () (interactive)
  (snippet-insert
   "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-val_pres () (interactive)
  (snippet-insert
   "validates_presence_of :$${attr}"))

(defun rails-snip-ar-val_uniq () (interactive)
  (snippet-insert
   "validates_uniqueness_of :$${attr}"))

(defun rails-snip-ar-val_num () (interactive)
  (snippet-insert
   "validates_numericality_of :$${attr}"))

(defun rails-snip-erb-ft () (interactive)
  (snippet-insert
   "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"))

(defun rails-snip-erb-lia () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-liai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-lic () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"))

(defun rails-snip-erb-lica () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-licai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-if () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-ifel () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"))

(defun rails-snip-erb-unless () (interactive)
  (snippet-insert "<% unless $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-block () (interactive)
  (snippet-insert "<% $. -%>"))

(defun rails-snip-erb-echo-block () (interactive)
  (snippet-insert "<%= $. %>"))
