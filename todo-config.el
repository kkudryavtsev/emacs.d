;; ToDo mode stuff
(autoload 'todo-show "todo-mode" "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode" "Add TODO item." t)
(autoload 'todo-cp "todo-mode" 
  "Make a diary entry appear only in the current date's diary" t)
(setq todo-prefix "&%%(todo-cp)")
(setq todo-time-string-format nil)
(setq todo-initials "")
