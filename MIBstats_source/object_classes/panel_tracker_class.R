# _This class is used to keep track of the states of user interface elements

panel_tracker <- setClass("panel_tracker", slots = c(panel_tracking = "list"),
prototype=list(
  panel_tracking = list()
))

setGeneric(name="update_toggle_button_tracker",
           def=function(theObject, switch_name_f, button_name_f, forced_value_f)
           {
             standardGeneric("update_toggle_button_tracker")
           }
)

setMethod(f="update_toggle_button_tracker",
          signature="panel_tracker",
          definition=function(theObject, switch_name_f, button_name_f, forced_value_f)
          {
            temp_list <- slot(theObject, "panel_tracking")
            
            if(forced_value_f == "FALSE") {
              temp_list[[switch_name_f]] <- FALSE
            } else {
              temp_list[[switch_name_f]] <- !temp_list[[switch_name_f]]
            }
            theObject@panel_tracking <- temp_list
            return(theObject)
          }
)

setGeneric(name="update_toggle_button_icon",
           def=function(theObject, switch_variable_name_f, button_name_f, forced_value_f)
           {
             standardGeneric("update_toggle_button_icon")
           }
)

setMethod(f="update_toggle_button_icon",
          signature="panel_tracker",
          definition=function(theObject, switch_variable_name_f, button_name_f, forced_value_f)
          {
            temp_list <- theObject@panel_tracking
            if(forced_value_f == "FALSE") {
              updateActionButton(session, button_name_f, icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
            } else {
              toggle_f <- temp_list[[switch_variable_name_f]]
              
              if(toggle_f) {
                updateActionButton(session, button_name_f, icon = icon("minus-square-o", class = NULL, lib = "font-awesome"))
              } else if (!toggle_f) {
                updateActionButton(session, button_name_f, icon = icon("plus-square-o", class = NULL, lib = "font-awesome"))
              }
            }
          }
)

setGeneric(name="update_toggle_button",
           def=function(theObject, switch_name_f, button_name_f)
           {
             standardGeneric("update_toggle_button")
           }
)

setMethod(f="update_toggle_button",
          signature="panel_tracker",
          definition=function(theObject, switch_name_f, button_name_f)
          {
            theObject <- update_toggle_button_tracker(theObject, switch_name_f, button_name_f, "NONE")
            update_toggle_button_icon(theObject, switch_name_f, button_name_f, "NONE")
            return(theObject)
          }
)

setGeneric(name="set_toggle_button_to_false",
           def=function(theObject, switch_name_f, button_name_f)
           {
             standardGeneric("set_toggle_button_to_false")
           }
)

setMethod(f="set_toggle_button_to_false",
          signature="panel_tracker",
          definition=function(theObject, switch_name_f, button_name_f)
          {
            theObject <- update_toggle_button_tracker(theObject, switch_name_f, button_name_f, "FALSE")
            update_toggle_button_icon(theObject, switch_name_f, button_name_f, "FALSE")
            return(theObject)
          }
)
