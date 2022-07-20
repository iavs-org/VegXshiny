#' newStrataDefTemplate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_newStrataDefTemplate_ui <- function(id){
  ns <- NS(id)
  showModal(
    modalDialog(
      size = "l",
      tagList(
        h3("Method"),
        tags$p("Describe the stratum classification method.", class = "text-info annotation"),
        
        textInput(ns("method_name"), label = "Name *", width = "100%"),
        textAreaInput(ns("method_description"), label = "Description *", width = "100%"),
        textInput(ns("method_citation"), label = "Citation", width = "100%"),
        hr(),
        
        h3("Attributes"),   
        tags$p("Define the quantitative measurement scale that was used to delimit different stratum classes.", class = "text-info annotation"),
        rhandsontable::rHandsontableOutput(ns("attrs")),
        
        h3("Strata"),   
        tags$p("Define the stratum classes of your method. You can add new rows for additional strata by right-clicking in the table.", class = "text-info annotation"),
        rhandsontable::rHandsontableOutput(ns("strata"))
      ), 
      footer = tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                         actionButton(ns("add_custom_method"), class = "pull-right btn-success", "Add method", icon("check")))
    )
  )
}

#' newStrataDefTemplate Server Functions
#'
#' @noRd 
mod_newStrataDefTemplate_server <- function(id, subject, templates, templates_lookup){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$attrs = data.frame("Unit *" = character(1), "Precision" = numeric(1), "Lower limit" = numeric(1), "Upper limit" = numeric(1), check.names = F) %>% 
      na_if(0) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_context_menu(allowRowEdit = FALSE) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$strata = data.frame("Name *" = character(1), "Order" = integer(1), "Lower limit" = numeric(1), "Upper limit" = numeric(1), check.names = F) %>% 
      na_if(0) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
  
    observeEvent(
      eventExpr = input$dismiss_modal,
      handlerExpr = {
        removeModal()
      }
    )
    
    observeEvent(
      eventExpr = input$add_custom_method,
      handlerExpr = {
        tryCatch({
          if(!isTruthy(input$method_name) | !isTruthy(input$method_description)){stop("Please fill all mandatory method fields")}
          template_id = max(templates_lookup()$template_id) + 1
          
          # Create new lookup record
          new_lookup = data.frame("template_id" = template_id, 
                                  "target_element" = "strata",
                                  "subject" = "strata definition",
                                  "name" = input$method_name,
                                  "description" = input$method_description,
                                  "user_defined" = 1) 
          
          # Create new template
          new_node_id = id_generator()
          
          # 1. Method
          method_template = data.frame("template_id" = template_id,
                              "node_id" = new_node_id(),
                              "main_element" = "methods",
                              "node_path" = c("method > subject", "method > name", "method > description", "method > choice > citationString"),
                              "node_value" = c(subject, input$method_name, input$method_description, input$method_citation))
          
          # 2. Attributes
          attrs_df = rhandsontable::hot_to_r(input$attrs) %>% 
            dplyr::filter(isTruthy(.[[1]]))
          
          if(nrow(attrs_df) == 0){
            stop("Please fill all mandatory attribute fields")
          }
          attrs_template = lapply(1:nrow(attrs_df), function(i){
            data.frame("template_id" = template_id,
                       "node_id" = new_node_id(),
                       "main_element" = "attributes",
                       "node_path" = c("attribute > choice > quantitative > unit", "attribute > choice > quantitative > precision", "attribute > choice > quantitative > lowerLimit", "attribute > choice > quantitative > upperLimit", "attribute > choice > quantitative > methodID"),
                       "node_value" = c(attrs_df[i,1] , attrs_df[i,2], attrs_df[i,3], attrs_df[i,4], "{1}"))
          }) %>% bind_rows()
      
          # 3. Strata
          strata_df = rhandsontable::hot_to_r(input$strata) %>% 
            dplyr::filter(isTruthy(.[[1]]))
          
          
          if(nrow(strata_df) == 0){
            stop("Please fill all mandatory strata fields")
          }
          strata_template = lapply(1:nrow(strata_df), function(i){
            data.frame("template_id" = template_id,
                       "node_id" = new_node_id(),
                       "main_element" = "strata",
                       "node_path" = c("stratum > stratumName", "stratum > order", "stratum > lowerLimit", "stratum > upperLimit", "stratum > methodID"),
                       "node_value" = c(strata_df[i,1] , strata_df[i,2], strata_df[i,3], strata_df[i,4], "{1}"))
          }) %>% bind_rows()

          # rowbind template and drop NAs
          new_template = bind_rows(method_template, attrs_template, strata_template) %>% 
            mutate(node_value = na_if(node_value, "")) %>% 
            drop_na()
          
          # Update template reactiveVals
          templates(bind_rows(templates(), new_template))
          templates_lookup(bind_rows(templates_lookup(), new_lookup))
          
          shiny::showNotification("Strata definition template added", type = "message")
          removeModal()
        }, error = function(e){
          shiny::showNotification(e$message, type = "error")
        })
      }
    )
  })
}

## To be copied in the UI
# mod_newStrataDefTemplate_ui("newStrataDefTemplate_ui_1")

## To be copied in the server
# mod_newStrataDefTemplate_server("newStrataDefTemplate_ui_1")
