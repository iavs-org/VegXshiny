#' newMethodTemplate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_newMethodTemplate_ui <- function(id){
  ns <- NS(id)
  showModal(
    modalDialog(
      size = "l",
      tagList(
        h3("Method"),
        tags$p("Describe the measurement method.", class = "text-info annotation"),
        radioButtons(ns("method_scale"), label = "Scale of measurement", choices = c("quantitative", "ordinal", "qualitative"), inline = T),
        textInput(ns("method_name"), label = "Name *", width = "100%"),
        textAreaInput(ns("method_description"), label = "Description *", width = "100%"),
        textInput(ns("method_citation"), label = "Citation", width = "100%"),
        hr(),
        
        h3("Attributes"),   
        tags$p("Define the measured properties of the method.", class = "text-info annotation"),
        br(),
        
        uiOutput(ns("quant_attrs_ui")),
        uiOutput(ns("ord_attrs_ui")),
        uiOutput(ns("qual_attrs_ui"))
      ), 
      footer = tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                         actionButton(ns("add_custom_method"), class = "pull-right btn-success", "Add method", icon("check")))
    )
  )
}

#' newMethodTemplate Server Functions
#'
#' @noRd 
mod_newMethodTemplate_server <- function(id, subject, templates, templates_lookup){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$quant_attrs = data.frame("Unit *" = character(1), "Precision" = numeric(1), "Lower limit" = numeric(1), "Upper limit" = numeric(1), check.names = F) %>% 
      na_if(0) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_context_menu(allowRowEdit = FALSE) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$quant_attrs_ui = renderUI({
      if(input$method_scale == "quantitative"){
        tagList(
          tags$label("Quantitative attributes"),
          br(),
          tags$p("Quantitative methods (e.g. elevation above sea level) have only one attribute that defines the continuous scale used for measurement.", class = "text-info annotation"),
          rhandsontable::rHandsontableOutput(ns("quant_attrs"))
        )
      }
    })
    
    output$ord_attrs = data.frame("Code *" = character(1), "Definition" = character(1), "Lower limit" = numeric(1), "Upper limit" = numeric(1), "Order" = integer(1), check.names = F) %>% 
      na_if(0) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$ord_attrs_ui = renderUI({
      if(input$method_scale == "ordinal"){
        tagList(
          tags$label("Ordinal attributes"),
          br(),
          tags$p("Ordinal methods (e.g. Braun-Blanquet cover classification) have multiple attributes describing distinct measurement levels on an ordinal scale. You can add new rows for additional attributes by right-clicking in the table.", class = "text-info annotation"),
          rhandsontable::rHandsontableOutput(ns("ord_attrs"))
        )
      }
    })
    
    output$qual_attrs = data.frame("Code *" = character(1), "Description" = character(1), check.names = F) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$qual_attrs_ui = renderUI({
      if(input$method_scale == "qualitative"){
        tagList(
          tags$label("Qualitative attributes"),
          br(),
          tags$p("Qualitative methods (e.g. rock type) have multiple attributes describing distinct measurement levels on a categorical scale. You can add new rows for additional attributes by right-clicking in the table.", class = "text-info annotation"),
          rhandsontable::rHandsontableOutput(ns("qual_attrs"))
        )
      }
    })
    
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
          if(!isTruthy(input$method_name) | !isTruthy(input$method_description)){stop("Please fill all mandatory fields")}
          template_id = max(templates_lookup()$template_id) + 1
          
          # Create new lookup record
          new_lookup = data.frame("template_id" = template_id, 
                                  "target_element" = "methods",
                                  "subject" = subject,
                                  "name" = input$method_name,
                                  "description" = input$method_description,
                                  "user_defined" = 1) 
          
          # Create new template
          new_node_id = id_generator()
          method = data.frame("template_id" = template_id,
                              "node_id" = new_node_id(),
                              "main_element" = "methods",
                              "node_path" = c("method > subject", "method > name", "method > description", "method > choice > citationString"),
                              "node_value" = c(subject, input$method_name, input$method_description, input$method_citation))
          
          attributes = list()
          if(input$method_scale == "quantitative" & isTruthy(input$quant_attrs)){
            quant_attr_df = rhandsontable::hot_to_r(input$quant_attrs) %>% 
              dplyr::filter(isTruthy(.[[1]]))
            if(nrow(quant_attr_df) != 0){
              attributes$quant_attr = lapply(1:nrow(quant_attr_df), function(i){
                data.frame("template_id" = template_id,
                           "node_id" = new_node_id(),
                           "main_element" = "attributes",
                           "node_path" = c("attribute > choice > quantitative > unit", "attribute > choice > quantitative > precision", "attribute > choice > quantitative > lowerLimit", "attribute > choice > quantitative > upperLimit", "attribute > choice > quantitative > methodID"),
                           "node_value" = c(quant_attr_df[i,1] , quant_attr_df[i,2], quant_attr_df[i,3], quant_attr_df[i,4], 1))
              }) %>% bind_rows()
            }
          }
          
          if(input$method_scale == "ordinal" & isTruthy(input$ord_attrs)){
            ord_attr_df = rhandsontable::hot_to_r(input$ord_attrs) %>% 
              dplyr::filter(isTruthy(.[[1]]))
            if(nrow(ord_attr_df) != 0){
              attributes$ord_attr = lapply(1:nrow(ord_attr_df), function(i){
                data.frame("template_id" = template_id,
                           "node_id" = new_node_id(),
                           "main_element" = "attributes",
                           "node_path" = c("attribute > choice > ordinal > code", "attribute > choice > ordinal > definition", "attribute > choice > ordinal > lowerLimit", "attribute > choice > ordinal > upperLimit", "attribute > choice > ordinal > order", "attribute > choice > ordinal > methodID"),
                           "node_value" = c(ord_attr_df[i,1] , ord_attr_df[i,2], ord_attr_df[i,3], ord_attr_df[i,4], ord_attr_df[i,5], 1))
              }) %>% bind_rows()
            }
          }
          
          if(input$method_scale == "qualitative" & isTruthy(input$qual_attrs)){
            qual_attr_df = rhandsontable::hot_to_r(input$qual_attrs) %>% 
              dplyr::filter(isTruthy(.[[1]]))
            if(nrow(qual_attr_df) != 0){
              attributes$qual_attr = lapply(1:nrow(qual_attr_df), function(i){
                data.frame("template_id" = template_id,
                           "node_id" = new_node_id(),
                           "main_element" = "attributes",
                           "node_path" = c("attribute > choice > qualitative > code", "attribute > choice > qualitative > description", "attribute > choice > qualitative > methodID"),
                           "node_value" = c(qual_attr_df[i,1] , qual_attr_df[i,2], 1))
              }) %>% bind_rows()
            }
          }
          
          if(length(attributes) == 0 || nrow(bind_rows(attributes)) == 0){
            stop("Method has no attributes")
          }
  
          # rowbind template and drop NAs
          new_template = method %>% 
            bind_rows(bind_rows(attributes)) %>% 
            mutate(node_value = na_if(node_value, "")) %>% 
            drop_na()

          # Update template reactiveVals
          templates(bind_rows(templates(), new_template))
          templates_lookup(bind_rows(templates_lookup(), new_lookup))
          
          shiny::showNotification("Method template added", type = "message")
          removeModal()
        }, error = function(e){
          shiny::showNotification(e$message, type = "error")
        })
      }
    )
  })
}
