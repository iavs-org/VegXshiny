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
        tags$p("Describe your custom method", class = "text-info annotation"),
        radioButtons(ns("method_scale"), label = "Scale of measurement", choices = c("quantitative", "ordinal", "qualitative", "mixed"), inline = T),
        textInput(ns("method_name"), label = "Name *", width = "100%"),
        textAreaInput(ns("method_description"), label = "Description *", width = "100%"),
        textInput(ns("method_citation"), label = "Citation", width = "100%"),
        hr(),
        
        h3("Attributes"),
        tags$p("Define the measured properties of the method. You can new add rows for additional attributes by right-clicking in the table.", class = "text-info annotation"),
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
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$quant_attrs_ui = renderUI({
      if(input$method_scale %in% c("quantitative", "mixed")){
        tagList(
          tags$label("Quantitative attributes"),
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
      if(input$method_scale %in% c("ordinal", "mixed")){
        tagList(
          tags$label("Ordinal attributes"),
          rhandsontable::rHandsontableOutput(ns("ord_attrs"))
        )
      }
    })
    
    output$qual_attrs = data.frame("Code *" = character(1), "Description" = character(1), check.names = F) %>% 
      rhandsontable::rhandsontable(useTypes = T, readOnly = F, rowHeaders = NULL) %>% 
      rhandsontable::hot_cols(colWidths = "120 px") %>% 
      rhandsontable::renderRHandsontable()
    
    output$qual_attrs_ui = renderUI({
      if(input$method_scale %in% c("qualitative", "mixed")){
        tagList(
          tags$label("Qualitative attributes"),
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
          req(input$method_name, input$method_description)
          template_id = max(templates_lookup()$template_id) + 1
          
          # Create new lookup record
          new_lookup = data.frame("template_id" = template_id, 
                                  "target_element" = "methods",
                                  "subject" = subject,
                                  "name" = input$method_name,
                                  "description" = input$method_description,
                                  "user_defined" = 1) %>% 
            tidyr::drop_na()
          
          # Create new template
          new_node_id = id_generator()
          method_template = data.frame("template_id" = template_id,
                                       "node_id" = new_node_id(),
                                       "main_element" = "methods",
                                       "node_path" = c("method > subject", "method > name", "method > description", " method > choice > citationString"),
                                       "node_value" = c(subject, input$method_name, input$method_description, input$method_citation))
          
          quant_attr_df = rhandsontable::hot_to_r(input$quant_attrs)
          if(nrow(quant_attr_df) == 0){
            quant_attr_template = tibble()
          } else {
            quant_attr_template = lapply(1:nrow(quant_attr_df), function(i){
              if(!isTruthy(quant_attr_df[i,1])){return(NULL)}
              data.frame("template_id" = template_id,
                         "node_id" = new_node_id(),
                         "main_element" = "attributes",
                         "node_path" = c("attribute > choice > quantitative > unit", "attribute > choice > quantitative > precision", "attribute > choice > quantitative > lowerLimit", "attribute > choice > quantitative > upperLimit", "attribute > choice > quantitative > methodID"),
                         "node_value" = c(quant_attr_df[i,1] , quant_attr_df[i,2], quant_attr_df[i,3], quant_attr_df[i,4], 1))
            }) %>% bind_rows()
          }
          
          ord_attr_df = rhandsontable::hot_to_r(input$ord_attrs)
          if(nrow(ord_attr_df) == 0){
            ord_attr_template = tibble()
          } else {
            ord_attr_template = lapply(1:nrow(ord_attr_df), function(i){
              if(!isTruthy(ord_attr_df[i,1])){return(NULL)}
              data.frame("template_id" = template_id,
                         "node_id" = new_node_id(),
                         "main_element" = "attributes",
                         "node_path" = c("attribute > choice > ordinal > code", "attribute > choice > ordinal > definition", "attribute > choice > ordinal > lowerLimit", "attribute > choice > ordinal > upperLimit", "attribute > choice > ordinal > order", "attribute > choice > ordinal > methodID"),
                         "node_value" = c(ord_attr_df[i,1] , ord_attr_df[i,2], ord_attr_df[i,3], ord_attr_df[i,4], ord_attr_df[i,5], 1))
            }) %>% bind_rows()
          }
          
          qual_attr_df = rhandsontable::hot_to_r(input$qual_attrs)
          if(nrow(qual_attr_df) == 0){
            qual_attr_template = tibble()
          } else {
            qual_attr_template = lapply(1:nrow(qual_attr_df), function(i){
              if(!isTruthy(qual_attr_df[i,1])){return(NULL)}
              data.frame("template_id" = template_id,
                         "node_id" = new_node_id(),
                         "main_element" = "attributes",
                         "node_path" = c("attribute > choice > qualitative > code", "attribute > choice > qualitative > description", "attribute > choice > qualitative > methodID"),
                         "node_value" = c(qual_attr_df[i,1] , qual_attr_df[i,2], 1))
            }) %>% bind_rows()
          }
          
          new_template = bind_rows(method_template, quant_attr_template, ord_attr_template, qual_attr_template) %>% tidyr::drop_na()
          
          templates(bind_rows(templates(), new_template))
          templates_lookup(bind_rows(templates_lookup(), new_lookup))
          
          removeModal()
          shiny::showNotification("Method template added", type = "message")
        }, error = function(e){
          shiny::showNotification("Something went wrong", type = "error")
        })
      }
    )
  })
}
