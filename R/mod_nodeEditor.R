#' nodeEditor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom stats setNames
#' @importFrom jsonlite fromJSON
#' @importFrom shinyTree shinyTree renderTree get_selected
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyjs enable disable addClass click useShinyjs

mod_nodeEditor_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),
    # ----------------- #
    #### Header ####
    fluidRow(
      column(
        width = 10, offset = 1,
        tags$h2(textOutput(ns("tab_selected"))),
        div(textOutput(ns("annotation_main_element")), class = "text-info annotation")
        ## TODO insert small reminder if issues exist with this element
      )
    ),
    
    # ----------------- #
    #### Mappings ####
    fluidRow(
      column(
        width = 10, offset = 1,
        tabsetPanel(
          id = ns("tabset"),
          tabPanel(
            value = "create",
            title = "Create new",  
            
            fluidRow(
              column(
                width = 12,
                tags$h4(tags$b("Elements")),
                div(class = "annotation text-info",
                    tags$span("Select the VegX elements you want to use."), 
                    shinyWidgets::dropdownButton(circle = T, icon = icon('info'), status = "info", size = "xs", inline = T, width = "600px",
                                                 tags$p("The tree lists all available VegX elements for the currently selected main element. You can use the search
                                                    box to quickly identify the location of an element by its name. If available, additional documentation information
                                                    from the VegX schema is presented on mouse-over."))
                ),
                br(),
                shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
                hr(.noWS = c("before","after")),
              )
            ),
            
            # ----------------- #
            
            fluidRow(
              column(
                width = 12,
                tags$h4(tags$b("Values")),
                div(class = "annotation text-info",
                    tags$span("Associate the selected VegX elements with values."), 
                    shinyWidgets::dropdownButton(circle = T, icon = icon('info'), status = "info", size = "xs", inline = T, width = "600px",
                                                 tags$p("There are two ways to map elements to values:"),
                                                 tags$ol(tags$li("Map the selected elements to a column of your uploaded data. Choosing this option will create a new 
                                                                 VegX node for each row in the data."),
                                                         tags$li("Provide a free text value. If all mappings in the main element are free text values, this will create a 
                                                             single new VegX node. This option may be useful, e.g., when entering new methods or attributes. If there 
                                                             are mappings present that link to uploaded data, free text values will be copied to all newly created nodes.")
                                                 )
                    )
                ),
                fluidRow(
                  align = "center", 
                  column(5, tags$label("VegX Element")),
                  column(5, tags$label("Value(s)")),
                  column(2,
                         column(9, tags$label("Values from"))
                  )
                ),
                div(id = ns("placeholder")),
                actionButton(ns("add_mapping"), label = "Add mapping", icon = icon("plus", class = "icon-padded")), 
                hr(.noWS = c("before","after")),
              )
            ),
            
            # ----------------- #
            
            fluidRow(
              column(
                width = 12,
                actionButton(ns("add_nodes"), label = "Add new node(s)", width = "250px", class = "btn-primary", style = "font-weight:bold"),
                actionButton(ns("merge_nodes"), label = "Merge into existing node(s)", width = "250px", class = "btn-primary", style = "font-weight:bold"),
              )
            )
          )
        )
        # Templates Tab is build and inserted server-side
      )
    )
  )
}

#' nodeEditor Server Functions
#'
#' @noRd 
mod_nodeEditor_server <- function(id, user_data, tab_selected, elem_selected, elem_mappings, vegx_schema, vegx_doc, vegx_txt, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # --------------------------------------------------------------------------------------- #
    #### Header ####
    # --------------------------------------------------------------------------------------- #
    output$tab_selected = renderText(stringr::str_replace(tab_selected, "^.{1}", toupper)) # Capitalize first letter
    output$annotation_main_element = renderText(xml_attr(xml_find_all(vegx_schema, paste0(".//*[@name='", tab_selected, "']")), "annotation"))
    
    # --------------------------------------------------------------------------------------- #
    #### VegX element selection ####
    # --------------------------------------------------------------------------------------- #
    output$tree = shinyTree::renderTree(
      if(isolate(tab_selected) %in% c("simpleUserDefined", "complexUserDefined")){
        return()
      } else {
        schema_to_list(xml_find_all(vegx_schema, paste0(".//*[@name='", tab_selected, "']")), "name") 
      }
    )
    
    observeEvent(input$tree, {
      # TODO
      # prevent selection of multiple xsd:choice elements 
      # Highlight ID elements (with a chain icon?)
      selected = shinyTree::get_selected(input$tree, "slices") # Get selected elements
      selected = names(unlist(selected)) %>% str_replace_all("\\.", " > ")
      is_leaf = stringr::str_extract(selected, "[^ > ]+$") %in% vegx_leaf_elements # Check which selected elements are leaf nodes
      elem_selected[[tab_selected]] = selected[is_leaf] # Update elem_selected with selected leaf nodes
      updateSelectInput(inputId = "mapping_1", choices = elem_selected[[tab_selected]])
    })
    
    # --------------------------------------------------------------------------------------- #
    #### Element Mapping ####
    # --------------------------------------------------------------------------------------- #
    mapping_id = reactiveVal(1)    # running number for creating unique input widgets
    mapping_count = reactiveVal(0) # Counter for number of rows shown
    fields_used = reactiveValues(elements = list(), values = list()) # Elements and values currently selected
    
    data_columns = reactive({
      col_vectors = sapply(names(user_data), simplify = F, USE.NAMES = T, function(file_name){
        name_vec = user_data[[file_name]]$x$rColHeaders
        name_vec = setNames(paste(file_name, name_vec, sep = "$"), name_vec)
      })
    })
    
    # On startup, add one mapping row
    observeEvent(eventExpr = mapping_count,
                 handlerExpr = shinyjs::click("add_mapping"))
    
    # Adding new mapping row
    observeEvent(eventExpr = input$add_mapping,
                 handlerExpr = {
                   if(length(elem_selected[[tab_selected]]) > mapping_count() | mapping_count() == 0){ 
                     tmp_id = paste0("row", mapping_id())
                     mod_rowGenerator_server(id = tmp_id, tab_selected, elem_selected, data_columns, fields_used, elem_mappings, mapping_count)
                     insertUI(selector = paste0("#", ns("placeholder")),
                              where = "beforeBegin",
                              ui = mod_rowGenerator_ui(ns(tmp_id))
                     )
                     mapping_id(mapping_id() + 1)
                     mapping_count(mapping_count() + 1)
                   } else {
                     shiny::showNotification("Please select more Elements.", type = "warning")
                   }
                 }
    )
    
    # Remove deprecated mappings (e.g. when input$element has changed)
    observeEvent(eventExpr = fields_used$elements,
                 handlerExpr = {
                   elem_used = unlist(fields_used[["elements"]])
                   elem_mapped = names(elem_mappings[[tab_selected]])
                   mappings_remove = setdiff(elem_mapped, elem_used)
                   lapply(mappings_remove, function(name){
                     elem_mappings[[tab_selected]][[name]] = NULL}
                   )
                 })
    
    # --------------------------------------------------------------------------------------- #
    #### Submission ####
    # --------------------------------------------------------------------------------------- #
    node_values_df =  reactiveVal()
    node_ids = reactiveVal()
    
    # Dismiss Dialogue
    observeEvent(eventExpr = input$dismiss_modal, 
                 handlerExpr = {
                   removeModal()
                 })
    
    ##### Add nodes ######
    ###### Dialogue ######
    observeEvent(eventExpr = input$add_nodes,
                 handlerExpr = {
                   if(length(elem_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit", type = "warning")
                   } else {
                     # Process mappings and build node table
                     mappings = isolate(elem_mappings[[tab_selected]])
                     nodes_df = build_node_values_df(mappings, user_data)
                     
                     # Check df
                     if(is.null(nodes_df)){
                       shiny::showNotification("Mappings table could not be build. Did you specify data columns with different lengths?", type = "error")
                       return()
                     } else {
                       node_values_df(nodes_df) 
                     }
                     
                     # Extract basic information for modal dialog
                     n_nodes = nrow(nodes_df)
                     name_nodes = xml_find_all(vegx_schema, paste0("//*[@name='", tab_selected, "']")) %>%
                       xml_children() %>%
                       xml_attr("name")
                     
                     # Show modal dialog
                     showModal(
                       modalDialog(tags$h3("Add new nodes"),
                                   hr(),
                                   tags$label(paste0("This action will add ", n_nodes," new ", name_nodes," element(s) to your document.")),
                                   tags$br(),
                                   tags$br(),
                                   tags$p("Please specify whether IDs should be generated automatically or from a data column"),
                                   fluidRow(
                                     column(4, selectInput(ns("id_source"), label = "ID source", choices = list("Auto" = "auto", "File" = "file"), width = "100%")),
                                     column(8, uiOutput(ns("id_column")))
                                   ),
                                   
                                   tags$p("All mappings will be reset."),
                                   size = "l",
                                   footer = tagList(
                                     tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                               actionButton(ns("confirm_add_nodes"), class = "pull-right btn-success", "Confirm", icon("check")))
                                   )
                       )
                     )
                   }
                 })
    
    
    ###### Check ID source ######
    observeEvent(eventExpr = input$id_source,
                 handlerExpr = {
                   if(input$id_source == "auto"){
                     shinyjs::enable(id = "confirm_add_nodes")
                   } else {
                     shinyjs::disable(id = "confirm_add_nodes")  
                   }
                   
                   output$id_column = renderUI({
                     if(input$id_source == "file"){
                       if(length(data_columns()) == 0){
                         dropdown_choices = c("No files found" = "")
                       } else {
                         dropdown_choices = c("Select columns from uploaded data" = "", data_columns())
                       }
                       selectInput(ns("id_column"), label = "Column", width = "100%", choices = dropdown_choices)
                     }
                   })
                 })
    
    observe({
      req(input$id_column)
      if(input$id_column != ""){
        try({
          file_name = str_split(input$id_column, "\\$", simplify = T)[1]
          column_name = str_split(input$id_column, "\\$", simplify = T)[2]
          upload = user_data[[file_name]]
          upload_df = jsonlite::fromJSON(upload$x$data)
          colnames(upload_df) = upload$x$rColHeaders
          ids = upload_df[,column_name]
          if(length(ids) != nrow(node_values_df())){
            shinyjs::disable(id = "confirm_add_nodes")  
            shiny::showNotification("Number of IDs does not equal the number of nodes to be created", type = "warning")
            updateSelectInput(session, "id_column", selected = "")
          } else if(length(ids) != length(unique(ids))){
            shinyjs::disable(id = "confirm_add_nodes")  
            shiny::showNotification("Specified IDs are not unique", type = "warning")
            updateSelectInput(session, "id_column", selected = "")
          } else if(any(ids %in% xml_attr(xml_children(xml_find_all(vegx_doc, paste0("//", tab_selected))), "id"))){
            shinyjs::disable(id = "confirm_add_nodes")  
            shiny::showNotification("Specified IDs in use", type = "warning")
            updateSelectInput(session, "id_column", selected = "")
          } else {
            shinyjs::enable(id = "confirm_add_nodes")
            node_ids(ids)
          }
        })
      }
    })
    
    ###### Update ###### 
    observeEvent(eventExpr = input$confirm_add_nodes, 
                 handlerExpr = {
                   n_insertions = 0
                   n_failures = 0
                   n_errors = 0
                   n_warnings = 0
                   
                   node_values_df = isolate(node_values_df())
                   node_names = colnames(node_values_df)
                   
                   # Get IDs
                   ids = isolate(node_ids())
                   node_ids(NULL)
                   if(length(ids) == 0){ids = NULL}
                   
                   # loop over mappings
                   for(i in 1:nrow(node_values_df)){
                     # Create new node
                     fct_result = new_vegx_node(vegx_schema, node_names, as.character(node_values_df[i,]), id = ids[i], log_path)
                     new_node = fct_result$node
                     n_errors = n_errors + fct_result$errors
                     n_warnings = n_warnings + fct_result$warnings 
                     
                     if(is.null(new_node)){
                       n_failures = n_failures + 1
                     } else{
                       # Append new node to VegX document, respect sequence order defined in schema
                       parent_missing = (length(xml_find_all(vegx_doc, paste0("./", tab_selected))) == 0)
                       if(parent_missing){
                         elements_present = xml_root(vegx_doc) %>% xml_children() %>% xml_name()
                         if(length(elements_present) > 0){
                           elements_ordered = vegx_main_elements[vegx_main_elements %in% c(elements_present, tab_selected)]
                           insert_position = which(elements_ordered == tab_selected) - 1
                           xml_add_child(vegx_doc, tab_selected, .where = insert_position)  
                         } else {
                           xml_add_child(vegx_doc, tab_selected)  
                         }
                       }
                       parent = xml_find_all(vegx_doc, paste0("./", tab_selected))
                       xml_add_child(parent, new_node)
                       n_insertions = n_insertions + 1
                     }
                   }
                   
                   # Update VegX text 
                   vegx_txt(as.character(vegx_doc))
                   
                   # Reset node_ids reactiveVal
                   node_ids(NULL)
                   
                   # Update action log 
                   action_log(read_action_log(log_path))
                   
                   # Show notification
                   if(n_insertions > 0){shiny::showNotification(paste0(n_insertions, " node(s) successfully added."), type = "default") }
                   if(n_warnings > 0 | n_errors > 0){shiny::showNotification(paste0(n_warnings, " warning(s) and ", n_errors, " error(s) encountered. Please consult the log for more information."), type = "warning")} 
                   if(n_failures > 0){shiny::showNotification(paste0(n_failures, " node(s) not added. Please consult the log for more information."), type = "error") }
                   
                   # Update style and exit
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   removeModal()
                 })
    
    ##### Merge nodes #####
    ###### Dialogue ######
    observeEvent(eventExpr = input$merge_nodes,
                 handlerExpr = {
                   if(length(elem_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit.", type = "warning")
                   } else if(length(xml_children(xml_find_all(vegx_doc, paste0("//", tab_selected)))) == 0){
                     shiny::showNotification("No nodes to merge with.", type = "warning")
                   } else {
                     # Process mappings and build node table
                     mappings = isolate(elem_mappings[[tab_selected]])
                     nodes_df = build_node_values_df(mappings, user_data)
                     
                     # Check df
                     if(is.null(nodes_df)){
                       shiny::showNotification("Mappings table could not be build. Did you specify data columns of different length?", type = "error")
                       return()
                     } else {
                       node_values_df(nodes_df) # save as reactive
                     }
                     
                     # Extract basic information for modal dialog
                     n_new_nodes = nrow(node_values_df())
                     name_new_nodes = xml_find_all(vegx_schema, paste0("//*[@name='", tab_selected, "']")) %>%
                       xml_children() %>%
                       xml_attr("name")
                     
                     target_nodes_df = nodes_df %>% mutate("target_id" = NA) %>% relocate("target_id")
                     target_nodes_hot = rhandsontable::rhandsontable(target_nodes_df, useTypes = F)
                     output$target_nodes_hot = rhandsontable::renderRHandsontable(target_nodes_hot)
                     
                     # Show modal dialog
                     showModal(
                       modalDialog(tags$h3("Merge with existing nodes"),
                                   hr(),
                                   tags$label("This action will merge the mappings into the target specified below nodes."),
                                   rhandsontable::rHandsontableOutput(ns("target_nodes_hot")),
                                   tags$br(),
                                   tags$p("All mappings will be reset."),
                                   size = "l",
                                   footer = tagList(
                                     tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                               actionButton(ns("confirm_merge_nodes"), class = "pull-right btn-success", "Confirm", icon("check")))
                                   ),
                       )
                     )
                   }
                 })
    
    ###### Update ######
    observeEvent(eventExpr = input$confirm_merge_nodes, 
                 handlerExpr = {
                   n_merges = 0
                   n_failures = 0
                   n_warnings = 0
                   
                   # Prepare data 
                   target_nodes_hot =  rhandsontable::hot_to_r(input$target_nodes_hot)
                   node_values_df = target_nodes_hot %>% dplyr::select(-.data$target_id)
                   target_ids = target_nodes_hot$target_id
                   
                   node_paths = colnames(node_values_df)
                   node_names = node_paths %>% str_split(" > ")
                   root_name = unique(sapply(node_names, function(names){names[1]}))

                   # loop over mappings
                   for(i in 1:nrow(target_nodes_hot)){
                     # Create new node
                     target_root = xml_find_all(vegx_doc, paste0("//", root_name, "[@id='", target_ids[i], "']")) ####
                     node_values = as.character(node_values_df[i,])
                     fct_result = merge_into_vegx_node(vegx_schema, target_root, node_paths, node_values, log_path)
                     if(fct_result$errors == 0){
                       n_merges = n_merges + 1
                     } else {
                       n_failures = n_failures + fct_result$errors  
                     }
                     n_warnings = n_warnings + fct_result$warnings 
                   }
                   
                   # Update VegX text 
                   vegx_txt(as.character(vegx_doc))
                   
                   # Update Action log 
                   action_log(read_action_log(log_path))
                   
                   # Show notification
                   if(n_merges > 0){shiny::showNotification(paste0("Successfully merged into ", n_merges, " node(s)."), type = "default") }
                   if(n_warnings > 0){shiny::showNotification(paste0(n_warnings, " warning(s) encountered. Please consult the log for more information."), type = "warning")} 
                   if(n_failures > 0){shiny::showNotification(paste0("Merge failed for ", n_failures, " node(s). Please consult the log for more information."), type = "error") }
                   
                   # Update style and exit
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   removeModal()
                 })
    
    # --------------------------------------------------------------------------------------- #
    #### Templates ####
    # --------------------------------------------------------------------------------------- #
    ##### UI #####
    if(tab_selected %in% templates_lookup$target_element){
      templates_elem_overview = templates_lookup %>% filter(.data$target_element == tab_selected)
      templates_elem = templates %>% filter(.data$template_id %in% templates_elem_overview$template_id)
      
      output$templates = DT::renderDataTable(templates_elem_overview, # DT also creates input objects that can be accessed (see below input$templates_rows_selected)
                                             rownames = FALSE,
                                             options = list(columnDefs = list(list(width = '80px', targets = 0)))) 
      
      output$templates_selected = renderText(paste(templates_elem_overview$name[input$templates_rows_selected], collapse = ", "))
      
      insertTab(
        inputId = "tabset",
        tab = tabPanel(
          value= templates,
          title = "Templates",
          fluidRow(
            column(
              width = 12,
              tags$h4(tags$b("Templates selected:")),
              textOutput(ns("templates_selected")),
              
              hr(),
              DT::dataTableOutput(ns("templates")),
              hr(.noWS = c("before","after"))
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionButton(ns("add_template"), label = "Add template(s)", width = "250px", class = "btn-primary", style = "font-weight:bold")
            )
          )
        )
      )
      
      ##### Dialogue #####
      observeEvent(eventExpr = input$add_template,
                   handlerExpr = {
                     if(length(input$templates_rows_selected) == 0){
                       shiny::showNotification("Nothing to submit", type = "warning")
                     } else {
                       # Summarize selection
                       templates_selected = templates_lookup %>% filter(.data$target_element == tab_selected) %>% 
                         slice(input$templates_rows_selected) %>% 
                         pull(template_id)
                       
                       nodes_summary = templates %>% 
                         dplyr::filter(.data$template_id %in% templates_selected) %>% 
                         group_by(.data$template_id, .data$node_id, .data$main_element) %>% 
                         tally()
                       
                       # Extract basic information for modal dialog
                       n_nodes = nrow(nodes_summary)
                       node_elements = unique(nodes_summary$main_element) 
                       
                       # Show modal dialog
                       showModal(
                         modalDialog(tags$h3("Add template node"),
                                     hr(),
                                     tags$label("This action will:"),
                                     tags$li(paste0("Add ", n_nodes," new element(s) to the following main elements: ", paste0(node_elements, collapse = ", "))),
                                     tags$br(),
                                     tags$p("All existing mappings will be reset."),
                                     size = "l",
                                     footer = tagList(
                                       tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                                 actionButton(ns("confirm_add_template"), class = "pull-right btn-success", "Confirm", icon("check")))
                                     ),
                         )
                       )
                     }
                   })
      
      ##### Update #####
      observeEvent(eventExpr = input$confirm_add_template, 
                   handlerExpr = {
                     n_insertions = 0
                     n_failures = 0
                     n_errors = 0
                     n_warnings = 0
                     
                     templates_selected = templates_lookup %>% filter(.data$target_element == tab_selected) %>% 
                       slice(input$templates_rows_selected) %>% 
                       pull(template_id)
                     
                     template_mappings = templates %>% 
                       dplyr::filter(template_id %in% templates_selected) %>% 
                       group_by(.data$template_id) %>% 
                       group_split()
                     
                     # loop over template_ids
                     for(i in 1:length(template_mappings)){
                       node_mappings = template_mappings[[i]] %>% 
                         group_by(node_id) %>% 
                         group_split()
                       
                       new_nodes = list()
                       # loop over node_ids, create new nodes
                       for(j in 1:length(node_mappings)){ 
                         fct_result = new_vegx_node(vegx_schema, node_mappings[[j]]$node_path, node_mappings[[j]]$node_value, id = NULL, log_path)
                         new_node = fct_result$node
                         n_errors = n_errors + fct_result$errors
                         n_warnings = n_warnings + fct_result$warnings 
                         
                         leaf_nodes = xml_find_all(new_node, "//*[not(*)]")
                         leaf_names = leaf_nodes %>% xml_name()
                         if(any(str_detect(leaf_names, "ID$"))){  # ID links present, replace template-internal node_id with actual id generated for VegX output
                           id_nodes = leaf_nodes[str_detect(leaf_names, "ID$")]
                           for(id_node in id_nodes){
                             node_id = as.numeric(xml_text(id_node))
                             if(node_id > j){ # links allowed only to earlier nodes
                               new_node = NULL 
                               break
                             } 
                             vegx_id = xml_attr(new_nodes[[node_id]], "id")
                             xml_text(id_node) = vegx_id
                           }
                         }
                         
                         # Save new node for lookup in subsequent iterations
                         new_nodes[[j]] = new_node
                         
                         # Append new node to VegX document
                         if(is.null(new_node)){
                           new_action_log_record(log_path, "Template error", paste0("Ivalid ID reference for 'template_id=", node_mappings[[j]]$template_id[1], 
                                                                                    "'. Make sure to reference only earlier node_ids in a template"))
                           n_failures = n_failures + 1
                           next
                         } else {
                           parent_name = unique(node_mappings[[j]]$main_element)
                           parent_missing = (length(xml_find_all(vegx_doc, paste0("./", parent_name))) == 0)
                           if(parent_missing){
                             xml_add_child(vegx_doc, parent_name)
                           }
                           parent = xml_find_all(vegx_doc, paste0("./", parent_name))
                           xml_add_child(parent, new_node)  
                           n_insertions = n_insertions + 1
                         }
                       }
                     }
                     
                     # Update VegX text 
                     vegx_txt(as.character(vegx_doc))
                     
                     # Update Action log 
                     action_log(read_action_log(log_path))
                     
                     # Show notification
                     if(n_insertions > 0){shiny::showNotification(paste0(n_insertions, " node(s) successfully added."), type = "default") }
                     if(n_warnings > 0 | n_errors > 0){shiny::showNotification(paste0(n_warnings, " warning(s) and ", n_errors, " error(s) encountered. Please consult the log for more information."), type = "warning")} 
                     if(n_failures > 0){shiny::showNotification(paste0(n_failures, " node(s) not added. Please consult the log for more information."), type = "error") }
                     
                     # Update style and exit
                     shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                     
                     removeModal()
                   })
    }
  })
}
