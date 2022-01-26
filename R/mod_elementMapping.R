#' elementMapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom shinyTree shinyTree renderTree get_selected
#' @importFrom shinyWidgets radioGroupButtons

mod_elementMapping_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    # ----------------- #
    #### Header ####
    fluidRow(
      column(
        width = 12,
        tags$h2(textOutput(ns("tab_selected"))),
        div(textOutput(ns("annotation_main_element")), class = "text-info annotation")
        ## TODO insert small reminder if issues exist with this element
      )
    ),
    
    # ----------------- #
    #### Mappings ####
    fluidRow(
      column(
        width = 12,
        tabsetPanel(
          
          ##### new mapping #####
          tabPanel(
            title = "Create new",  
            tags$h4(tags$b("Elements")),
            div(class = "annotation text-info",
                tags$span("Select the VegX elements you want to use."), 
                shinyWidgets::dropdownButton(circle = T, icon = icon('info'), status = "info", size = "xs", inline = T, width = "600px",
                                             tags$p("The tree lists all available VegX elements for the currently selected main element. You can use the search 
                                                    box to quickly identify the location of an element by its name. If available, additional documentation information 
                                                    from the VegX schema is presented on mouse-over."))
            ),
            br(),
            fluidRow(
              column(7, shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T)),
              column(5, 
                     div(class = "info-box",
                         textOutput(ns("annotation_text"))
                     )
              )
            ),
            
            hr(.noWS = c("before","after")),
            
            # ----------------- #
            
            fluidRow(
              column(
                width = 12,
                tags$h4(tags$b("Values")),
                div(class = "annotation text-info",
                    tags$span("Associate the selected VegX elements with values."), 
                    shinyWidgets::dropdownButton(circle = T, icon = icon('info'), status = "info", size = "xs", inline = T, width = "600px",
                                                 tags$p("There are two ways to map elements to values:"),
                                                 tags$ol(tags$li("Map the selected elements to a column of your uploaded data. Choosing this option will force the create 
                                                             one VegX node per row for the entire VegX main element."),
                                                         tags$li("Provide a free text value. If all mappings in the main element are free text values, this will create a 
                                                             single new VegX node. This option may be useful, e.g., when entering new methods or attributes. If there 
                                                             are mappings present that link to uploaded data, free text values will be copied to all newly created nodes."))
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
                tags$h4(tags$b("Submission")),
                width = 12,
                actionButton(ns("add_nodes"), label = "Add as new nodes", width = "250px", class = "btn-primary", style = "font-weight:bold"),
                actionButton(ns("merge_nodes"), label = "Merge into existing nodes", width = "250px", class = "btn-primary", style = "font-weight:bold"),
                actionButton(ns("replace_nodes"), label = "Replace existing nodes", width = "250px", class = "btn-primary", style = "font-weight:bold")
              )
            )
          ),
          
          # ----------------- #
          ##### templates #####
          tabPanel(
            title = "Templates",
            tags$h1("templates")
          )
        )
      )
    )
  )
}

#' elementMapping Server Functions
#'
#' @noRd 
mod_elementMapping_server <- function(id, user_data, tabs_visible, tab_selected, elem_selected, vegx_mappings, annotation_text, vegx_text, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # --------------------------------------------------------------------------------------- #
    #### Header ####
    # --------------------------------------------------------------------------------------- #
    output$tab_selected = renderText(stringr::str_replace(tab_selected, "^.{1}", toupper)) # Capitalize first letter
    output$annotation_main_element = renderText(xml_attr(xml_find_all(vegx_schema_simple, paste0(".//*[@name='", tab_selected, "']")), "annotation"))
    
    # --------------------------------------------------------------------------------------- #
    #### VegX element selection ####
    # --------------------------------------------------------------------------------------- #
    if(isolate(tab_selected) %in% c("simpleUserDefined", "complexUserDefined")){
      return()
    } else {
      elem_list = schema_to_list(xml_find_all(vegx_schema_simple, paste0(".//*[@name='", tab_selected, "']")), "name") 
    }
    
    output$tree = shinyTree::renderTree(elem_list)
    output$annotation_text = renderText({annotation_text()})
    
    observeEvent(input$tree, {
      # TODO
      # prevent selection of multiple xsd:choice elements 
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
                     mod_rowGenerator_server(id = tmp_id, tab_selected, elem_selected, data_columns, fields_used, vegx_mappings, mapping_count)
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
                   elem_mapped = names(vegx_mappings[[tab_selected]])
                   mappings_remove = setdiff(elem_mapped, elem_used)
                   lapply(mappings_remove, function(name){
                     vegx_mappings[[tab_selected]][[name]] = NULL}
                   )
                 })
    
    # --------------------------------------------------------------------------------------- #
    #### Submission ####
    # --------------------------------------------------------------------------------------- #
    node_values_df =  reactiveVal()
    
    ##### Modal Dialogues #####
    ###### Add nodes ######
    observeEvent(eventExpr = input$add_nodes,
                 handlerExpr = {
                   if(length(vegx_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit", type = "warning")
                   } else {
                     # Process mappings and build node table
                     mappings = isolate(vegx_mappings[[tab_selected]])
                     nodes_df = build_node_values_df(mappings, user_data)
                     
                     # Check df
                     if(is.null(nodes_df)){
                       shiny::showNotification("Mappings table could not be build. Did you specify data columns of different length?", type = "error")
                     } else {
                       node_values_df(nodes_df) 
                     }
                     
                     # Extract basic information for modal dialog
                     n_nodes = nrow(nodes_df)
                     name_nodes = xml_find_all(vegx_schema_simple, paste0("//*[@name='", tab_selected, "']")) %>%
                       xml_children() %>%
                       xml_attr("name")
                     
                     # Show modal dialog
                     showModal(
                       modalDialog(tags$h3("Add new nodes"),
                                   hr(),
                                   tags$label("This action will:"),
                                   tags$li(paste0("Add ", n_nodes," new ", name_nodes," element(s) to your document")),
                                   tags$li("Reset the mapping interface"),
                                   size = "l",
                                   footer = tagList(
                                     tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                               actionButton(ns("confirm_add_nodes"), class = "pull-right btn-success", "Confirm", icon("check")))
                                   ),
                       )
                     )
                   }
                 })
    
    ###### Merge nodes ######
    observeEvent(eventExpr = input$merge_nodes,
                 handlerExpr = {
                   if(length(vegx_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit.", type = "warning")
                   } else if(length(xml_children(xml_find_all(vegx_doc, paste0("//", tab_selected)))) == 0){
                     shiny::showNotification("No nodes to merge with.", type = "warning")
                   } else {
                     # Process mappings and build node table
                     mappings = isolate(vegx_mappings[[tab_selected]])
                     nodes_df = build_node_values_df(mappings, user_data)
                     
                     # Check df
                     if(is.null(nodes_df)){
                       shiny::showNotification("Mappings table could not be build. Did you specify data columns of different length?", type = "error")
                     } else {
                       node_values_df(nodes_df) 
                     }
                     
                     # Extract basic information for modal dialog
                     n_new_nodes = nrow(node_values_df())
                     name_new_nodes = xml_find_all(vegx_schema_simple, paste0("//*[@name='", tab_selected, "']")) %>%
                       xml_children() %>%
                       xml_attr("name")
                     
                     target_nodes_df = nodes_df %>% mutate(target_id = NA) %>% relocate("target_id")
                     target_nodes_hot = rhandsontable::rhandsontable(target_nodes_df, useTypes = F)
                     output$target_nodes_hot = rhandsontable::renderRHandsontable(target_nodes_hot)
                     
                     # Show modal dialog
                     showModal(
                       modalDialog(tags$h3("Merge with existing nodes"),
                                   hr(),
                                   tags$label("This action will merge the below mappings into the specified target nodes."),
                                   rhandsontable::rHandsontableOutput(ns("target_nodes_hot")),
                                   tags$br(),
                                   tags$p("All existing mappings will be reset."),
                                   size = "l",
                                   footer = tagList(
                                     tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                               actionButton(ns("confirm_merge_nodes"), class = "pull-right btn-success", "Confirm", icon("check")))
                                   ),
                       )
                     )
                   }
                 })
    
    ###### Replace nodes ######
    observeEvent(eventExpr = input$replace_nodes,
                 handlerExpr = {
                   if(length(vegx_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit", type = "warning")
                   } else {
                     # Process mappings and build node table
                     mappings = isolate(vegx_mappings[[tab_selected]])
                     nodes_df = build_node_values_df(mappings, user_data)
                     
                     # Check df
                     if(is.null(nodes_df)){
                       shiny::showNotification("Mappings table could not build. Did you specify data columns of different length?", type = "error")
                     } else {
                       node_values_df(nodes_df) 
                     }
                     
                     # ...
                   }
                 })
    
    
    ##### Update document #####
    observeEvent(eventExpr = input$dismiss_modal, 
                 handlerExpr = {
                   removeModal()
                 })
    
    ###### Add nodes ######
    observeEvent(eventExpr = input$confirm_add_nodes, 
                 handlerExpr = {
                   node_values_df = isolate(node_values_df())
                   node_names = colnames(node_values_df)
                   
                   # loop over mappings
                   for(i in 1:nrow(node_values_df)){
                     # Create new node
                     new_node = new_vegx_node(node_names, as.character(node_values_df[i,]))
                     if(is.na(new_node)){next}
                     
                     # Append new node to VegX document
                     parent_missing = (length(xml_find_all(vegx_doc, paste0("./", tab_selected))) == 0)
                     if(parent_missing){
                       xml_add_child(vegx_doc, tab_selected)
                     }
                     parent = xml_find_all(vegx_doc, paste0("./", tab_selected))
                     xml_add_child(parent, new_node)
                   }
                   
                   # Update VegX text 
                   tmp = tempfile(fileext = ".xml")
                   write_xml(vegx_doc, tmp, options = "format")
                   new_text = readChar(tmp, file.info(tmp)$size)
                   vegx_text(new_text)
                   
                   # TODO update action log and progress tab
                   #
                   
                   # Update style
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   
                   # # Jump to next tab
                   tabs = sort(tabs_visible())
                   tab_index = which(tabs == tab_selected)
                   next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper) # TODO smart next tab if empty ids were added
                   nav_select("sidebar", selected = next_tab, session = parent_session)
                   
                   removeModal()
                 })
    
    ###### Merge nodes ######
    observeEvent(eventExpr = input$confirm_merge_nodes, 
                 handlerExpr = {
                   # Get existing ids from document
                   existing_ids = xml_find_all(vegx_doc, paste0("//", tab_selected)) %>% 
                     xml_children() %>% 
                     xml_attr("id")
                   
                   # Prepare data 
                   target_nodes_hot =  rhandsontable::hot_to_r(input$target_nodes_hot) %>% 
                     filter(target_id %in% existing_ids)
                   target_ids = target_nodes_hot$target_id
                   node_values_df = target_nodes_hot %>% dplyr::select(-target_id)
                   node_names = colnames(node_values_df)
                   
                   # loop over mappings
                   # TODO check for compliance with schema (maxOccurs)
                   for(i in 1:nrow(target_nodes_hot)){
                     
                     merge_into_vegx_node(target_ids[i], node_names, as.character(node_values_df[i,]))
                   }
                   
                   # Update VegX text 
                   tmp = tempfile(fileext = ".xml")
                   write_xml(vegx_doc, tmp, options = "format")
                   new_text = readChar(tmp, file.info(tmp)$size)
                   vegx_text(new_text)
                   
                   # TODO update action log and progress tab
                   #
                   
                   # Update style
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   
                   # # Jump to next tab
                   tabs = sort(tabs_visible())
                   tab_index = which(tabs == tab_selected)
                   next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper) # TODO smart next tab if empty ids were added
                   nav_select("sidebar", selected = next_tab, session = parent_session)
                   
                   removeModal()
                 })
  })
}
