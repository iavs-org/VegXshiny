#' elementMapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom DT renderDataTable
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
          id = ns("tabset"),
          tabPanel(
            value = "create",
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
              column(12, shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T))
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
                width = 12,
                actionButton(ns("add_nodes"), label = "Add new node(s)", width = "250px", class = "btn-primary", style = "font-weight:bold"),
                actionButton(ns("merge_nodes"), label = "Merge into existing node(s)", width = "250px", class = "btn-primary", style = "font-weight:bold"),
                actionButton(ns("replace_nodes"), label = "Replace existing node(s)", width = "250px", class = "btn-primary", style = "font-weight:bold")
              )
            )
          )
        )
        # Templates Tab is build and inserted server-side
      )
    )
  )
}

#' elementMapping Server Functions
#'
#' @noRd 
mod_elementMapping_server <- function(id, user_data, tabs_visible, tab_selected, elem_selected, vegx_mappings, vegx_txt, action_log, parent_session){
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
    
    observeEvent(input$tree, {
      # TODO
      # prevent selection of multiple xsd:choice elements 
      # Highlight ID elements (with a chain icon?)
      # Render linked tree if ID element is selected?
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
                       return
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
                                   tags$br(),
                                   tags$p("All existing mappings will be reset."),
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
                       return()
                     } else {
                       node_values_df(nodes_df) # save as reactive
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
                       shiny::showNotification("Mappings table could not be build. Did you specify data columns of different length?", type = "error")
                       return
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
                     new_node = new_vegx_node(node_names, as.character(node_values_df[i,]), session = session)
                     if(is.null(new_node)){next}
                     
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
                   }
                   
                   # Update VegX text 
                   tmp = tempfile(fileext = ".xml")
                   write_xml(vegx_doc, tmp, options = "format")
                   new_text = readChar(tmp, file.info(tmp)$size)
                   vegx_txt(new_text)
                   
                   # Update Action log 
                   log = read_action_log(session)
                   action_log(log)
                   
                   # Update style
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   
                   # Jump to next tab
                   # tabs = sort(tabs_visible())
                   # tab_index = which(tabs == tab_selected)
                   # next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper) # TODO smart next tab if empty ids were added
                   # nav_select("sidebar", selected = next_tab, session = parent_session)
                   
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
                     merge_into_vegx_node(target_ids[i], node_names, as.character(node_values_df[i,]), session)
                   }
                   
                   # Update VegX text 
                   tmp = tempfile(fileext = ".xml")
                   write_xml(vegx_doc, tmp, options = "format")
                   new_text = readChar(tmp, file.info(tmp)$size)
                   vegx_txt(new_text)
                   
                   # Update Action log 
                   log = read_action_log(session)
                   action_log(log)
                   
                   # Update style
                   shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   
                   # # Jump to next tab
                   # tabs = sort(tabs_visible())
                   # tab_index = which(tabs == tab_selected)
                   # next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper) # TODO smart next tab if empty ids were added
                   # nav_select("sidebar", selected = next_tab, session = parent_session)
                   
                   removeModal()
                 })
    
    
    # --------------------------------------------------------------------------------------- #
    #### Templates ####
    # --------------------------------------------------------------------------------------- #
    if(tab_selected %in% templates_lookup$target_element){
      templates_elem_overview = templates_lookup %>% filter(target_element == tab_selected)
      templates_elem = templates %>% filter(template_id %in% templates_elem_overview$template_id)
      
      output$templates = DT::renderDataTable(templates_elem_overview, # DT also creates input objects that can be accessed (see below input$templates_rows_selected)
                                             style = "bootstrap",
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
      
      observeEvent(eventExpr = input$add_template,
                   handlerExpr = {
                     if(length(input$templates_rows_selected) == 0){
                       shiny::showNotification("Nothing to submit", type = "warning")
                     } else {
                       # Summarize selection
                       nodes_summary = templates %>% 
                         dplyr::filter(template_id %in% templates_lookup$template_id[input$templates_rows_selected]) %>% 
                         group_by(template_id, node_id, main_element) %>% 
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
      
      observeEvent(eventExpr = input$confirm_add_template, 
                   handlerExpr = {
                     template_mappings = templates %>% 
                       dplyr::filter(template_id %in% templates_lookup$template_id[input$templates_rows_selected]) %>% 
                       group_by(template_id) %>% 
                       group_split()
                     
                     # loop over template_ids
                     for(i in 1:length(template_mappings)){
                       node_mappings = template_mappings[[i]] %>% 
                         group_by(node_id) %>% 
                         group_split()
                       
                       new_nodes = list()
                       # loop over node_ids, create new nodes
                       for(j in 1:length(node_mappings)){ 
                         new_node = new_vegx_node(node_mappings[[j]]$node_path, node_mappings[[j]]$node_value, session = session)
                         leaf_nodes = xml_find_all(new_node, "//*[not(*)]")
                         leaf_names = leaf_nodes %>% xml_name()
                         if(any(str_detect(leaf_names, "ID$"))){  # ID links present, replace internal node_id with actual id generated for VegX output
                           id_nodes = leaf_nodes[str_detect(leaf_names, "ID$")]
                           for(id_node in id_nodes){
                             node_id = as.numeric(xml_text(id_node))
                             if(node_id > j){
                               error(paste0("Ivalid ID reference with 'template_id=", node_mappings[[j]]$template_id[1], "'. 
                                            Make sure to reference only earlier node_ids in a template"))
                             } 
                             vegx_id = xml_attr(new_nodes[[node_id]], "id")
                             xml_text(id_node) = vegx_id
                           }
                         }
                         
                         # Save new node for lookup later
                         new_nodes[[j]] = new_node
                         
                         # Append new node to VegX document
                         parent_name = unique(node_mappings[[j]]$main_element)
                         parent_missing = (length(xml_find_all(vegx_doc, paste0("./", parent_name))) == 0)
                         if(parent_missing){
                           xml_add_child(vegx_doc, parent_name)
                         }
                         parent = xml_find_all(vegx_doc, paste0("./", parent_name))
                         xml_add_child(parent, new_node)
                       }
                     }
                     
                     # Update VegX text 
                     tmp = tempfile(fileext = ".xml")
                     write_xml(vegx_doc, tmp, options = "format")
                     new_text = readChar(tmp, file.info(tmp)$size)
                     vegx_txt(new_text)
                     
                     # Update Action log 
                     log = read_action_log(session)
                     action_log(log)
                     
                     # Update style
                     shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                     
                     removeModal()
                   })
    }
  })
}
