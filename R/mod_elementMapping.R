#' elementMapping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyTree shinyTree renderTree get_selected
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyjs click addClass
#' @importFrom bslib nav_select
#' @importFrom stringr str_replace_all
#' @importFrom xml2 xml_find_all xml_add_child

mod_elementMapping_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    # ----------------- #
    fluidRow(
      column(
        width = 12,
        tags$h2(textOutput(ns("tab_selected"))),
        div(textOutput(ns("annotation_main_element")), class = "text-info annotation"),
        hr()
      )
    ),
    
    # ----------------- #
    fluidRow(
      column(
        width = 12,
        tags$h4(tags$b("Elements")),
        div(class = "annotation text-info",
            tags$span("Select the VegX elements you want to use."), 
            shinyWidgets::dropdownButton(circle = T, icon = icon('info'), status = "info", size = "xs", inline = T, width = "600px",
                                         tags$p("The tree lists all available VegX elements for the currently selected main element. You can use the search box to quickly identify the 
                                             location of an element by its name. If available, additional documentation information from the VegX schema is presented on mouse-over."))
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
        hr(.noWS = "before")
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
                                         tags$ol(tags$li("Map the selected elements to a column of your uploaded data. Choosing this option will force the create one VegX node 
                                                         per row for the entire VegX main element."),
                                                 tags$li("Provide a free text value. If all mappings in the main element are free text values, this will create a single new VegX node. 
                                                          This option may be useful, e.g., when entering new methods or attributes. If there are mappings present that link to uploaded data, 
                                                          free text values will be copied to all newly created nodes."))
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
      align = "center",
      column(
        width = 12,
        actionButton(ns("submit"), label = "Submit", width = "300px", class = "btn-primary", style = "font-weight:bold"),
        shinyjs::hidden(actionButton(ns("edit"), label = "Edit", width = "300px", class = "btn-success", style = "font-weight:bold")))
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
      sapply(names(user_data), simplify = F, USE.NAMES = T, function(file_name){
        user_data[[file_name]]$x$rColHeaders
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
    #### Submit & Edit buttons ####
    # --------------------------------------------------------------------------------------- #
    observeEvent(eventExpr = input$submit,
                 handlerExpr = {
                   if(length(vegx_mappings[[tab_selected]]) == 0){
                     shiny::showNotification("Nothing to submit", type = "warning")
                   } else {
                     # Process mappings and build VegX
                     mappings = isolate(vegx_mappings[[tab_selected]])
                     node_paths = names(unlist(mappings))
                     node_values = unname(unlist(mappings))
                     new_node = mappings_to_vegx(node_paths, node_values)
                     
                     # Append new node to VegX document
                     parent_missing = (length(xml_find_all(vegx_doc, paste0("./", tab_selected))) == 0)
                     if(parent_missing){
                       xml_add_child(vegx_doc, tab_selected)
                     }
                     parent = xml_find_all(vegx_doc, paste0("./", tab_selected))
                     xml_add_child(parent, new_node)
         
                     # Update VegX text 
                     tmp = tempfile(fileext = ".xml")
                     write_xml(vegx_doc, tmp, options = "format")
                     new_text = readChar(tmp, file.info(tmp)$size)
                     vegx_text(new_text)
                     
                     # update action log and progress tab
                     
                     # Remove incomplete mappings
                     showModal(
                       modalDialog(tags$label("This action will:"),
                                   tags$li("Remove x incomplete mappings"),
                                   tags$li("Add x Vew vegX elements to your document"),
                                   footer = modalButton("Confirm"),
                                   size = "m",
                                   easyClose = T
                       )
                     )
                     
                     # Update style
                     shinyjs::addClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                     
                     # # Disable UI
                     # shinyjs::disable(selector = paste0("div[data-value='", stringr::str_replace(tab_selected, "^.{1}", toupper), "'] > *"))
                     # shinyjs::runjs(paste0("$('#", ns("tree"), " li').each( function() {
                     #                        $('#", ns("tree") ,"').jstree().disable_node(this.id);
                     #                      })"))
                     # 
                     # # Insert Edit Button
                     # shinyjs::hide("submit")
                     # shinyjs::enable("edit")
                     # shinyjs::enable(selector = ".btn-circle-xs")
                     # shinyjs::show("edit")
                     # 
                     # # Jump to next tab
                     # tabs = sort(tabs_visible())
                     # tab_index = which(tabs == tab_selected)
                     # next_tab = stringr::str_replace(tabs[(tab_index  %% length(tabs)) + 1], "^.{1}", toupper)
                     # nav_select("sidebar", selected = next_tab, session = parent_session)
                   }
                 })
    
    observeEvent(eventExpr = input$edit,
                 handlerExpr = {
                   # Update style of tab in sidebar
                   shinyjs::removeClass(class = "bg-success", selector = paste0("a[data-value=", stringr::str_replace(tab_selected, "^.{1}", toupper), "]"))
                   shinyjs::enable(selector = paste0("div[data-value='", stringr::str_replace(tab_selected, "^.{1}", toupper), "'] > *"))
                   shinyjs::runjs(paste0("$('#", ns("tree"), " li').each( function() {
                                            $('#", ns("tree") ,"').jstree().enable_node(this.id);
                                          })"))
                   
                   # Show submit button
                   shinyjs::hide("edit")
                   shinyjs::show("submit")
                 })
  })
}
