#' turbovegImport UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_turbovegImport_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
    column(
      width = 10, offset = 1,
      
      fluidRow(
        h2("Project"),
        textInput(inputId = ns("project_title"), label = "Project title *",  width = "100%"),
        textAreaInput(inputId = ns("project_abstract"), label = "Abstract", width = "100%", resize = "vertical"),
        textInput(inputId = ns("project_citation"), label = "Citation", width = "100%"),
        tags$label("Responsible Party"),
        fluidRow(
          column(width = 4, textInput(ns("party_name"), "Name", width = "100%")),
          column(width = 4, textInput(ns("party_role"), "Role", width = "100%")),
          column(width = 4, selectizeInput(ns("party_type"), label = "Type", choices = c("", "Individual", "Organization", "Position"), width = "100%"))
        )
      ),
      
      fluidRow(
        tags$h2 ("File"),
        tags$p("Select the Turboveg XML file you want to import", class = "text-info annotation"),
        selectizeInput(ns("tv_file"), label = NULL, choices = c("No files found" = ""))
      ), 
      
      hr(),
      
      fluidRow(
        actionButton(ns("submit"), label = "submit", width = "250px", class = "btn-success center-block")
      )
    )
  )
}

#' turbovegImport Server Functions
#'
#' @noRd 
mod_turbovegImport_server <- function(id, user_data, vegx_schema, vegx_doc, vegx_txt, templates, templates_lookup, action_log, log_path){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tv_dfs = reactiveVal()
    
    dropdown_empty = reactive({
      if(length(names(user_data)) == 0){
        c("No files found" = "")
      } else {
        c("Choose a file" = "")
      }
    })
    
    schema_list_plots = vegx_schema %>% xml_find_all("//*[@name='plots']") %>% schema_to_list(name_attr = "name") 
    schema_list_plotObs = vegx_schema %>% xml_find_all("//*[@name='plotObservations']") %>% schema_to_list(name_attr = "name") 
    output$tree = shinyTree::renderTree(list("plots" = schema_list_plots, "plotObservations" = schema_list_plotObs))
    
    observe({  
      if(!is.null(input$tv_file) & length(names(user_data)) != 0){
        file_selected = input$tv_file # save current selection
        choices = names(user_data)[stringr::str_ends(names(user_data), ".xml")]
        updateSelectizeInput(session, inputId = "tv_file", selected = file_selected, choices = c(dropdown_empty(), choices)) 
        
        if(input$tv_file != ""){
          tv_tables = tv_to_df(user_data[[input$tv_file]])
          tv_dfs(tv_tables)
        } else {
          tv_dfs(NULL)
        }
      }
    })
    
    observeEvent(
      eventExpr = input$submit, 
      handlerExpr = {
        udf_cols = colnames(tv_dfs()$udf_header)
        
        if(length(udf_cols) == 0){
          modal_content = div(class = "text-center text-info", icon("check"), tags$p("This will overwrite the existing VegX document."))
        } else {
          modal_content = tagList(
            div(class = "text-center text-warning", tags$p(paste0("The plot header data of your file contain ", length(udf_cols), " undefined features.\nYou can map them to the corresponding VegX elements below."))),
            shinyTree::shinyTree(ns("tree"), theme = "proton", multiple = T, checkbox = T, themeIcons = F, search = T),
            div(class = "text-center text-info", tags$p("This will overwrite existing the VegX document."))
          )
        }
        
        # Show modal dialog
        showModal(
          modalDialog(tags$h3("Import data"),
                      hr(),
                      modal_content,
                      size = "l",
                      footer =  tagList(
                        tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")),
                                  actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check")))
                      )
          )
        )
      }
    )
    
    observeEvent(
      eventExpr = input$dismiss_modal, 
      handlerExpr = {
        removeModal()
      }
    )
    
    observeEvent(
      eventExpr = input$confirm_import,
      handlerExpr = {
        tryCatch({
          nodes = list()
          mappings = list()
          
          # Project ####
          
          # 
          # if(isTruthy(input$project_title)){
          #   mappings$project[["project > title"]] = list(value = input$project_title, source = "Text")
          # }
          # if(isTruthy(input$project_abstract)){
          #   mappings$project[["project > abstract"]] = list(value = input$project_abstract, source = "Text")
          # }
          # if(isTruthy(input$party_name) & isTruthy(input$party_role)){
          #   mappings$project[["project > personnel > role"]] = list(value = input$party_role, source = "Text")
          # }
          # if(length(mappings$project) != 0){
          #   project_df = build_node_values_df(mappings$project, user_data) 
          #   nodes$projects = list(new_vegx_node(colnames(project_df), project_df[1,], id = NULL, log_path, vegx_schema, write_log = F))  
          # }
          # 
          # if(isTruthy(input$project_citation)){
          #   mappings$literatureCitations[["literatureCitation > citationString"]]  = list(value = input$project_citation, source = "Text")
          #   literatureCitations_df = build_node_values_df(mappings$literatureCitations, user_data)
          #   nodes$literatureCitations = list(new_vegx_node(colnames(literatureCitations_df), literatureCitations_df[1,], id = NULL, log_path, vegx_schema, write_log = F))
          #   link_vegx_nodes(nodes$projects[[1]]$node, "project > documentCitationID", nodes$literatureCitations[[1]]$node, log_path, vegx_schema)
          # }
          # 
          # if(isTruthy(input$party_name)){
          #   mappings$parties[[paste0("party > choice > ", tolower(input$party_type), "Name")]] = list(value = input$party_name, source = "Text")
          #   parties_df = build_node_values_df(mappings$parties, user_data)
          #   nodes$parties = list(new_vegx_node(colnames(parties_df), parties_df[1,], id = NULL, log_path, vegx_schema, write_log = F))
          #   link_vegx_nodes(nodes$projects[[1]]$node, "project > personnel > partyID", nodes$parties[[1]]$node, log_path, vegx_schema)
          # }
          
          #-------------------------------------------------------------------------# 
          # Plots ####
          plots_df = data.frame("plot > plotName" = tv_dfs()$std_header[["releve_nr"]], 
                                "plot > plotUniqueIdentifier" = tv_dfs()$std_header[["releve_nr"]],
                                check.names = F)
          std_cols = colnames(tv_dfs()$std_header) 
          if("surf_area" %in% std_cols){
            method_nodes = templates() %>% dplyr::filter(template_id == 1) %>% templates_to_nodes(vegx_schema, log_path, write_log = F)   # Plot area/m2
            
            plots_df[["plot > geometry > area > value"]] = tv_dfs()$std_header[["surf_area"]]
            plots_df[["plot > geometry > area > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
            
            nodes$methods = append(nodes$methods, method_nodes$methods)
            nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
          }
          
          if("altitude" %in% std_cols){
            method_nodes = templates() %>% dplyr::filter(template_id == 20) %>% templates_to_nodes(vegx_schema, log_path, write_log = F)  # Elevation/m
            
            plots_df[["plot > location > verticalCoordinates > elevation > value"]] = tv_dfs()$std_header[["altitude"]]
            plots_df[["plot > location > verticalCoordinates > elevation > attributeID"]] = xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
            
            nodes$methods = append(nodes$methods, method_nodes$methods)
            nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
          }
          
          if("exposition" %in% std_cols){
            method_nodes = templates() %>% dplyr::filter(template_id == 18) %>% templates_to_nodes(vegx_schema, log_path, write_log = F)  # Aspect/degrees
            
            plots_df[["plot > topography > aspect > value"]] = tv_dfs()$std_header[["exposition"]]
            plots_df[["plot > topography > aspect > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
            
            nodes$methods = append(nodes$methods, method_nodes$methods)
            nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
          }
          
          if("inclinatio" %in% std_cols){
            method_nodes = templates() %>% dplyr::filter(template_id == 16) %>% templates_to_nodes(vegx_schema, log_path, write_log = F)  # Slope/degrees
            
            plots_df[["plot > topography > slope > value"]] = tv_dfs()$std_header[["inclinatio"]]
            plots_df[["plot > topography > slope > attributeID"]] =  xml2::xml_attr(method_nodes$attributes[[1]]$node, "id")
            
            nodes$methods = append(nodes$methods, method_nodes$methods)
            nodes$attributes = append(nodes$attributes, method_nodes$attributes)  
          }
          
          
          # Build plot nodes 
          vegx_schema_plots = xml_find_all(vegx_schema, "./xsd:element[@name='plots']")
          plot_nodes = lapply(1:nrow(plots_df), function(i){
            new_vegx_node(colnames(plots_df), plots_df[i,], id = NULL, log_path, vegx_schema_plots, write_log = F)
          })
          plot_nodes = plot_nodes[which(sapply(plot_nodes, function(x) !is.null(x$node)))] 
          nodes$plots = append(nodes$plots, plot_nodes)
          
          plots_lookup = data.frame(
            plotID = sapply(nodes$plots, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
            plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}) # the mapped unique identifier in the data
          )
          
          #-------------------------------------------------------------------------#
          # plotObservations ####
          plotObs_df = data.frame("plotUniqueIdentifier" = tv_dfs()$std_header[["releve_nr"]],
                                  "plotObservation > obsStartDate" = lubridate::ymd(tv_dfs()$std_header[["date"]]),
                                  "plotObservation > projectID" = 1, 
                                  check.names = F) %>% 
            inner_join(plots_lookup, by = "plotUniqueIdentifier") %>% 
            mutate("plotObservation > plotID" = plotID) %>% 
            dplyr::select(-plotUniqueIdentifier, -plotID)
          
          vegx_schema_plotObs = xml_find_all(vegx_schema, "./xsd:element[@name='plotObservations']")
          plotObs_nodes = lapply(1:nrow(plotObs_df), function(i){
            new_vegx_node(colnames(plotObs_df), plotObs_df[i,], id = NULL, log_path, vegx_schema_plotObs, write_log = F)
          })
          nodes$plotObservations = append(nodes$plotObservations, plotObs_nodes)  
          
          plotObs_lookup = lapply(plotObs_nodes, function(x){
            data.frame(plotObservationID = xml2::xml_attr(x$node, "id"),
                       plotID = xml2::xml_text(xml2::xml_child(x$node, search = "plotID")),
                       obs_date = lubridate::ymd(xml2::xml_text(xml2::xml_child(x$node, search = "obsStartDate"))))}) %>% 
            bind_rows() %>% 
            left_join(plots_lookup, by = "plotID")
          
          
          #-------------------------------------------------------------------------#
          # Update app state ####
          # VegX document 
          for(element_name in names(nodes)){
            element_nodes = nodes[[element_name]]
            parent_missing = (length(xml_find_all(vegx_doc, paste0("./", element_name))) == 0)
            if(parent_missing){
              elements_present = xml_root(vegx_doc) %>% xml_children() %>% xml_name()
              if(length(elements_present) > 0){
                elements_ordered = vegx_main_elements[vegx_main_elements %in% c(elements_present, element_name)]
                insert_position = which(elements_ordered == element_name) - 1
                xml_add_child(vegx_doc, element_name, .where = insert_position)
              } else {
                xml_add_child(vegx_doc, element_name)
              }
            }
            parent = xml_find_all(vegx_doc, paste0("./",  element_name))
            xml_add_child(parent, element_nodes[[1]]$node)
            
            if(length(element_nodes) > 1){
              target = xml_child(parent, 1)
              for(i in 2:length(element_nodes)){
                if(!is.null(element_nodes[[i]]$node)){
                  xml_add_sibling(target, element_nodes[[i]]$node)  # TODO: This is much faster than xml_add_child() but still not ideal
                  target = xml_child(parent, i)
                }
              }
            }
          }
          
          # VegX text 
          vegx_txt(as.character(vegx_doc))
          
          # Action log 
          showNotification("Import finished.")
          new_action_log_record(log_path, "Import info", "Data imported from TurboVeg file")
          action_log(read_action_log(log_path))
        }, error = function(e){
          showNotification("Import failed. Please consult the log for more information.")
          new_action_log_record(log_path, "Import error", e$message)
          action_log(read_action_log(log_path))
        }, finally = {
          removeModal()
        })
      }
    )
  })
}
