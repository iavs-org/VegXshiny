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
    fluidRow(
      column(
        width = 10, offset = 1,
        tags$h2 ("TurboVeg Import"),
        tags$p("Select a TurboVeg XML file and read the data into R", class = "text-info annotation"),
        fluidRow(
          column(6, selectizeInput(ns("tv_file"), width = "100%", label = NULL, choices = c("No files found" = ""))),
          column(6, div(style = "display:left-align", actionButton(ns("read_tv"), label = "Read Turboveg XML", style = "height: 35px; line-height: 0px")))
        ),
        
        fluidRow(
          column(12, 
                 tags$label("Document summary"),
                 uiOutput(ns("tv_summary")),
                 hr()
          )
        ),
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
    output$tv_summary = renderText("No summary available.")
    
    dropdown_empty = reactive({
      if(length(names(user_data)) == 0){
        c("No files found" = "")
      } else {
        c("Choose a file" = "")
      }
    })
    
    # Observe and update input instead of using a reactive expression in the definition, thus preventing re-rendering of the entire UI when `user_data()` changes 
    observe({   
      file_selected = input$tv_file # save current selection
      choices = names(user_data)[stringr::str_ends(names(user_data), ".xml")]
      updateSelectizeInput(session, inputId = "tv_file", selected = file_selected, choices = c(dropdown_empty(), choices)) 
    })
    
    # Read TurboVeg XML into tabular format
    observeEvent(
      eventExpr = input$read_tv,
      handlerExpr = {  
        tryCatch({     
          shinyjs::disable("read_tv")
          shinyjs::disable("tv_file")
          
          if(!isTruthy(input$tv_file)){
            showNotification("Please select a file", type = "error")
            return()
          } 
          tv_import = tv_to_df(user_data[[input$tv_file]])
          tv_dfs(tv_import)
          showNotification("Turboveg file read.")
        }, finally = {
          shinyjs::enable("read_tv")
          shinyjs::enable("tv_file")
        })
      }
    )
    
    observeEvent(
      eventExpr = tv_dfs(),
      handlerExpr = {
        if(!isTruthy(tv_dfs())){
          output$tv_summary = renderText("No summary available.")
        } else {
          tv_import = isolate(tv_dfs())
          summary_df = list(
            c("Property" = "Plots", "Summary" = nrow(tv_import$std_header)),
            c("Property" = "Standard header columns", "Summary" = ncol(tv_import$std_header)),
            c("Property" = "Undefined header columns","Summary" = ncol(tv_import$udf_header)),
            c("Property" = "Observations", "Summary" = nrow(tv_import$species)),
            c("Property" = "Unique species names","Summary" = length(unique(tv_import$species$nr))),
            c("Property" = "Lookup tables", "Summary" = paste(names(tv_import$lookup), collapse = ", "))
          ) %>% bind_rows()
          output$tv_summary = renderTable(summary_df, colnames = F)
        }
      }
    )
    
    observeEvent(
      eventExpr = input$submit, 
      handlerExpr = {
        # Build Modal UI elements
        if(isTruthy(tv_dfs())){
          modal_content = tagList(
            div(class = "text-center text-info", icon("check"), 
                tags$p("This will convert the uploaded document into VegX document. Depending on the document size, this process may take a while."),
            ),
            tags$p(paste0("The header data contain ", ncol(tv_dfs()$udf_header), " undefined columns. Which fields should be imported as user defined plot attributes?")),
            checkboxGroupInput(ns("udf_header_import"), label = NULL, choices = colnames(tv_dfs()$udf_header)[-1], inline = T)
          )
          modal_footer = tagList(
            tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                      actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check")))
          )
        } else {
          modal_content = div(class = "text-center text-danger", icon("exclamation"), tags$p("Submission incomplete. Please read in a TurboVeg XML file."))
          modal_footer = tagList(tags$span(actionButton(ns("dismiss_modal"), "Dismiss", class = "pull-left btn-danger", icon = icon("times")), 
                                           shinyjs::disabled(actionButton(ns("confirm_import"), class = "pull-right btn-success", "Confirm", icon("check"))))
          )
        }
        
        # Show modal dialog
        showModal(
          modalDialog(tags$h3("Import data"),
                      hr(),
                      modal_content,
                      size = "l",
                      footer = modal_footer
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
          withProgress(
            message = "Importing data",
            expr = {
              # Preparations 
              shinyjs::disable("confirm_import")
              shinyjs::disable("dismiss_modal")
              nodes = list()
              mappings = list()
              
              #-------------------------------------------------------------------------# 
              # Project ####
              setProgress(value = 0.05, "Projects")
              if(isTruthy(input$project_title)){
                mappings$project[["project > title"]] = list(value = input$project_title, source = "Text")
                project_df = build_node_values_df(mappings$project, user_data)
                nodes$projects = list(new_vegx_node(colnames(project_df), project_df[1,], id = NULL, log_path, vegx_schema, write_log = F))
              }
              
              #-------------------------------------------------------------------------# 
              # Plots ####
              setProgress(value = 0.2, "Plots")
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
              
              # Undefined header 
              udf_method_nodes = lapply(input$udf_header_import, function(name){
                new_vegx_node(node_paths = c("method > subject", "method > name", "method > description"),
                              node_values = c("Turboveg udf_header", name, "TurboVeg undefined method"),
                              id = NULL, log_path, vegx_schema, write_log = T)
              })
              
              nodes$methods = append(nodes$methods, udf_method_nodes)
              
              udf_methods_lookup = data.frame(
                methodID = sapply(udf_method_nodes, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
                methodName = sapply(udf_method_nodes, function(x){xml_text(xml_find_first(x$node, "//name"))}) # the mapped unique identifier in the data
              )
              
              # Build plot nodes 
              vegx_schema_plots = xml_find_all(vegx_schema, "./xsd:element[@name='plots']")
              plot_nodes = lapply(1:nrow(plots_df), function(i){
                plot_node = new_vegx_node(colnames(plots_df), plots_df[i,], id = NULL, log_path, vegx_schema_plots, write_log = F)
                lapply(1:nrow(udf_methods_lookup), function(j){
                  method_name = udf_methods_lookup$methodName[j]
                  method_id = udf_methods_lookup$methodID[j]
                  simpleUserDef_node = xml_new_root("simpleUserDefined")
                  xml_add_child(simpleUserDef_node, "name", method_name)
                  xml_add_child(simpleUserDef_node, "value", tv_dfs()$udf_header[[method_name]][i])
                  xml_add_child(simpleUserDef_node, "choice")
                  xml_child(simpleUserDef_node, "choice") %>% 
                    xml_add_child("methodID", method_id)
                  
                  xml_add_child(plot_node$node, simpleUserDef_node)
                })
                return(plot_node)
              })
              plot_nodes = plot_nodes[which(sapply(plot_nodes, function(x) !is.null(x$node)))] 
              nodes$plots = append(nodes$plots, plot_nodes)
              
              plots_lookup = data.frame(
                plotID = sapply(nodes$plots, function(x){xml_attr(x$node, "id")}), # The internal id used by vegXshiny
                plotUniqueIdentifier = sapply(nodes$plots, function(x){xml_text(xml_find_first(x$node, "//plotUniqueIdentifier"))}) # the mapped unique identifier in the data
              )
              
              #-------------------------------------------------------------------------#
              # Organism names ####
              setProgress(value = 0.3, "Organisms")
              vegx_schema_orgNames = xml_find_all(vegx_schema, "./xsd:element[@name='organismNames']")
              orgNames_df = tv_dfs()$lookup$Species_list[[1]]$records %>% 
                mutate(taxonName = ifelse(valid_name == "", "true", "false"),
                       valid_name = ifelse(valid_name == "", name, valid_name))
              
              orgNames_nodes = lapply(1:nrow(orgNames_df), function(i){
                node = new_vegx_node("organismName", orgNames_df[i,"name"], id = NULL, log_path, vegx_schema_orgNames, write_log = F)
                xml_set_attr(node$node, "taxonName", orgNames_df[i,"taxonName"])
                return(node)
              })
              
              orgNames_df$orgNameID = sapply(orgNames_nodes, function(x){xml_attr(x$node, "id")})
              orgNames_df$preferredNameID = orgNames_df[match(orgNames_df$valid_name, orgNames_df$name), "orgNameID"]
              orgNames_df = orgNames_df %>% 
                mutate(preferredNameID = ifelse(preferredNameID == orgNameID, "", preferredNameID))
              
              vegx_schema_orgIdentities = xml_find_all(vegx_schema, "./xsd:element[@name='organismIdentities']")
              orgIdentities_df = data.frame("organismIdentity > originalOrganismNameID" = orgNames_df$orgNameID,
                                            "organismIdentity > preferredTaxonNomenclature > preferredTaxonNameID" = orgNames_df$preferredNameID,
                                            check.names = F)
              
              orgIdentities_nodes = lapply(1:nrow(orgIdentities_df), function(i){
                new_vegx_node(colnames(orgIdentities_df), orgIdentities_df[i,], id = NULL, log_path, vegx_schema_orgIdentities, write_log = F)
              })
              
              nodes$organismNames = orgNames_nodes
              nodes$organismIdentities = orgIdentities_nodes
              
              # Lookup
              orgNames_lookup = data.frame(organismName = orgNames_df$nr,
                                           originalOrganismNameID = ifelse(orgNames_df$preferredNameID == "", orgNames_df$orgNameID, orgNames_df$preferredNameID))
              
              orgIdentities_lookup = lapply(orgIdentities_nodes, function(x){
                data.frame(organismIdentityID = xml2::xml_attr(x$node, "id"),
                           originalOrganismNameID = xml2::xml_text(xml2::xml_child(x$node, search = "originalOrganismNameID")))}) %>% 
                bind_rows()
              
              organisms_lookup = left_join(orgIdentities_lookup, orgNames_lookup, by = "originalOrganismNameID")
              
              #-------------------------------------------------------------------------# 
              # Cover Scale #####
              setProgress(value = 0.35, "Cover scale definitions")
              coverscales = tv_dfs()$lookup$Coverscale_list
              new_template_id = id_generator()
              coverscale_templates = lapply(coverscales, function(coverscale){
                template_id = new_template_id()
                new_node_id = id_generator()
                method_df = data.frame("template_id" = template_id,
                                       "node_id" = new_node_id(),
                                       "main_element" = "methods",
                                       "node_path" = c("method > subject", "method > name", "method > description"),
                                       "node_value" = c("plant cover", coverscale$description, paste0("TurboVeg cover scale (Code: ", coverscale$code, ")")))
                
                if(coverscale$code == "00"){            # The only quantitative cover scale in turboveg
                  attributes_df = data.frame("template_id" = template_id,
                                             "node_id" = new_node_id(),
                                             "main_element" = "attributes",
                                             "node_path" = c("attribute > choice > quantitative > unit", "attribute > choice > quantitative > lowerLimit", "attribute > choice > quantitative > upperLimit", "attribute > choice > quantitative > methodID"),
                                             "node_value" = c("%", 0, 100, 1))
                } else {
                  attributes_df = lapply(1:nrow(coverscale$records), function(i){
                    data.frame("template_id" = template_id,
                               "node_id" = new_node_id(),
                               "main_element" = "attributes",
                               "node_path" = c("attribute > choice > ordinal > code", "attribute > choice > ordinal > definition",  "attribute > choice > ordinal > methodID"),
                               "node_value" = c(coverscale$records$code[i] , paste0("Approximately ", coverscale$records$percentage[i], " % cover"), 1))
                  }) %>% bind_rows()
                }
                
                template = bind_rows(method_df, attributes_df)
                return(template)
              })
              template_nodes = templates_to_nodes(bind_rows(coverscale_templates), vegx_schema, log_path, write_log = F) 
              
              nodes$methods = append(nodes$methods, template_nodes$methods)
              nodes$attributes = append(nodes$attributes, template_nodes$attributes) 
              
              measurementScale_lookup = lapply(template_nodes$attributes, function(x){
                data.frame(attributeID = xml2::xml_attr(x$node, "id"),
                           taxon_measurement = xml2::xml_text(xml2::xml_find_first(x$node, "..//code")))}) %>% 
                bind_rows()
              
              #-------------------------------------------------------------------------# 
              # Strata definition #####
              setProgress(value = 0.4, "Layer definitions")
              stratadef_template_id = templates_lookup() %>% 
                dplyr::filter(name == "Strata definition/Turboveg") %>% 
                pull(template_id) %>% 
                unique()
              
              stratadef_template = templates() %>% 
                dplyr::filter(template_id == stratadef_template_id)
              
              template_nodes = templates_to_nodes(stratadef_template, vegx_schema, log_path, write_log = F)
              nodes$strata = append(nodes$strata, template_nodes$strata)
              nodes$methods = append(nodes$methods, template_nodes$methods)
              nodes$attributes = append(nodes$attributes, template_nodes$attributes) 
              
              strata_lookup = lapply(nodes$strata, function(x){
                data.frame(stratumID = xml2::xml_attr(x$node, "id"),
                           stratumName = xml2::xml_text(xml2::xml_child(x$node, search = "stratumName")))}) %>% 
                bind_rows()
              
              #-------------------------------------------------------------------------#
              # PlotObservations ####
              setProgress(value = 0.45, "Plot observations")
              plotObs_df = data.frame("plotUniqueIdentifier" = tv_dfs()$std_header[["releve_nr"]],
                                      "plotObservation > obsStartDate" = tv_dfs()$std_header[["date"]],
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
                           obs_date = xml2::xml_text(xml2::xml_child(x$node, search = "obsStartDate")))}) %>% 
                bind_rows() %>% 
                left_join(plots_lookup, by = "plotID")
              
              #-------------------------------------------------------------------------#
              # StratumObservations ####
              # Build mapping table
              setProgress(value = 0.5, "Stratum observations")
              stratumObs_df = data.frame(
                plotUniqueIdentifier = tv_dfs()$species$releve_nr,
                stratumName = tv_dfs()$species$layer
              ) %>%  
                filter(stratumName != 0) %>%    # Stratum 0 is defined as no strata in Turboveg
                left_join(plotObs_lookup, by = c("plotUniqueIdentifier")) %>% 
                left_join(strata_lookup, by = "stratumName") %>% 
                dplyr::select("stratumObservation > plotObservationID" = plotObservationID, 
                              "stratumObservation > stratumID" = stratumID) %>% 
                distinct() %>% 
                arrange("stratumObservation > plotObservationID", "stratumObservation > stratumID")
              
              if(nrow(stratumObs_df) > 0){
                # Create nodes
                vegx_schema_stratumObs = xml_find_all(vegx_schema, "./xsd:element[@name='stratumObservations']")
                stratumObs_nodes = lapply(1:nrow(stratumObs_df), function(i){
                  new_vegx_node(colnames(stratumObs_df), stratumObs_df[i,], id = NULL, log_path, vegx_schema_stratumObs, write_log = F)
                })
                nodes$stratumObservations = append(nodes$stratumObservations, stratumObs_nodes)  
                
                # Build lookup table
                stratumObs_lookup = lapply(nodes$stratumObservations, function(x){
                  data.frame(stratumObservationID = xml2::xml_attr(x$node, "id"),
                             plotObservationID = xml2::xml_text(xml2::xml_child(x$node, search = "plotObservationID")),
                             stratumID = xml2::xml_text(xml2::xml_child(x$node, search = "stratumID")))}) %>% 
                  bind_rows()
              } else {
                stratumObs_lookup = data.frame(stratumObservationID = character(0),
                                               plotObservationID = character(0),
                                               stratumID = character(0))
              }
              
              #-------------------------------------------------------------------------#
              # AggregateOrganismObservations ####
              setProgress(value = 0.6, "Organism observations")
              aggOrgObs_mappings = tv_dfs()$species %>% 
                rename(plotUniqueIdentifier = releve_nr,
                       organismName = nr,
                       stratumName = layer,
                       taxon_measurement = cover)
              
              
              aggOrgObs_df = aggOrgObs_mappings %>% 
                left_join(plotObs_lookup, by = c("plotUniqueIdentifier")) %>% 
                left_join(organisms_lookup, by = "organismName") %>% 
                left_join(measurementScale_lookup, by = "taxon_measurement") %>%
                left_join(strata_lookup, by = "stratumName") %>% 
                left_join(stratumObs_lookup, by = c("stratumID", "plotObservationID")) %>% 
                dplyr::select("aggregateOrganismObservation > plotObservationID" = plotObservationID, 
                              "aggregateOrganismObservation > organismIdentityID" = organismIdentityID, 
                              "aggregateOrganismObservation > aggregateOrganismMeasurement > value" = taxon_measurement, 
                              "aggregateOrganismObservation > aggregateOrganismMeasurement > attributeID" = attributeID,
                              "aggregateOrganismObservation > stratumObservationID" = stratumObservationID)
              
              
              vegx_schema_aggOrgObs = xml_find_all(vegx_schema, "./xsd:element[@name='aggregateOrganismObservations']")
              aggOrgObs_nodes = lapply(1:nrow(aggOrgObs_df), function(i){
                new_vegx_node(colnames(aggOrgObs_df), aggOrgObs_df[i,], id = NULL, log_path, vegx_schema_aggOrgObs, write_log = F)
              })
              nodes$aggregateOrganismObservations = aggOrgObs_nodes  
              
              
              #-------------------------------------------------------------------------#
              # Update app state ####
              # VegX document 
              setProgress(value = 0.8, "Updating VegX document")
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
                xml_add_child(parent, "placeholder")
                placeholder = xml_child(parent, "placeholder")
                
                for(i in 1:length(element_nodes)){
                  if(!is.null(element_nodes[[i]]$node)){
                    xml_add_sibling(placeholder, element_nodes[[i]]$node, .where = "before", .copy = F)  # This is much faster than xml_add_child()
                  }
                }
                xml_remove(placeholder)  # Remove placeholder
              }
              
              # VegX text 
              vegx_txt(as.character(vegx_doc))
              
              # Action log 
              setProgress(value = 1)
              showNotification("Import finished.")
              new_action_log_record(log_path, "Import info", "Data imported from TurboVeg file")
              action_log(read_action_log(log_path))
            })  
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