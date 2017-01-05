ggplotlyModuleOutput <- function(id) {
  
  ns <- NS(id)
  
  return(
    fluidRow(
      column(12,
             actionButton(ns("button_add_layer"), "Add Layer"),
             tags$hr(),
             tags$div(id = paste0(ns("layer_ui_container"),0)),
             textOutput(ns("test_layer_1")),
             textOutput(ns("test_pt_options")),
             plotlyOutput(ns("plot_main"), height = "600px")
      )
    )
  )
}

ggplotlyModule <- function(input, output, session, rf_data) {
  
  ns <- session$ns
  
  #Layer Data -------------
  geom_choices <- c("geom_bar", "geom_point", "geom_line", "geom_boxplot")
  current_layer <- reactiveValues(layer_count = 0, layer_deleted = 0)
  
  #Layer UI elements ---------------------
  layer_ui <- function(n = 0) {
    tags$div(
      tags$div(
        fluidRow(
          column(2,
                  selectInput(paste0(ns("select_geom"),n), label = NULL, choices = geom_choices)
          ),
          column(2,
                  actionButton(paste0(ns("button_mapping"),n), "Mapping")
                  ),
          column(2,
                 actionButton(paste0(ns("button_remove_layer"),n), "Remove")
          ),
          column(6),
          column(12,
                 tags$hr())
        ),
        id = paste0(ns("layer_ui"), n)),
      id = paste0(ns("layer_ui_container"), n)
    )
  }
  
  observeEvent(input$button_add_layer, {
    
    layer_id <- current_layer$layer_count + 1
    current_layer$layer_count <- layer_id
    
    insertUI(selector = paste0('#',ns("layer_ui_container"), layer_id - 1), where = "afterEnd", ui = layer_ui(layer_id))
    
    observeEvent(input[[paste0("button_mapping", layer_id)]], {
      showModal(modal_mapping_ui)
      current_mapping$layer_id <- layer_id
      if(is_null_empty_na(current_mapping[[paste0("map_count",layer_id)]]) | is_null_empty_na(current_mapping[[paste0("aes", layer_id)]])) {
        current_mapping[[paste0("map_count", layer_id)]] <- 1
        shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), 0)), where = "afterEnd", ui = map_ui(1))
      } else {
        current_mapping[[paste0("map_count", layer_id)]] <- length(current_mapping[[paste0("aes", layer_id)]])
        for(i in 1:length(current_mapping[[paste0("aes", layer_id)]])) {
          shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), i - 1)), 
                          where = "afterEnd", 
                          ui = map_ui(i, 
                                      aes_selected = current_mapping[[paste0("aes", layer_id)]][i], 
                                      attr_selected = current_mapping[[paste0("attr", layer_id)]][i]))
        }
      }  
    })
    
    observeEvent(input[[paste0("button_remove_layer", layer_id)]], {
      removeUI(selector = paste0('#',ns("layer_ui"), layer_id))
      current_layer$layer_deleted <- c(current_layer$layer_deleted, layer_id)
    })
    
  })
  
  #Mapping Modal Data -------
  current_mapping <- shiny::reactiveValues(map_count = 0, map_deleted = 0)
  aes_mappings <- c("x","y","fill","color","group","shape")
  
  
  #Mapping Modal UI elements ------------
  map_ui <- function(n = 0, aes_selected = NULL, attr_selected = NULL) {
    tags$div(
      tags$div(
        fluidRow(
          column(3,
                 selectInput(ns(paste0("select_aes", n)), "Aes", choices = aes_mappings, selected = aes_selected, multiple = FALSE)
          ),
          column(7,
                 selectInput(ns(paste0("select_attr", n)), "Attribute", choices = names(rf_data()), selected = attr_selected, multiple = FALSE, width = "100%")
          ),
          column(2,
                 actionButton(ns(paste0("button_remove_mapping",n)), "Remove", style = "margin-top: 25px;")
          ),
          column(12,
                 tags$hr(id = paste0(ns("map_ui_hr"), n))
          )
        ),
        id = paste0(ns("map_ui"),n)),
      id = paste0(ns("map_ui_container"),n))
  }
  
  modal_mapping_ui <- 
    shiny::modalDialog(
      actionButton(ns("button_add_mapping"), "Add Mapping"),
      tags$div(id = paste0(ns("map_ui_container"),0)),
      footer = shiny::tagList(
        shiny::modalButton("cancel"),
        shiny::actionButton(ns("button_mapping_save"), "", icon = icon("floppy-o"))
      ),
      easyClose = TRUE
    )
  
  
  #Mapping Modal UI events ---------
  # observeEvent(input$button_mapping, {
  #   showModal(modal_mapping_ui)
  #   if(is_null_empty_na(current_mapping$map_count) | is_null_empty_na(current_mapping$aes)) {
  #     current_mapping$map_count <- 1
  #     shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), 0)), where = "afterEnd", ui = map_ui(1))
  #   } else {
  #     current_mapping$map_count <- length(current_mapping$aes)
  #     for(i in 1:length(current_mapping$aes)) {
  #       shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), i - 1)), 
  #                       where = "afterEnd", 
  #                       ui = map_ui(i, 
  #                                   aes_selected = current_mapping$aes[i], 
  #                                   attr_selected = current_mapping$attr[i]))
  #     }
  #   }
  # })
  
  observeEvent(input$button_add_mapping, {
    
    l <- current_mapping$layer_id
    current_mapping[[paste0("map_count",l)]] <- 
      if(is_null_empty_na(current_mapping[[paste0("map_count",l)]])) {
        1
      } else {
        current_mapping[[paste0("map_count",l)]] + 1
      }
    
    map_id <- current_mapping[[paste0("map_count",l)]]
    
    shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), map_id - 1)), where = "afterEnd", ui = map_ui(map_id))
    
    observeEvent(input[[paste0("button_remove_mapping",map_id)]],{
      removeUI(selector = paste0("#", paste0(ns("map_ui"), map_id)))
      current_mapping[[paste0("map_deleted",l)]] <- c(current_mapping[[paste0("map_deleted",l)]], map_id)
    })
    
  })
  
  observeEvent(input$button_mapping_save, {
    
    l <- current_mapping$layer_id
    
    #add new mapping
    aes_c <- character()
    attr_c <- character()
    
    map_count <- 
      if(is_null_empty_na(current_mapping[[paste0("map_count",l)]])) {
        1
      } else {
        current_mapping[[paste0("map_count",l)]]
      }
    
    print(map_count)
    
    for(i in 1:map_count) {
      
      if(!is_null_empty_na(input[[paste0("select_aes", i)]]) & 
         !is_null_empty_na(input[[paste0("select_attr",i)]])) {
        
        if(!(input[[paste0("select_aes", i)]] %in% aes_c) & !(i %in% current_mapping[[paste0("map_deleted",l)]])) {
          aes_c <- c(aes_c, input[[paste0("select_aes", i)]])
          attr_c <- c(attr_c, input[[paste0("select_attr",i)]])
        }
        
      }
    }
    
    current_mapping[[paste0("aes",l)]] <- aes_c
    current_mapping[[paste0("attr",l)]] <- attr_c
    current_mapping[[paste0("map_count",l)]] = length(aes_c)
    current_mapping[[paste0("map_deleted",l)]] = 0
    shiny::removeModal()
  })
  
  gg_options <- reactive({
    geom_list <- list()
    
    # create geoms from layers
    for(i in 1:current_layer$layer_count) {
      if(!(i %in% current_layer$layer_deleted) & 
         !is_null_empty_na(current_mapping[[paste0("aes",i)]])) {
        
        geom_list[[paste0("layer",i)]] <- 
          new_ggplotly_geom(name = input[[paste0("select_geom",i)]],
                            aes_list = name_val_to_list(current_mapping[[paste0("aes",i)]], 
                                                        current_mapping[[paste0("attr",i)]]),
                            geom_list = list("data" = rf_data()))
      }
    }
    
    p_options <- new_ggplotly_options(gg_geom = geom_list)
    
    p_options
  })
  
  output$plot_main <- renderPlotly({
    
    compose_ggplotly(ggplotly_options = gg_options())
    
  })
  
  output$test_layer_1 <- 
    renderText({
      paste0(
        "aes: ", current_mapping[[paste0("aes",1)]],
        " attr: ", current_mapping[[paste0("attr",1)]]
      )
    })
  
  output$test_pt_options <- 
    renderText({
      p <- gg_options()
      g1 <- p$gg_geom[[1]]
      paste0(
        "name: ", g1$name,
        " aes: ", paste0(names(g1$aes_list), collapse = ", "),
        " attr: ", paste0(g1$aes_list, collapse = ", "),
        " geom_names: ", paste0(names(g1$geom_list), collapse = ", ")
    )})
  
}