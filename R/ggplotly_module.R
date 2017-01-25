ggplotlyModuleOutput <- function(id) {
  
  ns <- NS(id)
  
  return(
    fluidRow(
      column(12,
             actionButton(ns("button_add_layer"), "Add Layer"),
             actionButton(ns("button_theme"), "Change Theme"),
             actionButton(ns("button_title"), "Change Title"),
             tags$hr(),
             tags$div(id = paste0(ns("layer_ui_container"),0)),
             textOutput(ns("test_layer_1")),
             textOutput(ns("test_pt_options")),
             plotlyOutput(ns("plot_main"), height = "600px")
      )
    )
  )
}

#Mapping Modal UI elements ------------
ggplotly_module_mapping_row_ui <- function(n = 0, aes_choices, attr_choices, ns, aes_selected = NULL, attr_selected = NULL, is_required = FALSE) {
  
  aes_choices_restricted <- 
    if(is_required) {
      aes_selected
    } else {
      aes_choices
    }
  
  button_label <- 
    if(is_required) {
      "Required"
    } else {
      "Remove"
    }
  
  button_id <-
    if(is_required) {
      "required_ignore"
    } else {
      "button_remove_mapping"
    }
  
  tags$div(
    tags$div(
      fluidRow(
        column(3,
               selectInput(ns(paste0("select_aes", n)), "Aes", choices = aes_choices_restricted, selected = aes_selected, multiple = FALSE)
        ),
        column(7,
               selectInput(ns(paste0("select_attr", n)), "Attribute", choices = attr_choices, selected = attr_selected, multiple = FALSE, width = "100%")
        ),
        column(2,
               actionButton(ns(paste0(button_id,n)), button_label, style = "margin-top: 25px;")
        ),
        column(12,
               tags$hr(id = paste0(ns("map_ui_hr"), n))
        )
      ),
      id = paste0(ns("map_ui"),n)),
    id = paste0(ns("map_ui_container"),n))
}

ggplotly_module_layer_row_ui <- function(n = 0, geom_choices, ns) {
  
    tags$div(
      tags$div(
        fluidRow(
          column(2,
                 selectInput(paste0(ns("select_geom"),n), label = NULL, choices = geom_choices)
          ),
          column(6,
                 miniUI::miniButtonBlock(
                   actionButton(paste0(ns("button_mapping"),n), "Mapping"),
                   actionButton(paste0(ns("button_param"),n), "Param"),
                   actionButton(paste0(ns("button_remove_layer"),n), "Remove")
                 )),
          column(4),
          column(12,
                 tags$hr())
        ),
        id = paste0(ns("layer_ui"), n)),
      id = paste0(ns("layer_ui_container"), n)
    )
  
}


ggplotly_module_modal_mapping_ui <- function(ns) {
  shiny::modalDialog(
    actionButton(ns("button_add_mapping"), "Add Mapping"),
    tags$div(id = paste0(ns("map_ui_container"),0)),
    footer = shiny::tagList(
      shiny::modalButton("cancel"),
      shiny::actionButton(ns("button_mapping_save"), "", icon = icon("floppy-o"))
    ),
    easyClose = TRUE
  )
}


ggplotly_module_param_dialog <- function(ns, param_list, layer_id) {
  
  ggplotly_module_layer_param_widget <- function(param, param_name) {
    
    id <- paste0(ns(dot_to_underscore(param_name)), layer_id)
    selected <- if(is_null_empty_na(param$selected)) {
      NULL
    } else {
      param$selected
    }
    
    ui <-
      if(param$type == "numeric") {
        if(is_null_empty_na(param$choices)) {
          shiny::numericInput(id, label = param_name, value = selected)
        } else {
          shiny::numericInput(id, label = param_name, value = selected, min = min(param$choices), max = max(param$choices))
        }
      } else if (param$type == "integer") {
        if(is_null_empty_na(param$choices)) {
          shiny::numericInput(id, label = param_name, value = selected, step = 1)
        } else {
          shiny::numericInput(id, label = param_name, value = selected, min = min(param$choices), max = max(param$choices), step = 1)
        }
        
      } else if (param$type == "character") {
        shiny::selectizeInput(id, label = param_name, choices = param$choices, selected = selected, multiple = TRUE, options = list(maxItems = 1))
      } else if (param$type == "logical") {
        shiny::checkboxInput(id, param_name, value = selected)
      } else if (param$type == "formula") {
        shiny::textInput(id, label = param_name, value = param$selected)
      } else {
        tags$div()
      }
    
    return(ui)
  
  }
  
  ui <- list()
  for(i in 1:length(param_list)) {
    ui[[i]] <- ggplotly_module_layer_param_widget(param_list[[i]], names(param_list)[i])
  }

  modalDialog(
    fluidRow(column(12,ui)),
    footer = shiny::tagList(
      shiny::modalButton("cancel"),
      shiny::actionButton(ns("button_param_save"), "", icon = icon("floppy-o"))
    ),
    easyClose = TRUE
  )
  
}

ggplotly_title_modal <- function(ns) {
  modal_ui <-
    fluidRow(
      column(12,
             actionButton(ns("button_title_add"), "Add")
      ),
      column(12,
             tags$hr()
             ),
      column(10,
             fluidRow(
               column(4,
                      tags$div("Title (ggtitle)")
               ),
               column(8,
                      textInput(ns("text_ggtitle"), label = NULL)
               ),
               column(4,
                      tags$div("x-axis (xlab)")
               ),
               column(8,
                      textInput(ns("text_xlab"), label = NULL)
               ),
               column(4,
                      tags$div("y-axis (ylab)")
               ),
               column(8,
                      textInput(ns("text_ylab"), label = NULL)
               )
             )
      ),
      column(2),
      column(12,
             tags$div(id = paste0(ns("title_ui_container"), 0))
      )
    )
  
  modalDialog(
    modal_ui,
    footer = shiny::tagList(
      shiny::modalButton("cancel"),
      shiny::actionButton(ns("button_title_save"), "", icon = icon("floppy-o"))
    ),
    easyClose = TRUE
  )
}


title_ui_row_func <- function(ns, i, current_title) {
  
  title_choices <- c("color", "fill", "shape", "size", "linetype", "weight", "alpha", "subtitle", "caption")
  title_choices <-title_choices[order(title_choices)]

  title_element_selected <- current_title[[paste0("title_element",i)]]
  
  tags$div(
    tags$div(
      fluidRow(
        column(10,
               fluidRow(
                 column(4,
                        selectizeInput(paste0(ns("select_title_element"),i), 
                                       label = NULL, 
                                       selected = title_element_selected, 
                                       choices = title_choices, 
                                       multiple = TRUE, 
                                       options = list(maxItems = 1))
                 ),
                 column(8,
                        textInput(paste0(ns("text_title_element_value"), i), label = NULL)
                 )
               )
        ),
        column(2,
               actionButton(paste0(ns("button_remove_title"), i), "Remove")
        )      
      ),
      id = paste0(ns("title_ui"), i)),
    id = paste0(ns("title_ui_container"), i)
  )
}

ggplotly_theme_modal <- function(ns) {
  
  theme_choices <- 
    c("theme_base",
      "theme_calc",
      "theme_economist",
      "theme_excel",
      "theme_few",
      "theme_fivethirtyeight",
      "theme_gdocs",
      "theme_hc",
      "theme_par",
      "theme_pander",
      "theme_solarized",
      "theme_stata",
      "theme_tufte",
      "theme_wsj",
      "theme_gray",
      "theme_bw",
      "theme_linedraw",
      "theme_light",
      "theme_dark",
      "theme_minimal",
      "theme_classic",
      "theme_void")
  theme_choices <- theme_choices[order(theme_choices)]

  modal_ui <- 
    fluidRow(
      column(6,
        selectInput(ns("select_theme"), label = NULL, choices = theme_choices)
      ),
      column(6,
             actionButton(ns("button_theme_add"), "Add ")
      ),
      column(12,
             tags$div(id = paste0(ns("theme_ui_container"), 0)) 
      ),
      column(12,
             textOutput(ns("theme_test_id")))
  )
  
  modalDialog(
    modal_ui,
    footer = shiny::tagList(
      shiny::modalButton("cancel"),
      shiny::actionButton(ns("button_theme_save"), "", icon = icon("floppy-o"))
    ),
    easyClose = TRUE
  )
  
}


ggplotly_theme_ui_widget <- function(param, ns, theme_id) {
  
  print("theme widget")
  
  id <- paste0(ns("select_theme_attribute_value_o"), theme_id)
  selected <- if(is_null_empty_na(param$selected)) {
    NULL
  } else {
    param$selected
  }
  
  ui <-
    if(param$type == "numeric") {
      if(is_null_empty_na(param$choices)) {
        shiny::numericInput(id, label = NULL, value = selected)
      } else {
        shiny::numericInput(id, label = NULL, value = selected, min = min(param$choices), max = max(param$choices))
      }
    } else if (param$type == "integer") {
      if(is_null_empty_na(param$choices)) {
        shiny::numericInput(id, label = NULL, value = selected, step = 1)
      } else {
        shiny::numericInput(id, label = NULL, value = selected, min = min(param$choices), max = max(param$choices), step = 1)
      }
      
    } else if (param$type == "character") {
      shiny::selectizeInput(id, label = NULL, choices = param$choices, selected = selected, multiple = TRUE, options = list(maxItems = 1))
    } else if (param$type == "logical") {
      shiny::checkboxInput(id, NULL, value = selected)
    } else if (param$type == "formula") {
      shiny::textInput(id, label = NULL, value = param$selected)
    } else if (param$type == "text") {
      shiny::textInput(id, label = NULL, value = param$selected)
    } else {
      tags$div()
    }
  
  return(ui)
  
}


insert_ui_row_by_row <- function(ns, ui_row_func, selector_func, rows = 0, where = "afterEnd") {
  
  if (rows > 0) {
    for(i in 1:rows) {
      insertUI(
        selector = selector_func(i), 
        where = where,
        ui = ui_row_func(ns, i)
      ) 
    }
  }
  
}

theme_ui_row_func <- function(ns, i, current_theme) {
  
  element_choices <- names(theme_mapping())
  
  element_selected <- 
    if(is_null_empty_na(current_theme[[paste0("theme_element", i)]], test_blank = TRUE)) {
      NULL
    } else {
      current_theme[[paste0("theme_element", i)]]
    }
  
  tags$div(
    tags$div(
      fluidRow(
        column(10,
               fluidRow(
                 column(4,
                        selectizeInput(paste0(ns("select_theme_element"),i), 
                                       label = NULL, 
                                       selected = element_selected, 
                                       choices = element_choices, 
                                       multiple = TRUE, 
                                       options = list(maxItems = 1))
                 ),
                 column(4,
                        uiOutput(paste0(ns("select_theme_attribute"), i))
                 ),
                 column(4,
                        uiOutput(paste0(ns("select_theme_attribute_value"), i))
                 )
               )
        ),
        column(2,
               actionButton(paste0(ns("button_remove_theme"), i), "Remove")
        ),
        column(12,
               tags$hr()
        )
      ),
      id = paste0(ns("theme_ui"), i)),
    id = paste0(ns("theme_ui_container"), i)
  )
}

ggplotlyModule <- function(input, output, session, rf_data) {
  
  ns <- session$ns
  
  attr_choices = reactive({
    names(rf_data())
  })
  
  # Title Data --------
  current_title <- reactiveValues(title_id = 0, title_deleted = 0)
  
  # Title UI elements ------------------
  observeEvent(input$button_title, {
    
    showModal(ggplotly_title_modal(ns))
    
    insert_ui_row_by_row(ns,
                         function(ns, i) title_ui_row_func(ns, i, current_title),
                         function(i) paste0('#',ns("title_ui_container"), i - 1),
                         current_title$title_id)
    
  })
  
  observeEvent(input$button_title_add, {
    
    # increment theme_id
    title_id <- isolate(current_title$title_id + 1)
    isolate(current_title$title_id <- title_id)
    
    # add ui row
    insertUI(selector = paste0('#',ns("title_ui_container"), title_id - 1),
             where = "afterEnd",
             ui = title_ui_row_func(ns, title_id, current_title))
    
    # add observers
    observeEvent(input[[paste0("button_remove_title", title_id)]], {
      removeUI(selector = paste0('#',ns("title_ui"), title_id))
      current_title$title_deleted <- c(current_title$title_deleted, title_id)
    })
    
  })
  
  observeEvent(input$button_title_save, {
    
    if(!is_null_empty_na(input$select_ggtitle)) {
      current_title$ggtitle <- input$select_ggtitle  
    }
    
    if(!is_null_empty_na(input$select_xlab)) {
      current_title$xlab <- input$select_xlab  
    }
    
    if(!is_null_empty_na(input$select_ylab)) {
      current_title$ylab <- input$select_ylab  
    }
    
    if(!is_null_empty_na(current_title$title_id) && current_title$title_id > 0) {
      for (i in 1:current_title$title_id) {
        if(!(i %in% current_title$title_deleted)) {
          current_title[[paste0("title_element", i)]] <- input[[paste0("select_title_element",i)]]
          current_title[[paste0("title_element_value", i)]] <- input[[paste0("select_title_element_value",i)]]
        } else {
          current_title[[paste0("title_element", i)]] <- NULL
          current_title[[paste0("title_element_value", i)]] <- NULL
        }
      }
    }
    
    removeModal()
    
  })
  
  
  #Theme Data --------------
  current_theme <- reactiveValues(theme_id = 0, theme_deleted = 0)
  
  #Theme UI elements --------------
  
  #button_theme -----------------------------------
  observeEvent(input$button_theme, {
    
    #show mdoal
    showModal(ggplotly_theme_modal(ns))
    
    updateSelectInput(session, "select_theme", selected = current_theme$ggtheme)
    
    #insert ui from current_theme
    insert_ui_row_by_row(ns, 
                         ui_row_func =  function(ns, i) theme_ui_row_func(ns = ns, i = i, current_theme = current_theme),
                         selector_func = function(i) paste0('#',ns("theme_ui_container"), i - 1), 
                         rows = current_theme$theme_id)
    
  }) # end of button_theme
  
  # button_theme_add -----------------------------
  observeEvent(input$button_theme_add, {
    
    # increment theme_id
    theme_id <- isolate(current_theme$theme_id + 1)
    isolate(current_theme$theme_id <- theme_id)
    
    # add ui row
    insertUI(selector = paste0('#',ns("theme_ui_container"), theme_id - 1),
             where = "afterEnd",
             ui = theme_ui_row_func(ns, theme_id, current_theme))
    
    
    # add observers
    observeEvent(input[[paste0("select_theme_element", theme_id)]], {
      output[[paste0("select_theme_attribute", theme_id)]] <- 
        renderUI({
          print("render theme element")
            selectizeInput(paste0(ns("select_theme_attribute_o"), theme_id), 
                           label = NULL,
                           selected = input[[paste0("select_theme_attribute_o", theme_id)]],
                           choices = names(theme_mapping(isolate(input[[paste0("select_theme_element", theme_id)]]))$attributes),
                           multiple = TRUE,
                           options = list(maxItems = 1))
        })
    })
    
#    observeEvent(input[[paste0("select_theme_attribute_0")]], {
#      current_theme[[paste0]]
#    })
    
    observeEvent(input[[paste0("select_theme_attribute_o", theme_id)]], {

      output[[paste0("select_theme_attribute_value", theme_id)]] <-
        renderUI({
          param <- theme_mapping(isolate(input[[paste0("select_theme_element", theme_id)]]))$attributes[[isolate(input[[paste0("select_theme_attribute_o", theme_id)]])]]
          param$selected <- input[[paste0("select_theme_attribute_value_o", theme_id)]]
          print("render attribute value")
            ggplotly_theme_ui_widget(
              param,
              ns,
              theme_id)
        })
    })
    
    observeEvent(input[[paste0("button_remove_theme", theme_id)]], {
      removeUI(selector = paste0('#',ns("theme_ui"), theme_id))
      current_theme$theme_deleted <- c(current_theme$theme_deleted, theme_id)
    })
    
    
    
  })
  
  observeEvent(input$button_theme_save, {
    
    if(!is_null_empty_na(input$select_theme)) {
      current_theme$ggtheme <- input$select_theme  
    }
    
    if(!is_null_empty_na(current_theme$theme_id) && current_theme$theme_id > 0) {
      for (i in 1:current_theme$theme_id) {
        if(!(i %in% current_theme$theme_deleted)) {
          current_theme[[paste0("theme_element", i)]] <- input[[paste0("select_theme_element",i)]]
          current_theme[[paste0("theme_attribute", i)]] <- input[[paste0("select_theme_attribute_o",i)]]
          current_theme[[paste0("theme_attribute_value", i)]] <- input[[paste0("select_theme_attribute_value_o",i)]]
        } else {
          current_theme[[paste0("theme_element", i)]] <- NULL
          current_theme[[paste0("theme_attribute", i)]] <- NULL
          current_theme[[paste0("theme_attribute_value", i)]] <- NULL
        }
      }
    }

    removeModal()
    
  })
  
  output$theme_test_id <- renderText({
    paste0(
      " select_theme_attribute_o1: ", input$select_theme_attribute_o1, "\n",
      " select_theme_attribute_o2: ", input$select_theme_attribute_o2, "\n",
      " select_theme_attribute_o3: ", input$select_theme_attribute_o3, "\n",
      " select_theme_attribute_o4: ", input$select_theme_attribute_o4
    )
  })
  
  
  #Layer Data -------------
  geom_choices <- names(geom_mapping())
  current_layer <- reactiveValues(layer_count = 0, layer_deleted = 0)
  current_param <- reactiveValues()
  
  #Layer UI elements ---------------------
  observeEvent(input$button_add_layer, {
    
    #Set layer_id of new layer
    layer_id <- current_layer$layer_count + 1
    current_layer$layer_count <- layer_id
    
    #insert layer UI
    insertUI(selector = paste0('#',ns("layer_ui_container"), layer_id - 1), 
             where = "afterEnd", 
             ui = ggplotly_module_layer_row_ui(layer_id, geom_choices, ns))
    
    #set current mapping layer id
    current_mapping$layer_id <- layer_id
    
    #set current_param layer id
    current_param$layer_id <- layer_id

    #Create observe event for new button
    observeEvent(input[[paste0("button_mapping", layer_id)]], {
      
      aes_required <- geom_mapping(input[[paste0("select_geom", layer_id)]])$aes_required
      current_mapping$layer_id <- layer_id
      
      showModal(ggplotly_module_modal_mapping_ui(ns))
      
      if(is_null_empty_na(current_mapping[[paste0("map_count",layer_id)]]) | is_null_empty_na(current_mapping[[paste0("aes", layer_id)]])) {
        current_mapping[[paste0("map_count", layer_id)]] <- 1
        shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), 0)), 
                        where = "afterEnd", 
                        ui = ggplotly_module_mapping_row_ui(1, 
                                                           aes_mappings, 
                                                           attr_choices(),
                                                           ns = ns))
      } else {
        current_mapping[[paste0("map_count", layer_id)]] <- length(current_mapping[[paste0("aes", layer_id)]])
        for(i in 1:length(current_mapping[[paste0("aes", layer_id)]])) {
          is_required <- current_mapping[[paste0("aes", layer_id)]][i] %in% aes_required
          shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), i - 1)), 
                          where = "afterEnd", 
                          ui = ggplotly_module_mapping_row_ui(i, 
                                                              aes_mappings, 
                                                              attr_choices(),
                                                              ns = ns, 
                                                              aes_selected = current_mapping[[paste0("aes", layer_id)]][i], 
                                                              attr_selected = current_mapping[[paste0("attr", layer_id)]][i],
                                                              is_required = is_required))
        }
      }  
    })
    
    #Create observe event for param button
    observeEvent(input[[paste0("button_param", layer_id)]], {
      
      current_param$layer_id <- layer_id
      
      #identify layer geom
      geom_name <- input[[paste0("select_geom", layer_id)]]
      geom_params <- geom_mapping(geom_name)$params
      
      if(is_null_empty_na(geom_params)) return(NULL)
      
      #update selected values if current_param exists for this layer and geom
      for (i in 1:length(geom_params)) {
        if(!is_null_empty_na(current_param[[paste0(names(geom_params)[i], layer_id)]])) {
          geom_params[[i]]$selected <- current_param[[paste0(names(geom_params)[i], layer_id)]]
        }
      }
      
      #show param modal
      showModal(
        ggplotly_module_param_dialog(ns, geom_params, layer_id)
      )
    })
    
    #Create observe event for new button
    observeEvent(input[[paste0("button_remove_layer", layer_id)]], {
      removeUI(selector = paste0('#',ns("layer_ui"), layer_id))
      current_layer$layer_deleted <- c(current_layer$layer_deleted, layer_id)
    })
    
    #set default mapping on change of input_geom
    observeEvent(input[[paste0("select_geom",layer_id)]], {
      default_mappings <- geom_default_mapping(input[[paste0("select_geom",layer_id)]], current_mapping[[paste0("attr",layer_id)]])
      current_mapping[[paste0("aes",layer_id)]] <- default_mappings$aes
      current_mapping[[paste0("attr",layer_id)]] <- default_mappings$attr
      current_mapping[[paste0("map_count",layer_id)]] <- default_mappings$map_count
      current_mapping[[paste0("map_deleted",layer_id)]] <- default_mappings$map_deleted
      
      #identify layer geom
      geom_name <- input[[paste0("select_geom", layer_id)]]
      geom_params <- geom_mapping(geom_name)$params
      
      #remove params
      for(i in 1:length(geom_params)) {
        current_param[[paste0(names(geom_params)[i],layer_id)]] <- NULL
      }
      
    })
    
    
    
  })
  
  #Button for save param modal dialog
  observeEvent(input$button_param_save, {
    
    layer_id <- current_param$layer_id
    
    #identify layer geom
    geom_name <- input[[paste0("select_geom", layer_id)]]
    geom_params <- geom_mapping(geom_name)$params
    
    for(i in 1:length(geom_params)) {
      print(input[[paste0(dot_to_underscore(names(geom_params)[i]),layer_id)]])
      current_param[[paste0(names(geom_params)[i],layer_id)]] <- input[[paste0(dot_to_underscore(names(geom_params)[i]),layer_id)]]  
    }
    
    removeModal()
    
  })
 
  
  #Mapping Modal Data -------
  current_mapping <- shiny::reactiveValues(map_count = 0, map_deleted = 0)
  aes_mappings <- c("x","y","fill","color","group","shape")
  
  
  #Mapping Modal UI events ---------
  observeEvent(input$button_add_mapping, {
    
    l <- current_mapping$layer_id
    current_mapping[[paste0("map_count",l)]] <- 
      if(is_null_empty_na(current_mapping[[paste0("map_count",l)]])) {
        1
      } else {
        current_mapping[[paste0("map_count",l)]] + 1
      }
    
    map_id <- current_mapping[[paste0("map_count",l)]]
    
    shiny::insertUI(selector = paste0("#", paste0(ns("map_ui_container"), map_id - 1)), 
                    where = "afterEnd", 
                    ui = ggplotly_module_mapping_row_ui(map_id, 
                                                        aes_mappings, 
                                                        attr_choices(),
                                                        ns = ns))
    
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
        
        param_list <- geom_params_to_list(input[[paste0("select_geom",i)]], reactiveValuesToList(isolate(current_param)), i)
        print(param_list)
        geom_list[[paste0("layer",i)]] <- 
          new_ggplotly_geom(name = input[[paste0("select_geom",i)]],
                            aes_list = name_val_to_list(current_mapping[[paste0("aes",i)]], 
                                                        current_mapping[[paste0("attr",i)]]),
                            geom_list = append(list("data" = rf_data()), param_list))
      }
    }
    
    p_options <- new_ggplotly_options(gg_geom = geom_list, gg_theme = current_theme)
    
    p_options
  })
  
  output$plot_main <- renderPlotly({
    
    compose_ggplotly(ggplotly_options = gg_options())
    
  })
  
  # output$test_layer_1 <- 
  #   renderText({
  #     layer_id <- current_mapping$layer_id
  #     paste0(
  #       "layer: ", layer_id,
  #       "aes1: ", paste0(current_mapping[[paste0("aes",layer_id)]], collapse = ", "),
  #       " attr1: ", paste0(current_mapping[[paste0("attr",layer_id)]], collapse = ", "),
  #       " select_geom: ", input[[paste0("select_geom", layer_id)]]
  #     )
  #   })
  # 
  # output$test_pt_options <- 
  #   renderText({
  #     p <- gg_options()
  #     g1 <- p$gg_geom[[1]]
  #     paste0(
  #       "name: ", g1$name,
  #       " aes: ", paste0(names(g1$aes_list), collapse = ", "),
  #       " attr: ", paste0(g1$aes_list, collapse = ", "),
  #       " geom_names: ", paste0(names(g1$geom_list)[names(g1$geom_list) != "data"], collapse = ", "),
  #       " geom_values: ", paste0(g1$geom_list[names(g1$geom_list)[names(g1$geom_list) != "data"]], collapse = ", ")
  #   )})
  
}