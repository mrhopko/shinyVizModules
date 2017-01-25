#'New ggplotly_options
#'
#'@param gg_geom list of gg_geom_options
#'@param gg_scales list of options passed to scales
#'@param gg_coord list of options passed to coordinates
#'@param gg_position list of options passed to position
#'@param gg_annotation list of options passed to annotation
#'@param gg_theme list of options passed to theme
#'@returns ggploty_options
new_ggplotly_options <- function(gg_geom = list(),
                                 gg_scales = list(),
                                 gg_coord = list(),
                                 gg_facet = list(),
                                 gg_position = list(),
                                 gg_annotation = list(),
                                 gg_theme = list()) {
  
  
  me <- list(
    gg_geom = gg_geom,
    gg_scales = gg_scales,
    gg_coord = gg_coord,
    gg_facet = gg_facet,
    gg_position = gg_position,
    gg_annotation = gg_annotation,
    gg_theme = gg_theme
  )
  
  class(me) <- append(class(me), "ggplotly_options")
  
  me
  
}


#'New ggplotly_geom
#'
#'@param name name of the ggplot geom
#'@param aes_list list of parameters passed to aes and passed to geom
#'@param geom_list list of options passed to geom
#'@returns ggplotly_geom
new_ggplotly_geom <- function(name = NA_character_,
                              aes_list = list(),
                              geom_list = list()) {
  
  me <- list(
    name = name,
    aes_list = aes_list,
    geom_list = geom_list
  )
  
  class(me) <- append(class(me), "ggplotly_geom")
  
  me
  
}


#'Compose a ggplotly plot from an options object
#'
#'@param ggplotly_options object
#'@return ggplotly plot object
compose_ggplotly <- function(ggplotly_options) UseMethod("compose_ggplotly")

#'Compose a ggplotly plot from an options object
#'
#'@param ggplotly_options object
#'@return ggplotly plot object
compose_ggplotly.default <- function(ggplotly_options) stop("ggplotly_options must be of type ggplotly_options")

#'Compose a ggplotly plot from an options object
#'
#'@param ggplotly_options object
#'@return ggplotly plot object
compose_ggplotly.ggplotly_options <- function(ggplotly_options, ...) {
  
  p <- ggplot2::ggplot()
  
  #add geoms
  if(is_null_empty_na(ggplotly_options$gg_geom)) return(NULL)
  
  for(i in 1:length(ggplotly_options$gg_geom)) {
    
    g <- ggplotly_options$gg_geom[[i]]
    
    testthat::expect_is(g, "ggplotly_geom", info = paste0(i, " is not a ggplotly_geom"))
    
    #select the geom function
    geom <- get(g$name, envir = environment(ggplot2::ggplot))
    testthat::expect_is(geom, "function", info = paste0("function for ", g$name, " not found"))
    
    #compose geom 
    geom_aes <- purrr::lift_dl(ggplot2::aes_string)(g$aes_list)
    p <- p + purrr::lift_dl(geom)(append(list(mapping = geom_aes), g$geom_list))
  }
  
  #add theme
  theme_arg_list <- list()
  
  if(!is_null_empty_na(ggplotly_options$gg_theme)) {
    
    if(!is_null_empty_na(ggplotly_options$gg_theme$ggtheme)) {
      
      theme_func <- 
        tryCatch({
          get(ggplotly_options$gg_theme$ggtheme, envir = environment(ggplot2::ggplot))
        }, error = function(e) {
          get(ggplotly_options$gg_theme$ggtheme, envir = environment(ggthemes::theme_hc))
        })
      p <- p + theme_func()
    }
  
    #create argument list
    for(i in 1:ggplotly_options$gg_theme$theme_id) {
      if(!(i %in% ggplotly_options$gg_theme$theme_deleted))
      
      #Obtain elemnt and attribute values and functions
      theme_element <- ggplotly_options$gg_theme[[paste0("theme_element",i)]]
      element_func <- theme_mapping(theme_element)$func
      theme_attribute <- ggplotly_options$gg_theme[[paste0("theme_attribute",i)]]
      theme_attribute_value <-  ggplotly_options$gg_theme[[paste0("theme_attribute_value",i)]]
      
      #create element value from function or from value
      theme_element_value <-
        if(is.null(element_func)) {
          theme_attribute_value
        } else {
          params <- list(theme_attribute_value)
          names(params) <- theme_attribute
          purrr::lift_dl(element_func)(params)
        }
      
      #create theme argument and value pairing
      theme_arg <- list(theme_element_value)
      names(theme_arg) <- theme_element
       
      #append to argument list
      theme_arg_list <- append(theme_arg_list, theme_arg)

    }
    
    #pass argument list to theme
    if(length(theme_arg_list) > 0) {
      p <- p + purrr::lift_dl(theme)(theme_arg_list)
    }
  }
  
  
  # scales
  p <- apply_fun_list_to_object(p, ggplotly_options$gg_scales, environment(ggplot2::ggplot))
  
  # Coordinates
  p <- apply_fun_list_to_object(p, ggplotly_options$gg_coord, environment(ggplot2::ggplot))
  
  # Facet
  p <- apply_fun_list_to_object(p, ggplotly_options$gg_facet, environment(ggplot2::ggplot))

  # Position
  p <- apply_fun_list_to_object(p, ggplotly_options$gg_position, environment(ggplot2::ggplot))
  
  #Annotation
  p <- apply_fun_list_to_object(p, ggplotly_options$gg_annotation, environment(ggplot2::ggplot))
  
  #plotly
  p <- plotly::ggplotly(p, ...)
  
  return(p)
  
}

#'Convert vectors of names and values to a list
#'
#'@param l_names character vector of list names
#'@param l_vals character vector of list values
#'@retunrs list of l_names[i] = l_vals[i]
name_val_to_list <- function(l_names, l_vals) {
  if(is_null_empty_na(l_vals)) return(list())
  l <- as.list(l_vals)
  if(is_null_empty_na(l_names)) return(l)
  if(length(l_vals) != length(l_names)) {
    warning("length of l_names and l_vals does not match")
    return(l)
  }
  names(l) <- l_names
  l
}


dot_to_underscore <- function(x) {
  gsub("\\.", "\\_\\_",x)
}

underscore_to_dot <- function(x) {
  gsub("\\_\\_","\\." ,x)
}

