theme_mapping <- function(theme_name) {
  
  p_list <- function(type, choices, selected) {
    return(
      list(type = type, choices = choices, selected = selected)
    )
  }
  
  p_list_text <- function(selected) p_list("text", character(), selected)
  
  colour_choices <- c("black","red","green","blue")
  
  p_list_colour = function(selected) p_list("character", colour_choices, selected)
  
  element_text_list <- 
    list(
      family = p_list_text(NULL),
      face = p_list("character", c("plain", "italic", "bold", "bold.italic"), NULL),
      colour = p_list_colour(NULL),
      size = p_list("numeric", numeric(), NULL),
      hjust = p_list("numeric",c(0,1), NULL),
      vjust = p_list("numeric", c(0,1), NULL),
      angle = p_list("numeric", c(0,360), NULL),
      lineheight = p_list("numeric", numeric(), NULL)
    )
  

  mappings <-
    list(
      x_axis = 
        list(
          params = 
            list(axis.title.x = p_list_text())
        ),
      y_axis = 
        list(
          params = 
            list()
        ),
      legend = 
        list(
          params = 
            list()
        ),
      panel =
        list(
          params = 
            list()
        ),
      plot = 
        list(
          params = 
            list()
        )
    )
}