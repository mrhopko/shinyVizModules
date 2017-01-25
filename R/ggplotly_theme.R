theme_mapping <- function(theme_element = "") {
  
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
  
  element_line_list <- 
    list(
      colour = p_list_colour(NULL),
      size = p_list("numeric", numeric(), NULL),
      linetype = p_list("character", c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), NULL),
      lineend = p_list("character",c("round","butt","square"), NULL)
    )
  
  element_rect_list <- 
    list(
      fill = p_list_colour(NULL), 
      colour = p_list_colour(NULL), 
      size = p_list("numeric", numeric(), NULL), 
      linetype = p_list("character", c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), NULL)
      )
  
  mappings <-
    list(
        axis.title = list(attributes = element_text_list, func = element_text),
        axis.title.x = list(attributes = element_text_list, func = element_text),
        axis.title.y = list(attributes = element_text_list, func = element_text),
        axis.text = list(attributes = element_text_list, func = element_text),
        axis.text.x = list(attributes = element_text_list, func = element_text),
        axis.text.y = list(attributes = element_text_list, func = element_text),
        axis.ticks = list(attributes = element_line_list, func = element_line),
        axis.ticks.x = list(attributes = element_line_list, func = element_line),
        axis.ticks.y = list(attributes = element_line_list, func = element_line),
        axis.ticks.length = list(attributes = p_list("numeric", numeric(), NULL), func = NULL),
        axis.line = list(attributes = element_line_list, func = element_line),
        axis.line.x = list(attributes = element_line_list, func = element_line),
        axis.line.y = list(attributes = element_line_list, func = element_line),
        legend.background = list(attributes = element_rect_list, func = element_rect),
        legend.margin = list(attributes = p_list("numeric", numeric(), NULL), func = NULL),
        legend.key = list(attributes = element_rect_list, func = element_rect),
        legend.key.size = list(attributes = p_list("numeric", numeric(), NULL), func = NULL),
        legend.key.height = list(attributes = p_list("numeric", numeric(), NULL), func = NULL),
        legend.key.width = list(attributes = p_list("numeric", numeric(), NULL), func = NULL),
        legend.text = list(attributes = element_text_list, func = element_text),
        legend.text.align = list(attributes = element_text_list, func = NULL),
        legend.title = list(attributes = element_text_list, func = element_text),
        legend.title.align = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        legend.position = list(attributes = p_list("character", c("none", "left", "right", "bottom", "top"), NULL), func = NULL),
        legend.direction = list(attributes = p_list("character", c("horizontal","vertical"), NULL), func = NULL),
        legend.justification = list(attributes = p_list("character", c("center"), NULL), func = NULL),
        legend.box = list(attributes = p_list("character", c("horizontal","vertical"), NULL), func = NULL),
        legend.box.just = list(attributes = p_list("character", c("none", "left", "right", "bottom", "top"), NULL),  func = NULL), 
        panel.background = list(attributes = element_rect_list, func = element_rect),
        panel.border = list(attributes = element_rect_list, func = element_rect),
        panel.margin = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        panel.margin.x = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        panel.margin.y = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        panel.grid = list(attributes = element_line_list, func = element_line),
        panel.grid.major = list(attributes = element_line_list, func = element_line),
        panel.grid.major.x = list(attributes = element_line_list, func = element_line),
        panel.grid.major.y = list(attributes = element_line_list, func = element_line),
        panel.grid.minor = list(attributes = element_line_list, func = element_line),
        panel.grid.minor.x = list(attributes = element_line_list, func = element_line),
        panel.grid.minor.y = list(attributes = element_line_list, func = element_line),
        panel.ontop = list(attributes = p_list("logical", c(TRUE,FALSE), NULL), func = NULL),
        plot.background = list(attributes = element_rect_list, func = element_rect),
        plot.title = list(attributes = element_text_list, func = element_text),
        plot.margin = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        strip.background = list(attributes = element_rect_list, func = element_rect),
        strip.text = list(attributes = element_text_list, func = element_text),
        strip.text.x = list(attributes = element_text_list, func = element_text),
        strip.text.y = list(attributes = element_text_list, func = element_text),
        strip.switch.pad.grid = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL),
        strip.switch.pad.wrap = list(attributes = p_list("numeric", c(0,1), NULL), func = NULL)
    )
  
  if(is_null_empty_na(theme_element, test_blank = TRUE)) {
    mappings
  } else {
    mappings[[theme_element]]
  }

}
