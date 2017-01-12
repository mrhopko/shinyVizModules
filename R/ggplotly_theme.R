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
        axis.title = element_text_list,
        axis.title.x = element_text_list,
        axis.title.y = element_text_list,
        axis.text = element_text_list,
        axis.text.x = element_text_list,
        axis.text.y = element_text_list,
        axis.ticks = element_line_list,
        axis.ticks.x = element_line_list,
        axis.ticks.y = element_line_list,
        axis.ticks.length = p_list("numeric", numeric(), NULL),
        axis.line = element_line_list,
        axis.line.x = element_line_list,
        axis.line.y = element_line_list,
        legend.background = element_rect_list,
        legend.margin = p_list("numeric", numeric(), NULL),
        legend.key = element_rect_list,
        legend.key.size = p_list("numeric", numeric(), NULL),
        legend.key.height = p_list("numeric", numeric(), NULL),
        legend.key.width = p_list("numeric", numeric(), NULL),
        legend.text = element_text_list,
        legend.text.align = element_text_list,
        legend.title = element_text_list,
        legend.title.align = p_list("numeric", c(0,1), NULL),
        legend.position = p_list("character", c("none", "left", "right", "bottom", "top"), NULL),
        legend.direction = p_list("character", c("horizontal","vertical"), NULL),
        legend.justification = p_list("character", c("center"), NULL),
        legend.box = p_list("character", c("horizontal","vertical"), NULL),
        legend.box.just = p_list("character", c("none", "left", "right", "bottom", "top"), NULL),  
        panel.background = element_rect_list,
        panel.border = element_rect_list,
        panel.margin = p_list("numeric", c(0,1), NULL),
        panel.margin.x = p_list("numeric", c(0,1), NULL),
        panel.margin.y = p_list("numeric", c(0,1), NULL),
        panel.grid = element_line_list,
        panel.grid.major = element_line_list,
        panel.grid.major.x = element_line_list,
        panel.grid.major.y = element_line_list,
        panel.grid.minor = element_line_list,
        panel.grid.minor.x = element_line_list,
        panel.grid.minor.y = element_line_list,
        panel.ontop = p_list("logical", c(TRUE,FALSE), NULL),
        plot.background = element_rect_list,
        plot.title = element_text_list,
        plot.margin = p_list("numeric", c(0,1), NULL),
        strip.background = element_rect_list,
        strip.text = element_text_list,
        strip.text.x = element_text_list,
        strip.text.y = element_text_list,
        strip.switch.pad.grid = p_list("numeric", c(0,1), NULL),
        strip.switch.pad.wrap = p_list("numeric", c(0,1), NULL)
      
    )
  
  if(is_null_empty_na(theme_element, test_blank = TRUE)) {
    mappings
  } else {
    mappings[[theme_element]]
  }

}
