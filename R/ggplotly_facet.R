facet_mapping <- function(facet_name) {
  
  p_list <- function(type, choices, selected) {
    return(
      list(type = type, choices = choices, selected = selected)
    )
  }
    
  p_logical <- function(selected) p_list("logical", c(TRUE, FALSE), selected)
  
  mappings <-
    list(
      facet_grid = 
        list(
          params = 
            list(
              margins = p_logical(FALSE),
              scales = p_list("character", c("fixed", "free_x", "free_y", "free"), "fixed"),
              space = p_list("character", c("fixed", "free_x", "free_y", "free"), "fixed"),
              shrink = p_logical(TRUE),
              as.table = p_logical(TRUE),
              switch = p_list("character", c("x","y","both"), NULL),
              drop = p_logical(TRUE)
            )
        ),
      facet_wrap = 
        list(
          params = 
            list(
              nrow = p_list("integer", c(1,100), NULL),
              ncol = p_list("integer", c(1,100), NULL),
              scales = p_list("character", c("fixed", "free_x", "free_y", "free"), "fixed"),
              shrink = p_logical(TRUE),
              as.table = p_logical(TRUE),
              switch = p_list("character", c("x","y","both"), NULL),
              drop = p_logical(TRUE),
              dir = p_list("character", c("h","v"), "h")
            )
        )
    )
  
  return(mappings[[facet_name]])
}