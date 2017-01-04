is_null_empty_na <- function(x, test_blank = FALSE) {
  if(is.null(x)) return(TRUE)
  if(length(x) == 0) return(TRUE)
  if(all(is.na(x))) return(TRUE)
  if (test_blank) {
    if(all(x == "")) return(TRUE)
  }
  FALSE
}


#'Apply a function list to an object
#'
#'Where fun_list has the form list(function_name = list(function_params))
#'@param x object to apply functions
#'@param fun_list list(function_name = list(function_params))
#'@returns x object with functions applied
apply_fun_list_to_object <- function(x, fun_list, envir = as.environment(-1)) {

  if(!is_null_empty_na(fun_list)) {
    for(i in 1:length(x)) {
      f <- x[[i]]
      testthat::expect_is(f, "list")
      f_fun <- get(names(f), envir=envir)
      if(is_null_empty_na(f)) {
        x <- x + f_fun()
      } else {
        x <- x + purrr::lift_dl(f_fun)(f[[1]])
      }
    }
  }
  
  return(x)
}