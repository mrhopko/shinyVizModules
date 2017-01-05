library(testthat)
library(ggplot2)
library(plotly)

source("../../R/ggplotly_helpers.R")
source("../../R/helpers.R")
context("ggplotly helpers")

test_that("ggplotly plots", {
  
  data <- datasets::mtcars
  data$car <- rownames(data)
  data$cyl <- as.factor(data$cyl)
  
  ggplotly_options <- new_ggplotly_options(
    gg_geom = list(
      new_ggplotly_geom(
        name = "geom_boxplot", 
        aes_list = list(
          x = "cyl",
          y = "mpg"),
        geom_list = list(
          data = data
        )
      )
    )
  )
  
  p <- compose_ggplotly(ggplotly_options)
  
  expect_is(p, "plotly")
  
})