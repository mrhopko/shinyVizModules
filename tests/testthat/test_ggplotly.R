library(testthat)
library(ggplot2)
library(plotly)

source("../../R/ggplotly_helpers.R")

context("ggplotly helpers")

test_that("ggplotly plots", {
  
  data <- datasets::mtcars
  data$car <- rownames(data)
  
  ggplotly_options <- new_ggplotly_options(
    gg_geom = list(
      new_ggplotly_geom(
        name = "geom_bar", 
        aes_list = list(
          x = "car",
          y = "mpg",
          fill = "car"
        ),
        geom_list = list(
          stat = "identity",
          data = data
        )
      )
    )
  )
  
  p <- compose_ggplotly(ggplotly_options)
  
  expect_is(p, "plotly")
  
})