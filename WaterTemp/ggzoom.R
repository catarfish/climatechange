# Launch a Shiny app with your ggplot object, zoom control added: gg_zoom(g)
# Shiny brush event always return numerical values for the zoom box. If the x or y axis is in date, date-time, the zoom box coordinates need to be converted before taken by ggplot.
# https://github.com/hadley/scales/blob/38f81a7b79d98c06edd7d0b624c77b7834db508f/R/trans-date.r
# this will convert date, datetime axes automatically, though it need lubridate to work because the base R equivalent are more cumbersome to use.

library(shiny)
library(ggplot2)
library(lubridate)

ui <- fluidPage(
  plotOutput("plot",
             dblclick = "plot_dblclick",
             brush = brushOpts(
               id = "plot_brush",
               resetOnNew = TRUE
             ))
)
server_with_g <- function(g) {
  gb <- ggplot_build(g)
  x_scale <- gb$layout$panel_scales$x[[1]]$scale_name
  y_scale <- gb$layout$panel_scales$y[[1]]$scale_name
  get_trans_fun <- function(axis_scale) {
    switch(axis_scale,
           date = as_date,
           datetime = as_datetime,
           identity)
  }
  x_as <- get_trans_fun(x_scale)
  y_as <- get_trans_fun(y_scale)
  server <- function(input, output){
    add_zoom <- function(plot_id) {
      ranges <- reactiveValues(x = NULL, y = NULL)
      observeEvent(input[[paste0(plot_id, "_dblclick")]], {
        brush <- input[[paste0(plot_id, "_brush")]]
        if (!is.null(brush)) {
          # ranges$x <- c(brush$xmin, brush$xmax)
          ranges$x <- x_as(c(brush$xmin, brush$xmax))
          ranges$y <- y_as(c(brush$ymin, brush$ymax))
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })
      ranges
    }
    plot_range <- add_zoom("plot")
    output$plot <- renderPlot({
      g +
        coord_cartesian(xlim = plot_range$x, ylim = plot_range$y)
    })
  }
}

gg_zoom <- function(g) {
  shinyApp(ui = ui, server = server_with_g(g))
}