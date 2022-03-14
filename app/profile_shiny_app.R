# library(shiny)
library(profvis)
profvis({
  shiny::runApp()
})

library(lobstr)
mem_used()
