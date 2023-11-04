# mingiPolygons

This is a R package created to improve interactivity in  map visualizations deployed as dashboards in Shiny.

It identifies polygon objects (country, county, state) in a map.

Authors: Martin Mburu, Ted Muthomi.

Created: 30-09-2023


## DESCRIPTION

It identifies polygon objects in a map visualization formed by ggplot or any other package extension deployed in Shiny.

It is an extension of nearPoints() but specifically for maps.

It takes in original data used to form map in ggplot, data collected by nearPoints and input$plot_click value to determine the polygon object


## SETUP AND REQUIREMENTS

 The packages Shiny, tidyverse is needed for this package. 

 To setup this package in your RStudio, install devtools then devtools::install_github("mmburu8/mingiPolygons")

 To see the help page run '??mingiPolygons'. Select 'mingiPolygons::mingiPolygons'

 mingiPolygons requires the map visualization to be deployed in Shiny since it takes plot_click as an argument.

Dataframe used to form the map should be processed by mingiPolygons:::preWork(data_main, col_spec)

Then use the function mingiPolygons to determine polygon object.

## TECHNOLOGIES USED

Languages: R

Libraries: tidyverse, Shiny

IDE: RStudio

## RESOURCES

Full paper: https://docs.google.com/document/d/1IE6pnGhwDeZWEVqxT1ShMn0reihtTBVYD-o7M2TyhYs/edit?usp=sharing

Shiny application: https://naneai.shinyapps.io/mingiPolygons/


nan.viz
