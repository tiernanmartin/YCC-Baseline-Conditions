# library(sp)
library(rgdal)
library(plotly)
library(gplots)
library(ggplot2)
library(GISTools)
library(magrittr)
library(operator.tools)
library(shiny)
library(plyr)
library(dplyr)
library(shinydashboard)
library(leaflet)

library(shmodules)

data(newhaven)

crs_proj <- CRS("+init=epsg:4326")

proj4string(tracts) <- proj4string(blocks)

tracts %<>% spTransform(crs_proj)
blocks %<>% spTransform(crs_proj)

tracts@data %<>% mutate_at(vars(contains('P_')), funs(round_any(. * .01,.0001)))
blocks@data %<>% mutate_at(vars(contains('P_')), funs(round_any(. * .01,.0001)))

proj_light_grey <- col2hex("grey75")
proj_grey <- col2hex("grey50")
proj_dark_grey <- col2hex("grey25")
proj_orange <- '#D59C40'
