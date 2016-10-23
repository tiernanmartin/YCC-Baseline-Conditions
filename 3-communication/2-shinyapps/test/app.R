# SETUP -----
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(rprojroot)
library(shmodules) #devtools::install_github('tiernanmartin/shmodules')

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
source(root_file('3-communication/2-shinyapps/test/global.R'))

# UI -----

header <- dashboardHeader(title = "Map Template",titleWidth = "350px")

sidebar <- dashboardSidebar(
        width = "350px",
        sidebarCSS(),
        sidebarMenu(id = 'menu',
                    linkedScatterMapSidebarTabUI('barmap1','Map with bar plot','first'),
                    linkedScatterMapSidebarTabUI('scatmap2','Map with scatter plot','second')
        ),
        HTML("<hr style='margin: 5px;height:1px;border-width:0;color:#404040;background-color:#404040'>"),
        HTML("<div style='padding-right: 25px;padding-left: 25px;'>"),
        linkedScatterMapSidebarTabContentUI('barmap1','Map with bar plot','first',poc_ycc_sp),
        linkedScatterMapSidebarTabContentUI('scatmap2','Map with scatter plot','second',poc_sp),
        HTML("</div>")
)


body <- fluidDashboardBody(sidebarCollapsed = FALSE,
        tabItems(
                linkedScatterMapBodyUI(id = 'barmap1',tab_name = 'first'),
                linkedScatterMapBodyUI(id = 'scatmap2',tab_name = 'second')
                )



)

ui <- dashboardPage(header,sidebar,body, skin = 'yellow')

# SERVER  ------

server <- function(input, output, session) {

        plotly_event <- reactive({event_data('plotly_selected', source = 'source')})

        callModule(module = linkedBarMap,
                   id = "barmap1",
                   sp_rx = reactive({poc_ycc_sp}),
                   plotly_event_rx = reactive({plotly_event()})
        )
        callModule(module = linkedScatterMap,
                   id = "scatmap2",
                   sp_rx = reactive({poc_sp}),
                   plotly_event_rx = reactive({plotly_event()})
        )
}

# RUN -----

shinyApp(ui, server)
