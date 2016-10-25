# SETUP -----
library(scales)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(rprojroot)
library(miscgis)
library(shmodules) #devtools::install_github('tiernanmartin/shmodules')

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
source(root_file('3-communication/2-shinyapps/test3/global.R'))

# UI -----

header <- dashboardHeader(title = "Map Template",titleWidth = "350px")

sidebar <- dashboardSidebar(
        width = "350px",
        shmodules::sidebarCSS(),
        sidebarMenu(id = 'menu',
                    shmodules::linkedBarMapSidebarTabUI('barmap1','Map with bar plot','first')
        ),
        HTML("<hr style='margin: 5px;height:1px;border-width:0;color:#404040;background-color:#404040'>"),
        HTML("<div style='padding-right: 25px;padding-left: 25px;'>"),
        linkedBarMapSidebarTabContentUI('barmap1','Map with bar plot','first',poc_ycc_sp),
        HTML("</div>")
)


body <- shmodules::fluidDashboardBody(sidebarCollapsed = FALSE,
        tabItems(
                linkedBarMapBodyUI(id = 'barmap1',tab_name = 'first')
                )
)

ui <- dashboardPage(header,sidebar,body, skin = 'yellow')

# SERVER  ------

server <- function(input, output, session) {

        plotly_event <- reactive({event_data('plotly_selected', source = 'source')})

        callModule(module = linkedBarMap,
                   id = "barmap1",
                   df = poc_ycc_df,
                   sp_rx = reactive({poc_ycc_sp}),
                   plotly_event_rx = reactive({plotly_event()})
        )
        # callModule(module = shmodules::linkedScatterMap,
        #            id = "scatmap2",
        #            sp_rx = reactive({blocks}),
        #            plotly_event_rx = reactive({plotly_event()})
        # )
}

# RUN -----

shinyApp(ui, server)
