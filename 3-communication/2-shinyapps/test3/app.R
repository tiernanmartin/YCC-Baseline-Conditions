# SETUP -----
# library(scales)
# library(shiny)
# library(shinydashboard)
# library(DT)
# library(ggplot2)
# library(tidyverse)
# library(rprojroot)
# library(miscgis)
# library(shmodules) #devtools::install_github('tiernanmartin/shmodules')
library(scales)
library(sp)
library(rgdal)
library(plotly)
library(gplots)
library(ggplot2)
library(GISTools)
library(magrittr)
library(operator.tools)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(plyr)
library(knitr)
library(rprojroot)
library(rgdal)
library(sp)
library(rgeos)
library(tigris)
library(leaflet)
library(ggthemes)
library(magrittr)
library(stringr)
library(downloader)
library(webshot)
library(htmltools)
library(gplots)
library(ggmap)
library(shiny)
library(htmlwidgets)
library(readxl)
library(acs)
library(RColorBrewer)
library(tidyverse)
library(operator.tools)
library(ggiraph)
library(leaflet.extras)
library(viridisLite)
library(shmodules)
library(miscgis)
library(tidyverse)
library(sf)

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
        linkedBarMapSidebarTabContentUI(id = 'barmap1', 
                                        menu_item_name = 'Map with bar plot', 
                                        tab_name = 'first', 
                                        vars = vars),
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
                   sf_rx = reactive({comb_long_sf}),
                   vars = vars,
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
