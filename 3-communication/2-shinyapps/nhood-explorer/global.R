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
library(ggrepel)
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

proj_light_grey <- col2hex("grey75")
proj_grey <- col2hex("grey50")
proj_dark_grey <- col2hex("grey25")
proj_orange <- '#D59C40'

comb_long_sf <- read_rds(root_file('1-data/5-tidy/comb-long-sf.rds'))

vars <- comb_long_sf$Variable %>% unique

linkedBarMapSidebarTabContentUI <-
        function (id, menu_item_name, tab_name, vars) {
                ns <- NS(id)
                shiny::req(vars)
                cond_tab <- paste0("input.menu == '", tab_name, "'")
                
                tagList(conditionalPanel(condition = cond_tab,
                                         navbarPage(
                                                 "",
                                                 tabPanel(
                                                         title = "Explore",
                                                         fluidRow(width = 12,
                                                                  columnStyle(
                                                                          width = 10,
                                                                          selectizeInput(
                                                                                  inputId = ns("y_axis"),
                                                                                  label = "Select a variable (Y Axis):",
                                                                                  selected = vars[[1]],
                                                                                  choices = vars
                                                                          )
                                                                  )),
                                                         fluidRow(width = 12, plotlyOutput(ns("bar"),width = "auto")),
                                                         fluidRow(width = 12, uiOutput(ns("var_text")))
                                                 ),
                                                 tabPanel(
                                                         title = "Style",
                                                         fluidRow(
                                                                 width = 12,
                                                                 columnStyle(
                                                                         width = 9,
                                                                         selectizeInput(
                                                                                 inputId = ns("pal"),
                                                                                 label = "Select a color palette",
                                                                                 choices = c("Sequential",
                                                                                             "Divergent", "Qualitative"),
                                                                                 selected = "Sequential",
                                                                                 multiple = FALSE
                                                                         )
                                                                 ),
                                                                 columnStyle(
                                                                         width = 3,
                                                                         checkboxInputStyle(
                                                                                 inputId = ns("rev"),
                                                                                 label = "Reverse",
                                                                                 value = FALSE,
                                                                                 cssStyle = "padding: 0px;"
                                                                         ),
                                                                         cssStyle = "padding: 0px;"
                                                                 )
                                                         )
                                                 )
                                         )))
        }

linkedBarMap <-
        function (input,
                  output,
                  session,
                  sf_rx,
                  vars,
                  plotly_event_rx)
        {
                ns <- session$ns
                
                myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]

                x_axis <- reactive({
                        input$x_axis
                })
                
                y_axis <- reactive({
                        input$y_axis
                })

                sf_rx_all <- reactive({
                        sf_rx() %>%
                                mutate(KEY = 1:nrow(.)) %>%
                                filter(Variable == y_axis())
                        })

                sf_rx_map_ycc <- reactive({
                        sf_rx_all() %>%
                                filter(GEOGRAPHY == 'neighborhood')
                })
                
                sf_rx_map_no_ycc <- reactive({
                        
                        sf_rx_all() %>% 
                                filter(GEOGRAPHY == 'tract' & is.na(NAME))
                })

                
                sf_rx_bar <- reactive({
                        sf_rx_all() %>%
                                filter(GEOGRAPHY != 'tract') %>%
                                select(-geom) %>%
                                as.data.frame()
                })

                sf_rx_bar_ycc <- reactive({
                        sf_rx_bar() %>%  
                                filter(GEOGRAPHY %!in% c('city')) %>% 
                                mutate(NAME_FCT_YCC = factor(NAME, 
                                                             levels = c('CH','CA','FH','CID','YCC'),
                                                             ordered = TRUE))
                })
                
                sf_rx_bar_sea <- reactive({
                        sf_rx_bar() %>%  filter(GEOGRAPHY == 'city')
                })
                
                sf_rx_bar_sea_lbl <- reactive({
                        sf_rx_bar_sea() %>% mutate(NAME_FCT_YCC = factor('CID',
                                                                         levels = c('CH','CA','FH','CID','YCC'),
                                                                         ordered = TRUE))
                })
                
                pal_choice <- reactive({
                        if (input$pal == "Qualitative") {
                                RColorBrewer::brewer.pal(n = 9, name = "Set1")
                        }
                        else if (input$pal == "Divergent") {
                                RColorBrewer::brewer.pal(n = 10, name = "Spectral")[2:9]
                        }
                        else {
                                RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                        }
                })
                
                pal_choice_rev <- reactive({
                        if (input$rev) {
                                rev(pal_choice())
                        }
                        else
                                pal_choice()
                })
                
                colorpal <- reactive({
                        if (is.numeric(sf_rx_all()[['Estimate']])) {
                                colorNumeric(pal_choice_rev(), sf_rx_all()[['Estimate']])
                        }
                        else {
                                colorFactor(pal_choice_rev(),
                                            sf_rx_all()[['Estimate']] %>%
                                                    as.character() %>% factor)
                        }
                })
                
                map_layers <- c("YCC","Other Tracts")

                output$map <- renderLeaflet({ 
                        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                        
                        sf_all <- 
                                sf_rx() %>%
                                mutate(KEY = 1:nrow(.)) %>%
                                filter(Variable == vars[[1]])
                        
                        sf_ycc <- 
                                sf_all %>%
                                filter(GEOGRAPHY == 'neighborhood')
                        
                        sf_no_ycc <- 
                                sf_all %>%
                                filter(GEOGRAPHY == 'tract' & is.na(NAME))

                        pal <- colorNumeric(myYlOrRd, sf_all$Estimate)
                        
                        myLflt() %>%
                                addPolygons(
                                        data = as(sf_ycc,'Spatial'),
                                        color = col2hex("white"),
                                        opacity = 1,
                                        weight = 0.5,
                                        fillColor = pal(sf_ycc$Estimate),
                                        fillOpacity = 0.85,
                                        smoothFactor = 0,
                                        group = map_layers[[1]]
                                ) %>%
                                addPolygons(
                                        data = as(sf_no_ycc,'Spatial'),
                                        color = col2hex("white"),
                                        opacity = 1,
                                        weight = 0.5,
                                        fillColor = pal(sf_no_ycc$Estimate),
                                        fillOpacity = 0.85,
                                        smoothFactor = 0,
                                        group = map_layers[[2]]
                                ) %>%
                                addLegend(
                                        position = "bottomleft",
                                        opacity = 0.85,
                                        pal = pal,
                                        values = sf_all$Estimate
                                ) %>%
                                addLayersControl(baseGroups = map_layers[[1]],
                                                 overlayGroups = map_layers[[2]],
                                                 position = 'topright',
                                                 options = layersControlOptions(collapsed = FALSE,autoZIndex = FALSE))
                        
                })
                
                observe({
                        pal <- colorpal()

                        switch_labFrmt <- {
                                if (miscgis::is_pct(sf_rx_all()[['Estimate']])) {
                                        function(type,
                                                 ...)
                                                labelFormat(
                                                        suffix = '%',
                                                        transform = function(x)
                                                                round_any(100 * x, .1)
                                                )
                                } else{
                                        function(type,
                                                 ...)
                                                labelFormat()
                                }
                        }
                        
                        leafletProxy(ns("map")) %>%
                                clearShapes() %>%
                                clearControls() %>%
                                addPolygons(
                                        data = as(sf_rx_map_ycc(),'Spatial'),
                                        color = col2hex("white"),
                                        opacity = 1,
                                        weight = 0.5,
                                        fillColor = pal(sf_rx_map_ycc()[['Estimate']]),
                                        fillOpacity = 0.85,
                                        smoothFactor = 0,
                                        group = map_layers[[1]]
                                ) %>%
                                addPolygons(
                                        data = as(sf_rx_map_no_ycc(),'Spatial'),
                                        color = col2hex("white"),
                                        opacity = 1,
                                        weight = 0.5,
                                        fillColor = pal(sf_rx_map_no_ycc()[['Estimate']]),
                                        fillOpacity = 0.85,
                                        smoothFactor = 0,
                                        group = map_layers[[2]]
                                ) %>%
                                addLegend(
                                        title = y_axis(),
                                        position = "bottomleft",
                                        opacity = 0.85,
                                        pal = pal,
                                        values = sf_rx_all()[['Estimate']],
                                        labFormat = switch_labFrmt()
                                ) %>%
                                addLayersControl(baseGroups = map_layers[[1]],
                                                 overlayGroups = map_layers[[2]],
                                                 position = 'topright',
                                                 options = layersControlOptions(collapsed = FALSE,autoZIndex = FALSE))
                })
                
                
                # Lollipop
                output$bar <- renderPlotly({

                        pal <- colorpal()

                        y_rng <- range(sf_rx_all()[['Estimate']])

                        x_rng <- range(sf_rx_all()[['NAME_FCT']])

                        gg1 <- {
                                ggX <-  ggplot(sf_rx_bar_ycc())
                                ggX <- ggX + geom_segment(mapping = aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                                        xend = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                                        y = sf_rx_bar_ycc()[['Lower_Confint']],
                                                                        yend = sf_rx_bar_ycc()[['Upper_Confint']],
                                                                        color = sf_rx_bar_ycc()[['Estimate']]),
                                                            size = 3,
                                                            alpha = .25)
                                # ggX <- ggX + geom_point(aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                #                           y = sf_rx_bar_ycc()[['Estimate']],
                                #                           colour = sf_rx_bar_ycc()[['Estimate']],
                                #                           fill = sf_rx_bar_ycc()[['Estimate']],
                                #                           text = scales::percent_format()(sf_rx_bar_ycc()[['Estimate']])),
                                #                         shape = 21,
                                #                         size = 3,
                                #                         stroke = 0,
                                #                         stat = "identity",
                                #                         alpha = .85,
                                #                         na.rm = TRUE)
                                ggX <- ggX + geom_point(aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                          y = sf_rx_bar_ycc()[['Estimate']],
                                                          colour = sf_rx_bar_ycc()[['Estimate']]),
                                                        shape = 1,
                                                        size = 3,
                                                        stroke = .25,
                                                        stat = "identity",
                                                        alpha = 1,
                                                        na.rm = TRUE)
                                ggX <- ggX + geom_hline(data = sf_rx_bar_sea(),
                                                        aes(yintercept = sf_rx_bar_sea()[['Estimate']],
                                                            colour = sf_rx_bar_sea()[['Estimate']]),
                                                        size = .75, alpha = .85)
                                ggX <- ggX + geom_point(data = sf_rx_bar_sea_lbl(),
                                                        aes(x = sf_rx_bar_sea_lbl()[['NAME_FCT_YCC']],
                                                            y = sf_rx_bar_sea_lbl()[['Estimate']]),
                                                        alpha = 0)
                                ggX <- ggX + geom_text(data = sf_rx_bar_sea_lbl(),
                                                       aes(x = sf_rx_bar_sea_lbl()[['NAME_FCT_YCC']],
                                                           y = sf_rx_bar_sea_lbl()[['Estimate']],
                                                           label = paste("Seattle Avg: ",
                                                                           scales::percent_format()(sf_rx_bar_sea_lbl()[['Estimate']])),
                                                           colour = sf_rx_bar_sea_lbl()[['Estimate']]),
                                        size = 3,nudge_y = -0.05
                                )
                                
                                ggX <- ggX + geom_point(aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                          y = sf_rx_bar_ycc()[['Estimate']],
                                                          colour = sf_rx_bar_ycc()[['Estimate']],
                                                          fill = sf_rx_bar_ycc()[['Estimate']],
                                                          text = paste0(
                                                                  scales::percent_format()(sf_rx_bar_ycc()[['Estimate']])
                                                          )
                                                          ),
                                                        shape = 21,
                                                        size = 3,
                                                        stroke = 0,
                                                        stat = "identity",
                                                        alpha = .85,
                                                        na.rm = TRUE)


                        }

                        gg2 <- {

                                if (is.numeric(sf_rx_bar_ycc()[['Estimate']])) {
                                        gg1 +
                                                scale_color_gradientn(colors = pal_choice_rev(),limits = y_rng, na.value='transparent') +
                                                scale_fill_gradientn(colors = pal_choice_rev(),limits = y_rng, na.value='transparent')
                                } else{
                                        gg1 +
                                                scale_color_manual(values = pal(sf_rx_bar_ycc()[['Estimate']]),na.value='transparent') +
                                                scale_fill_manual(values = pal(sf_rx_bar_ycc()[['Estimate']]),na.value='transparent')

                                }
                        }


                        gg3 <- {
                                gg2 +
                                        xlab('Neighborhoods') +
                                        ylab(sf_rx_bar_ycc()[['Variable']][[1]]) +
                                        theme(
                                                legend.position = "none",
                                                plot.background = element_rect(fill = "transparent"),
                                                panel.background = element_rect(fill = "transparent"),
                                                text = element_text(color = "white"),
                                                axis.text = element_text(color = proj_grey),
                                                axis.ticks = element_blank(),
                                                panel.grid.major = element_line(color = proj_grey),
                                                panel.grid.minor = element_line(color = proj_grey,
                                                                                size = 2),
                                                axis.line.x = element_line(color = "white"),
                                                axis.line.y = element_line(color = "white")
                                        )
                        }

                        # Test if the scales are percentages or long numbers
                        gg4 <- {
                                y <- {
                                        if (miscgis::is_pct(sf_rx_bar_ycc()[['Estimate']]) &
                                            max(sf_rx_bar_ycc()[['Estimate']])<.1) {

                                                gg3 + scale_y_continuous(labels = scales::percent,limits = c(0,1))
                                                        # scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                                                        # geom_text(aes(x = sf_rx_bar_ycc()[['NAME_FCT']],
                                                        #               y =sf_rx_bar_ycc()[['Estimate']],
                                                        #               label = scales::percent(sf_rx_bar_ycc()[['Estimate']])),
                                                        #           stat = "identity",
                                                        #           color = col2hex("white"),
                                                        #           fontface = "bold",
                                                        #           size = 2.5,
                                                        #           nudge_y = .02)

                                        }else if(miscgis::is_pct(sf_rx_bar_ycc()[['Estimate']]) &
                                                 max(sf_rx_bar_ycc()[['Estimate']])>=.1){

                                                gg3 + scale_y_continuous(labels = scales::percent,limits = c(0,1))
                                                        # scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                                                        # geom_text(aes(x = sf_rx_bar_ycc()[['NAME_FCT']],
                                                        #               y = .05,
                                                        #               label = scales::percent(sf_rx_bar_ycc()[['Estimate']])),
                                                        #           stat = "identity",
                                                        #           color = col2hex("black"),
                                                        #           fontface = "bold",
                                                        #           size = 2.5)

                                        }else if(is.numeric(sf_rx_bar_ycc()[['Estimate']]) &
                                                 min(sf_rx_bar_ycc()[['Estimate']], na.rm = TRUE) >
                                                 1) {
                                                gg3 +
                                                        scale_y_continuous(labels = scales::comma)
                                        } else
                                                gg3
                                }

                                xy <-
                                {
                                        if (miscgis::is_pct(sf_rx_bar_ycc()[['NAME_FCT']])) {
                                                y +
                                                        scale_x_continuous(labels = scales::percent)
                                        } else if (is.numeric(sf_rx_bar_ycc()[['NAME_FCT']]) &
                                                   min(sf_rx_bar_ycc()[['NAME_FCT']], na.rm = TRUE) >
                                                   1) {
                                                gg3 +
                                                        scale_x_continuous(labels = scales::comma)
                                        } else
                                                y
                                }
                                gg4 <- xy

                        }
                        
                        gg5 <- {
                                ggplot(sf_rx_bar_ycc()) + 
                                        geom_segment(mapping = aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                                        xend = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                                        y = sf_rx_bar_ycc()[['Lower_Confint']],
                                                                        yend = sf_rx_bar_ycc()[['Upper_Confint']],
                                                                        color = sf_rx_bar_ycc()[['Estimate']]),
                                                            size = 3,
                                                            alpha = .25) +
                                        geom_point(aes(x = sf_rx_bar_ycc()[['NAME_FCT_YCC']],
                                                       y = sf_rx_bar_ycc()[['Estimate']],
                                                       colour = sf_rx_bar_ycc()[['Estimate']],
                                                       fill = sf_rx_bar_ycc()[['Estimate']],
                                                       text = scales::percent_format()(sf_rx_bar_ycc()[['Estimate']])),
                                                   shape = 21,
                                                   size = 3,
                                                   stroke = 0,
                                                   stat = "identity",
                                                   alpha = .85,
                                                   na.rm = TRUE)
                        }


                        gg <- gg4
                        
                        gg %>% 
                                ggplotly(tooltip = c('text')) %>% 
                                layout(dragmode = "select",
                                        margin = list(
                                                l = 60,
                                                r = 50,
                                                b = 50,
                                                t = 50
                                        ),
                                        font = list(family = "Open Sans",
                                                    size = 16)
                                ) %>% config(displaylogo = FALSE,
                                             displayModeBar = FALSE) %>% 
                                plotly_build()
                        

                })
                
                # Variable Text
                output$var_text <- renderUI({
                        file <- switch(input$y_axis,
                                       'Working Poor' = root_file('1-data/1-notebooks/work-poor-desc.html'),
                                       'People of Color' = root_file('1-data/1-notebooks/poc-desc.html'),
                                       'Housing Cost Burdened' = root_file('1-data/1-notebooks/hous-brdn-desc.html'),
                                       'Limited English' = root_file('1-data/1-notebooks/lmtd-eng-desc.html'),
                                       'Foreign Born' = root_file('1-data/1-notebooks/frn-born-desc.html'),
                                       'Unemployment' = root_file('1-data/1-notebooks/unemployed-desc.html'),
                                       stop("Unknown option")
                        )
                        includeHTML(file)
                })
                
                
                # # Bar
                # output$bar <- renderPlotly({
                # 
                #         pal <- colorpal()
                # 
                #        y_rng <- range(sf_rx_all()[['Estimate']])
                # 
                #        x_rng <- range(sf_rx_all()[['NAME_FCT']])
                # 
                #         gg1 <- {
                #                 ggplot(sf_rx_bar()) + geom_bar(
                #                         aes(
                #                                 x = sf_rx_bar()[['NAME_FCT']],
                #                                 y = sf_rx_bar()[['Estimate']],
                #                                 colour = sf_rx_bar()[['Estimate']],
                #                                 fill = sf_rx_bar()[['Estimate']]
                #                         ),
                #                         stat = "identity",
                #                         alpha = .85,
                #                         na.rm = TRUE
                #                 )
                #         }
                # 
                #         gg2 <- {
                # 
                #                 if (is.numeric(sf_rx_bar()[['Estimate']])) {
                #                         gg1 +
                #                                 scale_color_gradientn(colors = pal_choice_rev(),limits = y_rng, na.value='transparent') +
                #                                 scale_fill_gradientn(colors = pal_choice_rev(),limits = y_rng, na.value='transparent')
                #                 } else{
                #                         gg1 +
                #                                 scale_color_manual(values = pal(sf_rx_bar()[['Estimate']]),na.value='transparent') +
                #                                 scale_fill_manual(values = pal(sf_rx_bar()[['Estimate']]),na.value='transparent')
                # 
                #                 }
                #         }
                # 
                # 
                #         gg3 <- {
                #                 gg2 +
                #                         xlab('Neighborhoods') +
                #                         ylab(sf_rx_bar()[['Variable']][[1]]) +
                #                         theme(
                #                                 legend.position = "none",
                #                                 plot.background = element_rect(fill = "transparent"),
                #                                 panel.background = element_rect(fill = "transparent"),
                #                                 text = element_text(color = "white"),
                #                                 axis.text = element_text(color = proj_grey),
                #                                 axis.ticks = element_blank(),
                #                                 panel.grid.major = element_line(color = proj_grey),
                #                                 panel.grid.minor = element_line(color = proj_grey,
                #                                                                 size = 2),
                #                                 axis.line.x = element_line(color = "white"),
                #                                 axis.line.y = element_line(color = "white")
                #                         )
                #         }
                # 
                #         # Test if the scales are percentages or long numbers
                #         gg4 <- {
                #                 y <- {
                #                         if (miscgis::is_pct(sf_rx_bar()[['Estimate']]) &
                #                             max(sf_rx_bar()[['Estimate']])<.1) {
                # 
                #                                 gg3 +
                #                                         scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                #                                         geom_text(aes(x = sf_rx_bar()[['NAME_FCT']],
                #                                                       y =sf_rx_bar()[['Estimate']],
                #                                                       label = scales::percent(sf_rx_bar()[['Estimate']])),
                #                                                   stat = "identity",
                #                                                   color = col2hex("white"),
                #                                                   fontface = "bold",
                #                                                   size = 2.5,
                #                                                   nudge_y = .02)
                # 
                #                         }else if(miscgis::is_pct(sf_rx_bar()[['Estimate']]) &
                #                                  max(sf_rx_bar()[['Estimate']])>=.1){
                # 
                #                                 gg3 +
                #                                         scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                #                                         geom_text(aes(x = sf_rx_bar()[['NAME_FCT']],
                #                                                       y = .05,
                #                                                       label = scales::percent(sf_rx_bar()[['Estimate']])),
                #                                                   stat = "identity",
                #                                                   color = col2hex("black"),
                #                                                   fontface = "bold",
                #                                                   size = 2.5)
                # 
                #                         }else if(is.numeric(sf_rx_bar()[['Estimate']]) &
                #                                    min(sf_rx_bar()[['Estimate']], na.rm = TRUE) >
                #                                    1) {
                #                                 gg3 +
                #                                         scale_y_continuous(labels = scales::comma)
                #                         } else
                #                                 gg3
                #                 }
                # 
                #                 xy <-
                #                 {
                #                         if (miscgis::is_pct(sf_rx_bar()[['NAME_FCT']])) {
                #                                 y +
                #                                         scale_x_continuous(labels = scales::percent)
                #                         } else if (is.numeric(sf_rx_bar()[['NAME_FCT']]) &
                #                                    min(sf_rx_bar()[['NAME_FCT']], na.rm = TRUE) >
                #                                    1) {
                #                                 gg3 +
                #                                         scale_x_continuous(labels = scales::comma)
                #                         } else
                #                                 y
                #                 }
                #                 gg4 <- xy
                # 
                #         }
                # 
                # 
                #         gg <- gg4
                # 
                #         g <-
                #                 ggplotly(gg) %>% layout(
                #                         dragmode = "select",
                #                         margin = list(
                #                                 l = 60,
                #                                 r = 50,
                #                                 b = 50,
                #                                 t = 50
                #                         ),
                #                         font = list(family = "Open Sans",
                #                                     size = 16)
                #                 ) %>% config(displaylogo = FALSE,
                #                              displayModeBar = FALSE)
                # 
                #         build <- plotly_build(g)
                # 
                # })
                
        }
