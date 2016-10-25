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
library(plyr)
library(dplyr)
library(shinydashboard)
library(leaflet)

library(shmodules)

proj_light_grey <- col2hex("grey75")
proj_grey <- col2hex("grey50")
proj_dark_grey <- col2hex("grey25")
proj_orange <- '#D59C40'

linkedBarMapSidebarTabContentUI <-
        function (id, menu_item_name, tab_name, sp) {
                ns <- NS(id)
                df <- as.data.frame(sp@data)
                shiny::req(df)
                cond_tab <- paste0("input.menu == '", tab_name, "'")
                
                tagList(conditionalPanel(condition = cond_tab,
                                         navbarPage(
                                                 "",
                                                 tabPanel(
                                                         title = "Explore",
                                                         fluidRow(width = 12,
                                                                  columnStyle(
                                                                          width = 9,
                                                                          selectizeInput(
                                                                                  inputId = ns("y_axis"),
                                                                                  label = "Select a variable (Y Axis):",
                                                                                  selected = names(df)[[2]],
                                                                                  choices = names(df)
                                                                          )
                                                                  )),
                                                         fluidRow(width = 12, plotlyOutput(ns("bar"),
                                                                                           width = "auto")),
                                                         fluidRow(width = 12,
                                                                  fluidRow(
                                                                          column(
                                                                                  width = 12,
                                                                                  selectizeInput(
                                                                                          inputId = ns("x_axis"),
                                                                                          label = "X Axis:",
                                                                                          selected = names(df)[[1]],
                                                                                          choices = names(df)
                                                                                  )
                                                                          )
                                                                  ))
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
                                                                                             "Divergent", "Qualitative")
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
                  df,
                  sp_rx,
                  plotly_event_rx)
        {
                ns <- session$ns
                myYlOrRd <-
                        RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                sp_rx_id <- reactive({
                        sp_id <- sp_rx()
                        sp_id@data %<>% mutate(KEY = 1:nrow(.))
                        return(sp_id)
                })
                
                x_axis <- reactive({
                        input$x_axis %>% as.character() %>% toupper()
                })
                y_axis <- reactive({
                        input$y_axis %>% as.character() %>% toupper()
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
                        if (is.numeric(sp_rx()[[y_axis()]])) {
                                colorNumeric(pal_choice_rev(), sp_rx()[[y_axis()]])
                        }
                        else {
                                colorFactor(pal_choice_rev(),
                                            sp_rx()[[y_axis()]] %>%
                                                    as.character() %>% factor)
                        }
                })
                
                output$map <- renderLeaflet({
                        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                        var1 <- names(sp_rx_id())[[1]]
                        var1_type <- is.numeric(sp_rx_id()[[1]])
                        pal <- function(x) {
                                if (var1_type) {
                                        colorNumeric(myYlOrRd, sp_rx_id()[[var1]])
                                }
                                else {
                                        colorFactor(
                                                "Spectral",
                                                sp_rx_id()[[var1]] %>%
                                                        as.character() %>% factor
                                        )
                                }
                        }
                        myLflt() %>% addPolygons(
                                data = sp_rx_id(),
                                color = col2hex("white"),
                                opacity = 1,
                                weight = 0.5,
                                fillColor = pal(sp_rx_id()[[var1]]),
                                fillOpacity = 0.85,
                                smoothFactor = 0,
                                group = "main"
                        ) %>%
                                addLegend(
                                        position = "bottomleft",
                                        opacity = 0.85,
                                        pal = pal(),
                                        values = sp_rx_id()[[var1]]
                                )
                })
                observe({
                        pal <- colorpal()
                        
                        switch_labFrmt <- {
                                if (miscgis::is_pct(sp_rx_id()[[y_axis()]])) {
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
                        
                        
                        leafletProxy(ns("map")) %>% clearShapes() %>% clearControls() %>%
                                addPolygons(
                                        data = sp_rx_id(),
                                        color = col2hex("white"),
                                        opacity = 1,
                                        weight = 0.5,
                                        fillColor = pal(sp_rx_id()[[y_axis()]]),
                                        fillOpacity = 0.85,
                                        smoothFactor = 0
                                ) %>% addLegend(
                                        title = y_axis(),
                                        position = "bottomleft",
                                        opacity = 0.85,
                                        pal = pal,
                                        values = sp_rx_id()[[y_axis()]],
                                        labFormat = switch_labFrmt()
                                )
                })
                
                output$bar <- renderPlotly({
                        
                        df %<>% rename(NHOOD_ABBR = NAME)
                        
                        pal <- colorpal()
                        
                        gg1 <- {
                                ggplot(df) + geom_bar(
                                        aes(
                                                x = df[[x_axis()]],
                                                y = df[[y_axis()]],
                                                colour = df[[y_axis()]],
                                                fill = df[[y_axis()]]
                                        ),
                                        stat = "identity",
                                        alpha = .85,
                                        na.rm = TRUE
                                )
                        }
                        
                        gg2 <- {
                                if (is.numeric(df[[y_axis()]])) {
                                        gg1 +
                                                scale_color_gradientn(colors = pal_choice_rev(),na.value='transparent') +
                                                scale_fill_gradientn(colors = pal_choice_rev(),na.value='transparent')
                                } else{
                                        gg1 +
                                                scale_color_manual(values = pal(df[[y_axis()]]),na.value='transparent') +
                                                scale_fill_manual(values = pal(df[[y_axis()]]),na.value='transparent')
                                        
                                }
                        }
                        
                        
                        gg3 <- {
                                gg2 +
                                        xlab(x_axis()) +
                                        ylab(y_axis()) +
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
                                        if (miscgis::is_pct(df[[y_axis()]]) & 
                                            max(df[[y_axis()]])<.1) {
                                                
                                                gg3 +
                                                        scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                                                        geom_text(aes(x = df[[x_axis()]],
                                                                      y =df[[y_axis()]],
                                                                      label = scales::percent(df[[y_axis()]])), 
                                                                  stat = "identity",
                                                                  color = col2hex("white"),
                                                                  fontface = "bold",
                                                                  size = 2.5,
                                                                  nudge_y = .02)
                                                
                                        }else if(miscgis::is_pct(df[[y_axis()]]) &
                                                 max(df[[y_axis()]])>=.1){
                                                
                                                gg3 +
                                                        scale_y_continuous(labels = scales::percent,limits = c(0,1)) +
                                                        geom_text(aes(x = df[[x_axis()]],
                                                                      y = .05,
                                                                      label = scales::percent(df[[y_axis()]])), 
                                                                  stat = "identity",
                                                                  color = col2hex("black"),
                                                                  fontface = "bold",
                                                                  size = 2.5)
                                                
                                        }else if(is.numeric(df[[y_axis()]]) &
                                                   min(df[[y_axis()]], na.rm = TRUE) >
                                                   1) {
                                                gg3 +
                                                        scale_y_continuous(labels = scales::comma)
                                        } else
                                                gg3
                                }
                                
                                xy <-
                                {
                                        if (miscgis::is_pct(df[[x_axis()]])) {
                                                y +
                                                        scale_x_continuous(labels = scales::percent)
                                        } else if (is.numeric(df[[x_axis()]]) &
                                                   min(df[[x_axis()]], na.rm = TRUE) >
                                                   1) {
                                                gg3 +
                                                        scale_x_continuous(labels = scales::comma)
                                        } else
                                                y
                                }
                                gg4 <- xy
                                
                        }
                        
                        gg <- gg4
                        
                        g <-
                                ggplotly(gg, source = "source") %>% layout(
                                        dragmode = "select",
                                        margin = list(
                                                l = 60,
                                                r = 50,
                                                b = 50,
                                                t = 50
                                        ),
                                        font = list(family = "Open Sans",
                                                    size = 16)
                                ) %>% config(displaylogo = FALSE,
                                             displayModeBar = FALSE)
                        build <- plotly_build(g)
                        
                })
                
                
                
                
                # proj_light_grey <- col2hex("grey75")
                # proj_grey <- col2hex("grey50")
                # proj_dark_grey <- col2hex("grey25")
                # proj_orange <- "#D59C40"
                
        }
