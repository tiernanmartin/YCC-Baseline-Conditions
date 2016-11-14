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

linkedBarMapSidebarTabContentUI1 <- function (id, menu_item_name, tab_name, sp){
        ns <- NS(id)
        df <- as.data.frame(sp@data)
        shiny::req(df)
        cond_tab <- paste0("input.menu == '", tab_name, "'")
        # cond_linked_x_F <- sprintf("input['%s'] == false", ns("linked_x"))
        # cond_linked_x_T <- sprintf("input['%s'] == true", ns("linked_x"))
        tagList(conditionalPanel(condition = cond_tab, 
                                 navbarPage("", 
                                            tabPanel(title = "Explore", 
                                                     fluidRow(width = 12, 
                                                              columnStyle(width = 12, 
                                                                          selectizeInput(inputId = ns("y_axis"), 
                                                                                         label = "Select a variable",
                                                                                         selected = names(df)[[1]],
                                                                                         choices = names(df))), 
                                                     fluidRow(width = 12, 
                                                              plotlyOutput(ns("bar"), 
                                                                           width = "auto")), 
                                                     fluidRow(width = 12, 
                                                              fluidRow(column(width = 12, 
                                                                              selectizeInput(inputId = ns("x_axis"),
                                                                                             label = "X:",
                                                                                             selected = names(df)[[1]],
                                                                                             choices = names(df)))))
                                                              )), 
                                            tabPanel(title = "Style", 
                                                     fluidRow(width = 12, 
                                                              columnStyle(width = 9, 
                                                                          selectizeInput(inputId = ns("pal"), 
                                                                                         label = "Select a color palette", 
                                                                                         choices = c("Sequential", 
                                                                                                     "Divergent", 
                                                                                                     "Qualitative"))), 
                                                              columnStyle(width = 3, 
                                                                          checkboxInputStyle(inputId = ns("rev"), 
                                                                                             label = "Reverse", 
                                                                                             value = FALSE, 
                                                                                             cssStyle = "padding: 0px;"), 
                                                                          cssStyle = "padding: 0px;"))))))
}

linkedBarMap1 <- function (input, output, session, sp_rx, bar_df, plotly_event_rx){
        ns <- session$ns
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        sp_rx_id <- reactive({
                sp_id <- sp_rx()
                return(sp_id)
        })
        
        y_axis <- reactive({
                input$y_axis %>% as.character() %>% toupper()
        })
        
        x_axis <- reactive({
                input$x_axis %>% as.character() %>% toupper()
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
                else pal_choice()
        })
        
        colorpal <- reactive({
                if (is.numeric(sp_rx()[[y_axis()]])) {
                        colorNumeric(pal_choice_rev(), sp_rx()[[y_axis()]])
                }
                else {
                        colorFactor(pal_choice_rev(), sp_rx()[[y_axis()]] %>% 
                                            as.character() %>% factor)
                }
        })
        
        
        output$map <- renderLeaflet({
                miscgis::myLfltGrey()
                # # myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                # var1 <- names(sp_rx())[[1]]
                # var1_type <- is.numeric(sp_rx()[[1]])
                # pal <- colorpal()
                # 
                # myLflt() %>% addPolygons(data = sp_rx(), 
                #                          color = col2hex("white"), 
                #                          opacity = 1, 
                #                          weight = 0.5, 
                #                          fillColor = pal(sp_rx()[[var1]]), 
                #                          fillOpacity = 0.85, smoothFactor = 0, group = "main") %>% 
                #         addLegend(position = "bottomleft", opacity = 0.85, 
                #                   pal = pal(sp_rx()[[var1]]), values = sp_rx()[[var1]])
        })
        
        # observe({
        #         pal <- colorpal()
        #         leafletProxy(ns("map")) %>% 
        #                 clearShapes() %>% 
        #                 clearControls() %>% 
        #                 addPolygons(data = sp_rx(), 
        #                             color = col2hex("white"), 
        #                             opacity = 1, 
        #                             weight = 0.5, 
        #                             fillColor = pal(sp_rx()[[y_axis()]]), 
        #                             fillOpacity = 0.85, 
        #                             smoothFactor = 0) %>% 
        #                 addLegend(position = "bottomleft", 
        #                           opacity = 0.85, 
        #                           pal = pal, 
        #                           values = sp_rx()[[y_axis()]]) %>% 
        #                 removeLayersControl()
        # })
        
        output$bar <- renderPlotly({
                
                df <- bar_df %>% rename(NHOOD_ABBR = NAME)

                pal <- {
                        if (is.numeric(df[[y_axis()]])) {
                                colorNumeric(pal_choice_rev(), df[[y_axis()]])
                        }
                        else {
                                colorFactor(pal_choice_rev(), df[[y_axis()]] %>%
                                                    as.character() %>% factor)
                        }
                }

                gg1 <- ggplot(df) +
                        geom_bar(aes(x = df[[x_axis()]],
                                     y = df[[y_axis()]],
                                     fill = df[[y_axis()]],
                                     color = df[[y_axis()]]),
                                 alpha = 1,
                                 stat = 'identity') +
                        xlab(x_axis()) +
                        ylab(y_axis()) +
                        theme(plot.background = element_rect(fill = "transparent"),
                              panel.background = element_rect(fill = "transparent"),
                              text = element_text(color = "white"), axis.text = element_text(color = proj_grey),
                              axis.ticks = element_blank(), panel.grid.major = element_line(color = proj_grey),
                              panel.grid.minor = element_line(color = proj_grey,
                                                              size = 2), axis.line.x = element_line(color = "white"),
                              axis.line.y = element_line(color = "white"))

                gg <- {
                        if(is.numeric(df[[y_axis()]])){
                                gg1 +
                                        scale_colour_continuous(low = pal(df[[y_axis()]])[[1]],
                                                                high = pal(df[[y_axis()]])[[length(df[[y_axis()]])]]) +
                                        scale_fill_continuous(low = pal(df[[y_axis()]])[[1]],
                                                                high = pal(df[[y_axis()]])[[length(df[[y_axis()]])]])
                        }else {
                                gg1 +
                                        scale_colour_manual(values = pal(df[[y_axis()]])) +
                                        scale_fill_manual(values = pal(df[[y_axis()]]))

                        }
                }
                
                g <- ggplotly(gg, source = 'source') %>%
                        layout(dragmode = 'select',
                               margin = list(
                                       l = 60,
                                       r = 50,
                                       b = 50,
                                       t = 50
                               ),
                               font = list(family = 'Open Sans', size = 16)) %>%
                        config(displaylogo = FALSE,displayModeBar = FALSE)
                
                build <- plotly_build(g)
        # proj_light_grey <- col2hex("grey75")
        # proj_grey <- col2hex("grey50")
        # proj_dark_grey <- col2hex("grey25")
        # proj_orange <- "#D59C40"
})
}
