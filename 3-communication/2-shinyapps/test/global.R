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

linkedBarMap <- function (input, output, session, sp_rx, plotly_event_rx){
        ns <- session$ns
        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
        sp_rx_id <- reactive({
                sp_id <- sp_rx()
                sp_id@data <- cbind(KEY = 1:nrow(sp_id@data), sp_id@data)
                return(sp_id)
        })
        
        var <- reactive({
                input$var
        })
        pal_choice <- reactive({
                if (input$pal == "Sequential") {
                        pal_c <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                }
                else if (input$pal == "Divergent") {
                        pal_c <- RColorBrewer::brewer.pal(n = 10, name = "Spectral")[2:9]
                }
                else {
                        pal_c <- RColorBrewer::brewer.pal(n = 9, name = "Set1")
                }
        })
        pal_choice_rev <- reactive({
                if (input$rev) {
                        rev(pal_choice())
                }
                else pal_choice()
        })
        colorpal <- reactive({
                if (is.numeric(sp_rx()[[var()]])) {
                        colorNumeric(pal_choice_rev(), sp_rx()[[var()]])
                }
                else {
                        colorFactor(pal_choice_rev(), sp_rx()[[var()]] %>% 
                                            as.character() %>% factor)
                }
        })
        x_axis <- reactive({
                if (linked_x()) {
                        var() %>% as.character() %>% toupper()
                }
                else {
                        input$x_axis %>% as.character() %>% toupper()
                }
        })
        y_axis_linked <- reactive({
                input$y_axis_linked %>% as.character() %>% toupper()
        })
        y_axis <- reactive({
                input$y_axis %>% as.character() %>% toupper()
        })
        y_axis_control <- reactive({
                if (linked_x()) {
                        input$y_axis_linked %>% as.character() %>% toupper()
                }
                else {
                        input$y_axis %>% as.character() %>% toupper()
                }
        })
        linked_x <- reactive({
                input$linked_x
        })
        observeEvent(linked_x(), {
                if (!linked_x()) {
                        updateSelectizeInput(session = session, inputId = "y_axis", 
                                             label = "Y:", choices = names(sp_rx()), selected = y_axis_linked())
                }
                else {
                        updateSelectizeInput(session = session, inputId = "y_axis_linked", 
                                             label = "Y:", choices = names(sp_rx()), selected = y_axis())
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
                                colorFactor("Spectral", sp_rx_id()[[var1]] %>% 
                                                    as.character() %>% factor)
                        }
                }
                myLflt() %>% addPolygons(data = sp_rx_id(), color = col2hex("white"), 
                                         opacity = 1, weight = 0.5, fillColor = pal(sp_rx_id()[[var1]]), 
                                         fillOpacity = 0.85, smoothFactor = 0, group = "main") %>% 
                        addLegend(position = "bottomleft", opacity = 0.85, 
                                  pal = pal(), values = sp_rx_id()[[var1]])
        })
        observe({
                pal <- colorpal()
                if (is.null(sub())) {
                        leafletProxy(ns("map")) %>% clearShapes() %>% clearControls() %>% 
                                addPolygons(data = sp_rx_id(), color = col2hex("white"), 
                                            opacity = 1, weight = 0.5, fillColor = pal(sp_rx_id()[[var()]]), 
                                            fillOpacity = 0.85, smoothFactor = 0) %>% addLegend(position = "bottomleft", 
                                                                                                opacity = 0.85, pal = pal, values = sp_rx_id()[[var()]]) %>% 
                                removeLayersControl()
                }
                else {
                        leafletProxy(ns("map")) %>% clearShapes() %>% clearControls() %>% 
                                addPolygons(data = sp_rx_id(), color = col2hex("white"), 
                                            opacity = 1, weight = 0.5, fillColor = pal(sp_rx_id()[[var()]]), 
                                            fillOpacity = 0.85, smoothFactor = 0) %>% addLegend(position = "bottomleft", 
                                                                                                opacity = 0.85, pal = pal, values = sp_rx_id()[[var()]]) %>% 
                                clearGroup(group = "sub") %>% addPolygons(data = sub(), 
                                                                          fillOpacity = 0, color = "#00FFFF", opacity = 1, 
                                                                          group = "sub") %>% addLayersControl(baseGroups = "main", 
                                                                                                              overlayGroups = "sub", options = layersControlOptions(collapsed = TRUE)) %>% 
                                removeLayersControl()
                }
        })
        output$scatter <- renderPlotly({
                
                sea_key <- sum(nrow(sp_rx()@data),1)
                
                df <- 
                        poc_ycc_df %>% 
                        filter(NAME == 'SEA') %>% 
                        mutate(KEY = sea_key) %>% 
                        rename(NHOOD_ABBR = NAME) %>% 
                        rbind(cbind(KEY = 1:nrow(sp_rx()@data), sp_rx()@data),.)
                
                gg <- ggplot(df) + 
                        geom_bar(aes(x = df[[x_axis()]], 
                                     y = df[[y_axis_control()]],
                                     key = KEY), 
                                 fill = proj_orange, 
                                 color = proj_orange, 
                                 alpha = 1,
                                 stat = 'identity') + 
                        xlab(x_axis()) + 
                        ylab(y_axis_control()) + 
                        theme(plot.background = element_rect(fill = "transparent"), 
                              panel.background = element_rect(fill = "transparent"), 
                              text = element_text(color = "white"), axis.text = element_text(color = proj_grey), 
                              axis.ticks = element_blank(), panel.grid.major = element_line(color = proj_grey), 
                              panel.grid.minor = element_line(color = proj_grey, 
                                                              size = 2), axis.line.x = element_line(color = "white"), 
                              axis.line.y = element_line(color = "white"))
                g <- ggplotly(gg, source = "source") %>% layout(dragmode = "select",
                                                                hovermode = FALSE,
                                                                margin = list(l = 60, r = 50, b = 50, t = 50), font = list(family = "Open Sans", 
                                                                                                                           size = 16)) %>% config(displaylogo = FALSE, displayModeBar = FALSE)
                build <- plotly_build(g)
                # build$data[[1]]$text <- ""
                build$data[[1]]$text <- paste0(x_axis(), ": ", as.character(sp_rx_id()[[x_axis()]]),
                                               "<br>", y_axis_control(), ": ", as.character(sp_rx_id()[[y_axis_control()]]),
                                               "<br>")
                build
        })
        proj_light_grey <- col2hex("grey75")
        proj_grey <- col2hex("grey50")
        proj_dark_grey <- col2hex("grey25")
        proj_orange <- "#D59C40"
        sub <- reactive({
                if (is.null(plotly_event_rx())) {
                        return(NULL)
                }
                else {
                        sp_sel <- plotly_event_rx()[["key"]]
                        if (length(sp_sel) == 0) {
                                sp_sel <- "abcdefg"
                        }
                        ifelse(sp_sel %!in% sp_rx_id()[["KEY"]], return(NULL),
                               return(sp_rx_id()[which(sp_rx_id()[["KEY"]] %in%
                                                               sp_sel), ]))
                }
        })
}
