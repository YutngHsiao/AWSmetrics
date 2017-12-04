library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
require(rPython)
require(timevis)
library(leaflet)

DB_type = c('ApiName', "TableName")
instances = c("tracking", "last_packets", "raw_packets")

get_receiv <-  function(instance){
  python.load("rec.py")
  source <- python.call("aws_cw", instance)
  tmp1 <- matrix(unlist(source), nrow=length(source), byrow=T)
  tmp1 <- as.data.frame(tmp1)
  colnames(tmp1) <- c("Alive_nodes", "time")
  tmp1 <- tmp1[order(tmp1$time),]
  rownames(tmp1) <- 1:nrow(tmp1)
  tmp1$time <- as.POSIXct(tmp1$time, tz='GMT', origin='1970-01-01')
  return(tmp1)
}

get_metric <- function(type, instence, win1, win2, Namespace, MetricName, Unit, Period){
  python.load("api_latency.py")
  source <- python.call("aws_cw", type, instence, win1, win2, Namespace, MetricName, Unit, Period)
  tmp1 <- matrix(unlist(source), nrow=length(source), byrow=T)
  tmp1 <- as.data.frame(tmp1)
  colnames(tmp1) <- c("Avg_read", "time")
  tmp1 <- tmp1[order(tmp1$time),]
  rownames(tmp1) <- 1:nrow(tmp1)
  # metric1$date_time <- as.POSIXct(metric1$time, tz='GMT', origin='1970-01-01')
  MetricName = 'ConsumedWriteCapacityUnits'
  source <- python.call("aws_cw", type, instence, win1, win2, Namespace, MetricName, Unit, Period)
  tmp2 <- matrix(unlist(source), nrow=length(source), byrow=T)
  tmp2 <- as.data.frame(tmp2)
  colnames(tmp2) <- c("Avg_write", "time")
  tmp2 <- tmp2[order(tmp2$time),]
  rownames(tmp2) <- 1:nrow(tmp2)
  
  #metrics <- cbind(metric1, metric2$Avg_write)
  metrics <- merge(tmp1,tmp2,by="time", all=T)
  #metrics[is.na(metrics)] <- 0
  metrics$time <- as.POSIXct(metrics$time, tz='GMT', origin='1970-01-01')
  return(metrics)
}

get_metrics <- function(type, instence, win1, win2, Namespace, MetricName, Unit, Period){
  python.load("api_latency.py")
  source <- python.call("aws_cw", type, instence, win1, win2, Namespace, MetricName, Unit, Period)
  if (length(source)>1){
    tmp <- matrix(unlist(source), nrow=length(source), byrow=T)
    tmp <- as.data.frame(tmp)
    colnames(tmp) <- c("Avg_read", "time")
    tmp <- tmp[order(tmp$time),]
    rownames(tmp) <- 1:nrow(tmp)
  }
  tmp$time <- as.POSIXct(tmp$time, tz='GMT', origin='1970-01-01')
  return(tmp)
}

get_metrics_w <- function(type, instence, win1, win2, Namespace, MetricName, Unit, Period){
  python.load("rpy.py")
  source <- python.call("aws_cw", type, instence, win1, win2, Namespace, MetricName, Unit, Period)
  # metric1$date_time <- as.POSIXct(metric1$time, tz='GMT', origin='1970-01-01')
  MetricName = 'ConsumedWriteCapacityUnits'
  source <- python.call("aws_cw", type, instence, win1, win2, Namespace, MetricName, Unit, Period)
  tmp2 <- matrix(unlist(source), nrow=length(source), byrow=T)
  tmp2 <- as.data.frame(tmp2)
  colnames(tmp2) <- c("Avg_write", "time")
  tmp2 <- tmp2[order(tmp2$time),]
  rownames(tmp2) <- 1:nrow(tmp2)
  tmp2$time <- as.POSIXct(tmp2$time, tz='GMT', origin='1970-01-01')
  return(tmp2)
}

merge_metrics <- function(win1){
  type = 'TableName'
  win1 = win1
  win2 = 0
  Period = 60
  Namespace = 'AWS/DynamoDB'
  MetricName = c('ConsumedReadCapacityUnits', 'ConsumedWriteCapacityUnits')
  Unit = 'Count'
  for (i in instances){
    tmp <- get_metrics(type, i, win1, win2, Namespace, MetricName, Unit, Period)
  }
}

plot_dynamo <- function(win1, instance, tbname){
  type = 'TableName'
  instence = instance
  win1 = win1
  win2 = 0
  Period = 60
  Namespace = 'AWS/DynamoDB'
  MetricName = c('ConsumedReadCapacityUnits', 'ConsumedWriteCapacityUnits')
  Unit = 'Count'
  metrics <- get_metrics(type, instence, win1, win2, Namespace, MetricName, Unit, Period)
}

################ Receivers A #####################
lk210_total <- 2
lk210_rec <- get_receiv("LK")
lk210_rec_cur <- lk210_rec$Alive_nodes[nrow(lk210_rec)]
plot_rec_lk210 <- plot_ly(lk210_rec, x = ~time, y = ~Alive_nodes, name = 'Alive Nodes', type = 'bar') %>% ##'scatter', mode = 'lines', fill = "tozeroy") %>%
  layout(title = "LK210 Receivers - Number of nodes Alive",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Nodes Alive",range = c(0,5)))

################ Receivers B #####################
t6_total <- 3
t6_rec <- get_receiv("LB")
t6_rec_cur <- t6_rec$Alive_nodes[nrow(t6_rec)]
plot_rec_t6 <- plot_ly(t6_rec, x = ~time, y = ~Alive_nodes, name = 'Alive Nodes', type = 'bar', fill = "tozeroy") %>%
  layout(title = "T6 Receivers - Number of nodes Alive",
         xaxis = list(title = "Time"),
         yaxis = list(title = "Nodes Alive",range = c(0,5)))

############### UI start #############
ui <- bootstrapPage(theme = shinytheme("cerulean"),
                    navbarPage("Dev Metircs",id = "tabs",
                               
                               tabPanel("DynamoDB", value = "DynamoDB",
                                        fluidRow(
                                          tags$head(
                                            tags$style(HTML("		  
                                                            .link:hover {color:#b0d8ff;}
                                                            "))
                                            ),
                                          tags$div(style="height:100px; font-family: sans-serif;",
                                                   tags$div(style="height:70px; float:left; margin-top:10px; margin-left:20px; text-align:center;",
                                                            tags$span(style="font-size:300%; font-weight:bold; color:#47d3f4; line-height:95%", "18"),
                                                            tags$br(),
                                                            tags$span(style="color:#6d6d6d;", "Tables")
                                                   ),
                                                   tags$div(style="height:70px; float:left; margin-top:5px; margin-left:30px; text-align:center;",
                                                            tags$span(style="font-size:100%; font-weight:bold; color:#74a90d; line-height:95%", 
                                                                      sliderInput("dur_dynamo", "Time Frame (hr): ", min = 1, max = 24, value = 3)),
                                                            tags$br()
                                                   )
                                          )
                                            ),
                                        fluidRow(
                                          column(12,align="center",div(style = "background-color: #DFF8F7;"),
                                                 fluidRow(
                                                   column(12, align="center",div(style = "height:900px;", 
                                                                                 p(),
                                                                                 plotlyOutput("dynamo_plot"))
                                                   )
                                                 )
                                          )
                                        )
                               ),
                               
                               tabPanel("APIs", value = "APIs",
                                        fluidRow(
                                          tags$head(
                                            tags$style(HTML("		  
                                                            .link:hover {color:#b0d8ff;}
                                                            "))
                                            ),
                                          tags$div(style="height:100px; font-family: sans-serif;",
                                                   tags$div(style="height:70px; float:left; margin-top:10px; margin-left:20px; text-align:center;",
                                                            tags$span(style="font-size:300%; font-weight:bold; color:#47d3f4; line-height:95%", "18"),
                                                            tags$br(),
                                                            tags$span(style="color:#6d6d6d;", "Tables")
                                                   ),
                                                   tags$div(style="height:70px; float:left; margin-top:10px; margin-left:66px; text-align:center;",
                                                            tags$span(style="font-size:100%; font-weight:bold; color:#74a90d; line-height:95%", 
                                                                      selectInput(inputId="table_name", label="API Name", c("Device parser API", "Device parser API2", "Device parser API3"), width=200)),
                                                            tags$br()
                                                   ),
                                                   tags$div(style="height:70px; float:left; margin-top:5px; margin-left:30px; text-align:center;",
                                                            tags$span(style="font-size:100%; font-weight:bold; color:#74a90d; line-height:95%", 
                                                                      sliderInput("Hour", "Hour:", min = 1, max = 24, value = 3)),
                                                            tags$br()
                                                   ),
                                                   tags$div(style="height:70px; float:left; margin-top:10px; margin-left:30px; text-align:center;",
                                                            tags$span(style="font-size:100%; font-weight:bold; color:#47d3f4; line-height:95%", 
                                                                      actionButton("api", strong("Load"), style="color: #337ab7; background-color: #337ab7; border-color: #2e6da4")),
                                                            tags$br()
                                                   )
                                                   
                                                   
                                          )
                                            ),
                                        fluidRow(
                                          column(12,align="center",div(style = "background-color: #DFF8F7;"),
                                                 fluidRow(
                                                   column(12, align="center",div(style = "height:400px;", 
                                                                                 p(),
                                                                                 plotlyOutput("plot_api"))
                                                   )
                                                 )
                                          )
                                        )
                               ),
                               
                               tabPanel("Receivers",
                                        fluidRow(
                                          column(12, align="center",div(style = "height:36px;",
                                                                        p(),
                                                                        HTML(paste(tags$span(style="font-size:100%; margin-right:10px;font-weight:bold;color:red", paste("Unhealthy Nodes :", lk210_total-lk210_rec_cur)), sep = ""), "",
                                                                             paste(tags$span(style="font-size:100%; margin-left:10px;font-weight:bold;color:green", paste("Healthy Nodes : ", lk210_rec_cur)), sep = "")))
                                          )
                                        ),
                                        fluidRow(
                                          column(12, align="center",div(style = "height:410px;", plotlyOutput("plot_rec1"))
                                                 #h4(paste("Healthy Nodes : 3")), h4(paste("Unhealthy Nodes : 0")))
                                          )
                                        ), p(),
                                        fluidRow(
                                          column(12, align="center",div(style = "height:36px;",
                                                                        p(),
                                                                        HTML(paste(tags$span(style="font-size:100%; margin-right:10px;font-weight:bold;color:red", paste("Unhealthy Nodes :", t6_total-t6_rec_cur)), sep = ""), "",
                                                                             paste(tags$span(style="font-size:100%; margin-left:10px;font-weight:bold;color:green", paste("Healthy Nodes : ", t6_rec_cur)), sep = "")))
                                          )
                                        ),
                                        fluidRow(
                                          column(12, align="center",div(style = "height:360px;", plotlyOutput("plot_rec2"))
                                          )
                                        )
                               ),
                               
                               tabPanel("Santry",
                                        fluidRow( column(12, dataTableOutput('df')))
                                        
                               ),
                               tabPanel("Summary", uiOutput("lout"))
                               )
                    )
############### UI end #############


############### Server start #############
server <- function(input, output, session) {
  observeEvent(input$dur_dynamo, {
    dynamo_plot <- subplot(t6_plot, t6_plot, last_plot, nrows = 3)
    
    output$dynamo_plot <- renderPlotly({
      dynamo_plot
    })
    
  })
  
  
  observeEvent(input$api, {
    type = 'TableName'
    instence = "tracking"
    win1 = 180
    win2 = 0
    Period = 60
    Namespace = 'AWS/DynamoDB'
    MetricName = 'ConsumedReadCapacityUnits'
    Unit = 'Count'
    metrics <- get_metrics(type, instence, win1, win2, Namespace, MetricName, Unit, Period)
    plot_api <- plot_ly(metrics, x = ~time, y = ~Avg_read, name = 'Read Capacity', type = 'scatter', mode = 'lines', fill = "tozeroy") %>%
      add_trace(y = ~Avg_write, name = 'Write Capacity', line = list(color = 'rgb(255,128,0)', width = 2, fill = "tozeroy"))%>%
      layout(title = "DynamoDB - Workload of Tracking Table",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Consumed Capacity Units"))
    output$plot_api <- renderPlotly({
      plot_api
    })
  })
  
  
  
  output$plot_rec1 <- renderPlotly({
    plot_rec_lk210
  })
  output$plot_rec2 <- renderPlotly({
    plot_rec_t6
  })
}

############### Server end #############

shinyApp(ui, server)
