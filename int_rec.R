library(shiny)
require(rPython)
require(timevis)

if (interactive()) {
  ui <- fluidPage(
            fluidRow(
              column(12, align="center", textOutput('ctime'),
                     tags$head(tags$style("#ctime{color: black;
                                         font-size: 16px;
                                         font-style: italic;
                                         }"
                     )
                     )
              )
            ),
            p(),
            p(),
             fluidRow(
               column(12, align="center", textOutput('lk_h'),
                      tags$head(tags$style("#lk_h{color: green;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      ),
                      textOutput('lk_u'),
                      tags$head(tags$style("#lk_u{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      )
               )
             ),
             p(),
             p(),
             fluidRow(
               column(12, align="center", textOutput('t6_h'),
                      tags$head(tags$style("#t6_h{color: green;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      ),
                      textOutput('t6_u'),
                      tags$head(tags$style("#t6_u{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                      )
                      )
               )
             )
  )
  
  server <- function(input, output) {
    
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
    
    # Anything that calls autoInvalidate will automatically invalidate
    # every 2 seconds.
    autoInvalidate <- reactiveTimer(10000)
    
    observe({
      # Invalidate and re-execute this reactive expression every time the
      # timer fires.
      autoInvalidate()
      
      lk_total <- 2
      t6_total <- 2
      
      lk210_rec <- get_receiv("lk210")
      lk210_rec_cur <- lk210_rec$Alive_nodes[nrow(lk210_rec)]
      
      t6_rec <- get_receiv("publicT6LB")
      t6_rec_cur <- t6_rec$Alive_nodes[nrow(t6_rec)]
      ctime <- as.character(Sys.time())
      print(ctime)
      output$ctime <- renderText({ paste("Last update", ctime) })
      output$lk_h <- renderText({ paste("Healthy nodes of LK210", lk210_rec_cur) })
      output$lk_u <- renderText({ paste("Unealthy nodes of LK210",lk_total - lk210_rec_cur) })
      output$t6_h <- renderText({ paste("Healthy nodes of T6",t6_rec_cur) })
      output$t6_u <- renderText({ paste("Unealthy nodes of T6",t6_total - t6_rec_cur) })

    })

  }
  
  shinyApp(ui, server)
}
