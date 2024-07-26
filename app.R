## NEO PI-R APP for TESTCENTRAL scoring (v.13) 
# GH Gist 7f716a3de9f07ed05cd20a417a1cedf4
# Suporta datele in format: 1) TD,D,N,A,TA  2) td,d,n,a,ta  3) 0,1,2,3,4 
# Transforma in 1,2,3,4 si - pt date lipsa

library(shiny)
library(dplyr)
library(rhandsontable)
library(DT)
library(caroline)

##################################### Fix Download Button #####################################

# Workaround for Chromium Issue 468227    # dropped because downloadHandler doesnt work properly in shiny live
# https://shinylive.io/r/examples/#r-file-download
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

##################################### DF for Hottable #####################################
make_DF <- function(n) {
    DF <- data.frame(matrix(ncol = 241, nrow = n))
    DF <- as_tibble(DF)
    names <- c("Nume", c(sprintf("i%01d", seq(1,240))))
    colnames(DF) <- names
    DF$Nume <- as.character(DF$Nume)
    DF[,2:241] = apply(DF[,2:241], 2, function(x) as.character(x));
    return(DF)
}

##########################################################################################
####################################### UI ###############################################
ui <- fluidPage(
  
  
  titlePanel("App NEO PI-R"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Inserare Date",
                       br(),
                       sliderInput("nrDF", "Numar de randuri in baza de date:",
                                   min = 0, max = 500,
                                   value = 30),
                       br(),
                       rHandsontableOutput("hotable1"), 
                       br()
              ), 
              
              tabPanel("Date Calcul", 
                       br(),
                       DT::dataTableOutput('tbl'),
                       br(),
                       downloadButton("downloadData", "Download")
              )
              
  )
)

##########################################################################################
########################################### SERVER #######################################
server <- function(input, output, session) {
  
  
  values <- reactiveValues()
  
  observe({
    values$DF <- make_DF(input$nrDF)
  })
  
#################################### Render Hotttable ####################################
  
  output$hotable1 <- renderRHandsontable({ rhandsontable(values$DF, height = 700, selectCallback = TRUE, readOnly = FALSE) })
  
  dbhotable <-  reactive({
    if(is.null(input$hotable1)){return(values$DF)}
    else if(!identical(values$DF,input$hotable1)){
      as.data.frame(hot_to_r(input$hotable1))
    }
  })
  
####################################### Recode ###########################################
  
  ### TEST CENTRAL (codare 1-5)
  transform <- reactive({
    ScMan <- dbhotable()
    indexItemiNEO <- colnames(ScMan[ ,c(2:241)])
      
    ScMan <- 
      ScMan %>%
        dplyr::mutate_at(vars(indexItemiNEO), 
                  list(
                    ~dplyr::case_when(
                      . %in%  c("TD", "td", "0") ~ "1",
                      . %in%  c("D", "d", "1") ~ "2",
                      . %in%  c("N", "n", "2") ~ "3",
                      . %in%  c("A", "a", "3") ~ "4",
                      . %in%  c("TA", "ta", "4") ~ "5",
                      . %in%  c("NA", "na") ~ "-",
                      TRUE ~ "-"
                    )
                  )
        )
    
    ScMan
    
  }) 
  

  output$tbl = DT::renderDataTable(server = FALSE,  # server=FALSE ca sa save tot tabelul, nu doar ce vede clientul
    datatable(data = transform(), 
              extensions = 'Buttons', 
              options = list(
                dom = 'lfrtBp',                   # lBfrtip e ok, dar inlocuim i cu B pt e inutil
                buttons = c('csv', 'excel')
              ) 
    )
  )
  
  # Downloadable .txt 
  output$downloadData <- downloadHandler(
    filename = paste("NEO PI-R Ap ", gsub(":", "-", format(Sys.time(), "%d-%b-%Y %H.%M")),".txt",sep=""),
    content = function(file) {
      caroline::write.delim(transform(), file, row.names = FALSE, sep = "")
    }
  )
  

}

#########################################################################################

 shinyApp(ui = ui, server = server)
