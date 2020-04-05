# Used libaries
library(shiny)
library(shinythemes)
library(tidyverse)
library(tibble)
library(lubridate)
library(rchess)
library(bigchess)
library(knitr)
library(markdown)

options(encoding="utf-8")


# Define UI ----
# Defined UI for the website.
ui <- navbarPage(theme = shinytheme("simplex"),"Navigacija",
    

    #  Upload data ----
    tabPanel("Nalozi podatke",
             # Side bar
             sidebarPanel(
               helpText("Izberi kolo"),
               selectizeInput('round_pgn', 'Kolo', c(Izberi = "", "1", "2", "3",
                                                      "4", "5", "6",
                                                      "7", "8", "9"), 
                              multiple = FALSE),
               helpText("Nalozi ''.zip'' datoteko, ki vsebuje vase fizioloske meritve izbranega kola."),
               fileInput("file_zip", "File input (zip):", 
                         multiple = FALSE,
                         accept = c(".zip")),
               actionButton("upload_data", "Podatke poslji v obdelavo"),
               hr(),
               helpText("Izberi partijo izbrano kolo"),
               selectInput("in_game", "Sahovska partija", c("None"), selectize = FALSE),
               actionButton("upload_game", "Nalozi partijo"),
               hr()
             ),
             
             # Main panel
             mainPanel(
               h1("O programu"),
               h3("Info"),
               p("S programom, lahko preverite svoje fizioloske odzive za izbrano sahovsko partijo. Vsi fizioloski podatki (EDA, temperatura in povprecni srcni utrip) so shranjeni v ''.zip'' datoteki, ki vam je bila poslana preko elektronske poste. Pomembno je, da izberete ustrezno kolo in partijo."),
               p("\n"),
               h3("Kaj je EDA ali elektrodermalna aktivnost?"),
               p("Elektrodermalna aktivnost se nanasa na elektricne spremembe (prevodnost), ki je merjena na povrsini koze. Prevodnost koze se stalno spremenija, do vecjih sprememb pa pride, kadar smo custveno vzburjeni, kognitivno obremenjeni ali pri fizicnem naporu. V takih primerih mozgani posljejo ''kozi'' signal, naj poveca raven znojenja, s cimer se poveca elektricna prevodnost koze na merljivo pomemben nacin (spletna strana Empatica).")
               ),
             tableOutput("zipped")
    ),


  
    # Inspect EDA ----
tabPanel("Preveri fizioloske odzive",
verticalLayout(
  titlePanel("Fizioloski odzivi"),
  wellPanel(
    sliderInput("time_eda", 
                label = h3("Cas"), 
                min = as.POSIXct("15:00", format = "%H:%M", tz="Etc/GMT-1"), 
                max = as.POSIXct("23:00", format = "%H:%M", tz="Etc/GMT-1"), 
                value = c(as.POSIXct("17:15", format = "%H:%M", tz="Etc/GMT-1"), 
                          as.POSIXct("17:15", format = "%H:%M", tz="Etc/GMT-1")),
                timeFormat = "%H:%M"),
    actionButton("update_eda", align="center", "Osvezi pogled")
  ),
  plotOutput("plotEDA",  height = "280px", width='100%'),
  plotOutput("plotTEMP", height = "280px", width='100%'),
  plotOutput("plotHR", height = "280px", width='100%')
  
)
),
    # EDA + Chess ----
    tabPanel("EDA + Sah",
             fluidRow(
               column(6, align="center",
                      textOutput("o_moves")
               ),
               
               column(6,align="center",
                      textOutput("o_game")
               )
             ),
             fluidRow(
               column(6, align="center",
                      hr()
               ),
               column(6,align="center",
                      hr()
               )
             ),
             fluidPage(
               fluidRow(
                 column(6, align="center", 
                        tabsetPanel(
                          tabPanel("EDA", 
                                   plotOutput("plotEC")
                                   ),
                          tabPanel("Temperatura", 
                                   plotOutput("plotECtemp")
                                   ),
                          tabPanel("Srcni utrip", 
                                   plotOutput("plotEChr")
                          )
                        )
                        
                        ),
                 column(6, align="center",
                   textOutput("ticktack"),
                   hr(),
                   chessboardjsOutput('boardEC', width = 360)))
            ),
            fluidRow(
              column(6, align="right",
                     actionButton("previousEC", "Premakni se potezo nazaj")
              ),
              column(6,align="left",
                     actionButton("nextEC", "Premakni se potezo naprej")
              )
            )
      ),
             
             
    # About ----
    tabPanel("O eksperimentu",
             mainPanel(
               h1("O eksperimentu"),
               p("Eksperiment predstavlja merjenje fizioloskih odzivov sahista med sahosvko partijo.  Vase (fizioloske) podatke bomo v prihodnosti analizirali in korelirali s sahovskimi polozaji, casom ure, itn. in sklepali o stresu, ki ga je posameznik dozivljal v danem trenutku, saj je znano, da je povecan EDA odziv povezan s stresom in kognitivno obremenitijo. V kolikor imate dodatna vprasanja eksperimentu ali potrebujete pomoc pri aplikaciji mi prosim posljite sporocilo po e-posti. Poskusal vam bom odgovoriti v najboljsi moci."),
               p("Ob tej priloznosti bi se tudi ze rad zahvalil: vam udelezencem, Ivan Bratko (mentor), Jakob Valic in Andreja Dobrovoljc."),
               p(" "),
               p("Matej Mencin")
             ))
    
    )

# Define server logic ----
server <- function(input, output, session) {

  output$boardEC <- renderChessboardjs({
    # Chess board on EDA + Sah
    
    chssfen <- Chess$new()
    chessboardjs(toString(chssfen$fen()))
  })
  
  # [S] Reactive variables ----
  # Reactive variables are global variables.
  
  df <- reactiveValues(
    pgn = NULL, game = NULL, play = NULL, move = NULL,
    eda = NULL, temp = NULL, eda_plot = read.csv(text="time,eda"), temp_plot = read.csv(text="time,temp"),
    ec_plot = read.csv(text="time,hr"), ec=NULL, playEC = NULL, moveEC = NULL, 
    time_stamps = NULL, day = NULL, month = NULL, year = NULL, chess_time = NULL,
    indeks = NULL, hr = NULL, hr_plot = read.csv(text="time,hr")) 

  # [S] Functions used in server.func ----
  
  update_graph_eda <- function(){
    # Function that update plot on site Preveri EDA.
    
    output$plotEDA <- renderPlot({
    ggplot(data = df$eda_plot) +
        #geom_smooth(aes(x=time, y=eda), span=0.3, color="blue", size=1, alpha=1) +
        geom_line(aes(x=time, y=eda), color="seagreen3", alpha=1) +
        theme_bw() +
        labs(title = "Electrodermal aktivnost (EDA) v microsiemens ("~mu~"S)",
             x = "Cas",
             y = "EDA ("~mu~"V)")+
        scale_x_datetime(timezone="Etc/GMT-2")
    })
  }
  
  update_graph_temp <- function(){
    # Function that update plot on site Preveri temperaturo.
    
    output$plotTEMP <- renderPlot({
      ggplot(data = df$temp_plot) +
        #geom_smooth(aes(x=time, y=temp), span=0.3, color="red", size=1, alpha=1) +
        geom_line(aes(x=time, y=temp), color="orange2", alpha=1) +
        theme_bw() +
        labs(title = "Temperatura ",
             x = "Cas",
             y = ~degree~"C")+
        scale_x_datetime(timezone="Etc/GMT-2")
    })
  }
  
  update_graph_hr <- function(){
    # Function that update plot on site Preveri srcni utrip.
    
    output$plotHR <- renderPlot({
      ggplot(data = df$hr_plot) +
        #geom_smooth(aes(x=time, y=hr), span=0.3, color="green", size=1, alpha=1) +
        geom_line(aes(x=time, y=hr), color="tomato2", alpha=1) +
        theme_bw() +
        labs(title = "Srcni utrip ",
             x = "Cas",
             y = "Povprecni srcni utrip")+
        scale_x_datetime(timezone="Etc/GMT-2")
    })
  }
  

  update_data_ec <- function(a){
    # Function that UPDATE DATA for plot on site EDA + Sah
    

    if (length(df$time_stamps) < (df$moveEC-1)) {
      t1 <- df$time_stamps[length(df$time_stamps)]  
    } else {
      t1 <- df$time_stamps[df$moveEC-1]
    }
    indeks <- df$indeks
    t <- df$ec[,1]
    
    if(a == 0){

      while(1==1){
        indeks <- indeks + 1
        if ((as.integer(seconds(t1)) < as.integer(t[indeks]))) {
          df$indeks = indeks
          break
        }
      }
    }
    if(a != 0){

      difference <- as.integer(as.integer(seconds(t1)) - as.integer(t[indeks]))
      indeks <- indeks + (difference * 4) 
      df$indeks <- indeks 
    }
    
    indeks <- as.integer(indeks/4) - 10
    
    # Chaning the plots.
    df$ec_plot <- df$ec[(df$indeks-240):(df$indeks+240),] # We always take [-240 to 240] from the move
    df$ec_plottemp <- df$temp[(df$indeks-240):(df$indeks+240),]
    df$ec_plothr  <- df$hr[(indeks-60):(indeks+60),]
    
    return(0)
  }

  
  update_graph_ec <- function(){
    # Function that update plot EDA + Sah.
    
    output$plotEC <- renderPlot({
      ggplot(data = df$ec_plot) +
        geom_line(aes(x=time, y=eda), color="seagreen3", size=1) +
        geom_vline(xintercept=df$ec_plot$time[241], linetype="dotted", alpha=0.42, size=1) +
        labs(x = "Cas (crtkana crta prikazuje cas ob premiku)",
             y = "EDA ("~mu~"V)")+
        scale_x_datetime(timezone="Etc/GMT-2")
        })
  }
  
  update_graph_ec_temp <- function(){
    # Function that update plot EDA + Sah.
    
    output$plotECtemp <- renderPlot({ # OK
      ggplot(data = df$ec_plottemp) + # OK
        geom_line(aes(x=time, y=temp), color="orange2", size=1) + # OK
        geom_vline(xintercept=df$ec_plottemp$time[241], linetype="dotted", alpha=0.42, size=1) + # OK
        labs(x = "Cas (crtkana crta prikazuje cas ob premiku)",
             y = ~degree~"C")+
        scale_x_datetime(timezone="Etc/GMT-2")
    })
  }
  
  update_graph_ec_hr <- function(){
    # Function that update plot EDA + Sah.
    
    output$plotEChr <- renderPlot({ # OK
      ggplot(data = df$ec_plothr) + # OK
        geom_line(aes(x=time, y=hr), color="tomato2", size=1) + # OK
        geom_vline(xintercept=df$ec_plothr$time[61], linetype="dotted", alpha=0.42, size=1, label='Premik') + # OK
        labs(x = "Cas (crtkana crta prikazuje cas ob premiku)",
          y = "Povprecni srcni utrip")+
        scale_x_datetime(timezone="Etc/GMT-2")
    })
  }
  
  tidy_uploaded_data <- function(untidy_data, measure){
    # Functions, that tidy uploaded data of phy measurments..
    
    time_raw <- untidy_data[1, ] # Reading time.
    freq <- as.integer(untidy_data[2, ]) # Reading frequency.
    nh_eda <- untidy_data[-c(1,2), ] # Removing first two rows.
    
    # Converting and making sequence of time stamps.
    time <- as.POSIXct(untidy_data[1,], origin="1970-01-01", tz = "Etc/GMT-2")
    df$day <- day(time[1])
    df$month <- month(time[1])
    df$year <- year(time[1])
    time_seq <- seq.POSIXt(as.POSIXct(time), as.POSIXct((time_raw + length(nh_eda)/freq), origin="1970-01-01", tz = "Etc/GMT-2"), units = "seconds", by = 1/freq)
    
    
    # Removing last rows if columns are not the same size.
    k <- length(time_seq) - length(nh_eda)
    if (k > 0) {
      time_seq <- time_seq[1:(length(time_seq)-k)]
    }
    
    # Merging data (timestamps and phy)
    tidy_data <- data.frame(time_seq, nh_eda) 
    names(tidy_data) <- c("time", measure)
    return(tidy_data)
  }
  

  observeEvent(input$upload_data, {
    
    # Uploading data.
    req(input$file_zip)
    data_eda <- read.csv(unz(input$file_zip$datapath, "EDA.csv"), header = F) # read.csv(input$file_phy$datapath, header = F)
    data_temp <- read.csv(unz(input$file_zip$datapath, "TEMP.csv"), header = F)
    data_hr <- read.csv(unz(input$file_zip$datapath, "HR.csv"), header = F)
    
    # Tidying data.
    df$eda <- tidy_uploaded_data(data_eda, "eda")
    df$temp <- tidy_uploaded_data(data_temp, "temp")
    df$ec <- tidy_uploaded_data(data_eda, "eda")
    df$hr <- tidy_uploaded_data(data_hr, "hr")
    
    # Plot variables
    df$eda_plot <- df$eda
    df$temp_plot <- df$temp
    df$hr_plot <- df$hr
    
    
    # Updating slider on Inspect EDA
    # We always have to update sliders after we upload data. Before some dummy values.
    observe(updateSliderInput(session, "time_eda", 
                              min = df$eda[1,1], 
                              max = df$eda[length(df$eda$time),1], 
                              value = c(df$eda[1,1],
                                        df$eda[length(df$eda$time),1]),
                              timeFormat = "%H:%M"))
    
    observe(updateSelectInput(session, "in_game", 
                              choices = sort(paste(df$pgn[,5], df$pgn[,6], sep = " - "))))
    
    })
  # Updating plot on Inspect EDA

  update_graph_eda()

  update_graph_temp()

  update_graph_hr()
  
  observeEvent(input$upload_data,{
    
    # Uploading data.
    # req(input$file_pgn)
    data_round <- paste("./data/round", input$round_pgn, ".pgn", sep="")
    df$pgn <- read.pgn(data_round, #"./data/round7.pgn", # input$file_pgn$datapath
                   add.tags = c("Site", "Date", "Round", 
                                "White", "Black", "Result", 
                                "TimeControl", "WhiteElo", 
                                "BlackElo"))

  })
  

  
  # [S] EDA/TEMP ----
  
  observeEvent(input$update_eda, {
    # Button on Preveri EDA
    
    min <- input$time_eda[1]
    max <- input$time_eda[2]
    
    if (min == df$eda[1,1]){
      df$hr_plot <- df$hr[
        1:row.names(df$eda[which(df$eda$time == max),]),]
    } else if (max == df$eda[length(df$eda$time),1]){
      df$hr_plot <- df$hr[
        row.names(df$hr[which(df$hr$time == min),]):length(df$hr$time),]
    } else if ((min == df$eda[1,1]) & max == (df$eda[length(df$eda$time),1])){
      df$hr_plot <- df$hr
    } else{
      df$hr_plot <- df$hr[
        row.names(df$hr[which(df$hr$time == min),]):row.names(df$hr[which(df$hr$time == max),]),]
    }
    
    df$eda_plot <- df$eda[
      row.names(df$eda[which(df$eda$time == min),]):row.names(df$eda[which(df$eda$time == max),]),]
    df$temp_plot <- df$temp[
      row.names(df$temp[which(df$temp$time == min),]):row.names(df$temp[which(df$temp$time == max),]),]

    update_graph_eda()
    update_graph_temp()
    update_graph_hr()
  })
  
  # [S] Chess ----
  
  observeEvent(input$upload_game, {
    # String manipulation to get right person - right game.
    tmp_string = strsplit(input$in_game, " - ")
    tmp_string = matrix(unlist(tmp_string), ncol=2, byrow=TRUE)
    
    # Some stupid string manipulation, a little bit a lack of knowledge of R.
    # Temporal variable iksi is as well later used to determine the game.
    iksi <- as.integer(row.names(df$pgn[which(df$pgn$White == tmp_string[1, 1]),]))
    iksi <- df$pgn$Round[iksi]
    iksi <- as.numeric(substr(iksi, 3, nchar(iksi)))
    # print(iksi)
    
    
    # Loading time stamps.
    data_dir <- paste("./data/round", as.character(input$round_pgn) ,".csv", sep="")
    data_time_stamps <- read.csv(file = data_dir, header = TRUE, sep = ";")
    
    # Loading chess time.
    data_dir <- paste("./data/round", as.character(input$round_pgn) ,"-ctimes.csv", sep="")
    data_chess_time <- read.csv(file = data_dir, header = TRUE, sep = ";")
    
    
    x <- iksi # Change
    time_stamps <- matrix(unlist(strsplit(as.character(data_time_stamps$time[x]), ";")))
    time_stamps <- as.POSIXct(time_stamps, origin="1970-01-01", format="%H:%M:%S", tz = "Etc/GMT-2")
    
    for (i in 1:length(time_stamps)){
      # time_stamps[i] <- as.POSIXct(time_stamps[i],format="%H:%M:%S")
      day(time_stamps[i]) <- df$day
      month(time_stamps[i]) <- df$month
      year(time_stamps[i]) <- df$year
    }
    df$time_stamps <- time_stamps
    
    x <- iksi # Change
    chess_time <- matrix(unlist(strsplit(as.character(data_chess_time$time[x]), ";")))
    chess_time <- as.POSIXct(chess_time, origin="1970-01-01", format="%H:%M:%S", tz = "")
    
    for (i in 1:length(chess_time)){
      day(chess_time[i]) <- df$day
      month(chess_time[i]) <- df$month
      year(chess_time[i]) <- df$year
    }
    df$chess_time <- chess_time
    
    
    # Loading chess data
    # Old: game = row.names(df$pgn[which(df$pgn$Round == input$in_game),])
    game = row.names(df$pgn[which(df$pgn$White == tmp_string[1, 1]),])
    
    # For Chess tab
    df$move <- 2
    df$play <- tibble(stringsAsFactors = FALSE)
    
    # For EDA + Sah tab
    df$moveEC <- 2
    df$playEC <- tibble(stringsAsFactors = FALSE)
    df$game <- Chess$new()
    play_pgn <- Chess$new()
    df$game$load_pgn(df$pgn[game, "Movetext"])
    
    # Calculating all fen moves.
    history <- df$game$history(verbose = TRUE)
    for (move in 1:nrow(history)){
      df$play <- rbind(df$play, toString(play_pgn$fen()))
      df$playEC <- rbind(df$play, toString(play_pgn$fen()))
      play_pgn$move(history[move,6])
    }
    
    # Update graphs on EDA + Chess
    df$indeks <- 1
    update_data_ec(0) # A variable 0 means that we are looking next move.
    update_graph_ec()
    update_graph_ec_temp()
    update_graph_ec_hr()
    
    
    output$o_game <- renderText({
      # Text on on Sah (Main window)
      paste("Sahovska partija med: ", df$pgn[game, "White"], 
            " (beli) in ", df$pgn[game, "Black"], 
            " (crni).")
    })
    
    output$o_moves <- renderText({
      # Text on on Sah (Main window)
      paste("Fizioloski odzivi za potezo. (", 
            strftime(df$time_stamps[df$moveEC-1], format = "%H:%M:%S", tz="Etc/GMT-2"),")")
    })
    
    output$ticktack <- renderText({
      # Text on on Sah (Main window)
      if((df$moveEC-1) == 1){
        df$belicas <- strftime(df$chess_time[df$moveEC-1], format = "%H:%M:%S")
        df$crnicas <- strftime(df$chess_time[df$moveEC-1], format = "%H:%M:%S")
      }
      
      if((df$moveEC-1) %% 2 == 0){
        df$belicas <- strftime(df$chess_time[df$moveEC], format = "%H:%M:%S")
      }
      else{
        df$crnicas <- strftime(df$chess_time[df$moveEC], format = "%H:%M:%S")
      }
      paste("Ura beli: ", 
            df$belicas," - Ura crni: ", df$crnicas)
    })
    
    
    output$boardEC <- renderChessboardjs({
      # Chess board on EDA + Sah
      
      chssfen <- Chess$new()
      chessboardjs(toString(chssfen$fen()))
    })
  })
  
  
  observeEvent(input$nextEC,{
    # Button on EDA + Sah
    # print(df$moveEC)
    # print(dim(df$playEC)[1])
    if(df$moveEC < dim(df$playEC)[1]){
      df$moveEC <- df$moveEC + 1
      update_data_ec(1) # 1 = next move
      update_graph_ec()
      update_graph_ec_temp()
      update_graph_ec_hr()
    }
    chssfen <- Chess$new() # Creating an empty chess board
    fen <- df$playEC[df$moveEC,] # Setting up fen position.
    chssfen$load(toString(fen))  # Setting up fen position.
    output$boardEC <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    #print(df$game$history(verbose = TRUE)[df$moveEC-2,6])
    
    output$o_moves <- renderText({
      # Text on on Sah (Main window)
      paste("Fizioloski odzivi za potezo: ", as.integer((df$moveEC-1)/2), 
            ".  ", (df$game$history(verbose = TRUE)[df$moveEC-2,6]), 
            "   (", strftime(df$time_stamps[df$moveEC-1], format = "%H:%M:%S", tz="Etc/GMT-2"),")")
    })
    #print(df$chess_time[df$moveEC-1])
  })
  
  observeEvent(input$previousEC,{
    # Button on EDA + Sah
    
    if(df$moveEC > 2){
      df$moveEC <- df$moveEC - 1
      update_data_ec(-1) # -1 = previous move
      update_graph_ec()
      update_graph_ec_temp()
      update_graph_ec_hr()
    }
    chssfen <- Chess$new()
    fen <- df$playEC[df$moveEC,] # Setting up fen position.
    chssfen$load(toString(fen)) # Setting up fen position.
    output$boardEC <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    #print(df$game$history(verbose = TRUE)[df$moveEC-2,6])
    
    
    output$o_moves <- renderText({
      # Text on on Sah (Main window)
      paste("Fizioloski odzivi za potezo: ", as.integer((df$moveEC-1)/2), 
            ".  ", (df$game$history(verbose = TRUE)[df$moveEC-2,6]), 
            "   (", strftime(df$time_stamps[df$moveEC-1], format = "%H:%M:%S", tz="Etc/GMT-2"),")")
    })
    
    if(df$moveEC == 2){
      output$o_moves <- renderText({
        # Text on on Sah (Main window)
        paste("Fizioloski odzivi za potezo (", 
              strftime(df$time_stamps[df$moveEC-1], format = "%H:%M:%S", tz="Etc/GMT-2"),")")
      })
    }
    #print(df$chess_time[df$moveEC-1])
  })
  
  


}

# Run the app ----
shinyApp(ui = ui, server = server)

