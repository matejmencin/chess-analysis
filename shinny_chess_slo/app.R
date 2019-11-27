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
ui <- navbarPage(theme = shinytheme("simplex"),"Navigacija",
    
                 
    #  Upload data ----
    tabPanel("Nalozi podatke",
             # Side bar
             sidebarPanel(
               
               # Input physiological_file
               # helpText("Upload data of you physiological reponses."),
               # fileInput("file_phy", "File input:", 
               #           multiple = FALSE,
               #           accept = c("text/csv",
               #                      "text/comma-separated-values,text/plain",
               #                      ".csv")),
               
               helpText("Izberi kolo"),
               selectizeInput('round_pgn', 'Kolo', c(Choose = "", "1", "2", "3",
                                                      "4", "5", "6",
                                                      "7", "8", "9"), 
                              multiple = FALSE),
               
               helpText("Nalozi ''.zip'' datoteko izbranega kola."),
               fileInput("file_zip", "File input (zip):", 
                         multiple = FALSE,
                         accept = c(".zip")),
               
               # Input pgn
               # helpText("Upload .pgn data"),
               # fileInput("file_pgn", "File input:", 
               #           multiple = FALSE,
               #           accept = c(".pgn")),
               
               actionButton("upload_data", "Nalozi")
             ),
             
             # Main panel
             mainPanel(
               h1("Nalaganje podatkov in krajse informacije"),
               h3("Info"),
               p("Ta program vam omogoca, da lahko preverite svoje fizioloske odzive med vaso odigrano sahovsko partijo. Vasi fizioloski podatki so shranjeni v ''.zip'' datoteki, ki vam je bila poslana preko elektronske poste."),
               p("\n"),
               h3("Kaj je EDA ali elektrodermalna aktivnost?"),
               p("Elektrodermalna aktivnost se nanasa na elektricne spremembe (prevodnost), ki je merjene na povrsini koze. Ce pride do custvene vzburjenosti, povecane kognitivne obremenitve ali fizicnega napora, mozgani posljejo kozi signale, ki povecajo raven znojenja. In ceprav morda ne cutite nobenega znoja na povrsini koze se elektricna prevodnost poveca na merljivo pomemben nacin (prejeto s spletne strani Empatica).")
               ),
             tableOutput("zipped")
    ),
    

    # Inspect EDA ----
    tabPanel("Preveri EDA",
             # Main panel
             verticalLayout(
               titlePanel("Preveri EDA"),
               plotOutput("plotEDA"),
               wellPanel(
                 sliderInput("time_eda", 
                             label = h3("Cas"), 
                             min = as.POSIXct("15:00", format = "%H:%M"), 
                             max = as.POSIXct("23:00", format = "%H:%M"), 
                             value = c(as.POSIXct("17:15", format = "%H:%M"), 
                                       as.POSIXct("17:15", format = "%H:%M")),
                             timeFormat = "%H:%M"),
                 actionButton("update_eda", align="center", "Osvezi pogled")
               )
             )
             ),
    
    tabPanel("Preveri temperaturo",
             # Main panel
             verticalLayout(
               titlePanel("Preveri temperaturo"),
               plotOutput("plotTEMP"),
               wellPanel(
                 sliderInput("time_temp", 
                             label = h3("Cas"), 
                             min = as.POSIXct("15:00", format = "%H:%M"), 
                             max = as.POSIXct("23:00", format = "%H:%M"), 
                             value = c(as.POSIXct("17:15", format = "%H:%M"), 
                                       as.POSIXct("17:15", format = "%H:%M")),
                             timeFormat = "%H:%M"),
                 actionButton("update_temp", align="center", "Osvezi pogled")
               )
             )
    ),
    
    
    # Chess ----
    tabPanel("Sah",
             # Side bar
             sidebarPanel(
               helpText("Izberi partijo izbrano kolo"),
               selectInput("in_game", "Sahoska partija", c("None"), selectize = FALSE),
               actionButton("upload_game", "Nalozi partijo"),
               hr(),
               helpText("Premakni se na predhodno potezo."),
               actionButton("previous_move", "Premakni se potezo nazaj"),
               helpText("Premakni se na naslednjo potezo."),
               actionButton("next_move", "Premakni se potezo naprej")
               ),
             
             mainPanel(
               h1("Sahovska igra: "),
               textOutput("o_game"),
               hr(),
               p("Trenutni pogled: "),
               chessboardjsOutput('board', width = 420)
             )
    ),
    # EDA + Chess ----
    tabPanel("EDA + Sah",
             fluidRow(
               column(6, align="center",
                      "EDA (deluje zgolj za kola: 1, 5, 8, 9)"
               ),
               column(6,align="center",
                      "Sahovska poteza "
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
                 column(6, align="center", plotOutput("plotEC")),
                 column(6, align="center", chessboardjsOutput('boardEC', width = 360)))
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
               p("Eksperiment predstavlja merjenje fizioloskih odzivov sahista med sahosvko partijo. Te podatke si n.pr. zelimo korelirati s sahovskimi polozaji, casom ure, itn. Fizioloske odzive smo merili z Empatica E4. Ce imate dodatna vprasanja o eksperimentu, mi prosim posljite sporocilo po e-posti."),
               p("Pravtako bi se rad zahvalil: vam udelezencem, Ivan Bratko (mentor), Jakob Valic in Andreja Dobrovoljc."),
               p(" "),
               p("Matej Mencin")
             ))
    
    )

# Define server logic ----
server <- function(input, output, session) {

  
  
  # [S] Reactive variables ----
  
  df <- reactiveValues(
    pgn = NULL, game = NULL, play = NULL, move = NULL,
    eda = NULL, temp = NULL, eda_plot = NULL, temp_plot = NULL,
    ec_plot = NULL, ec=NULL, playEC = NULL, moveEC = NULL, 
    time_stamps = NULL, day = NULL, month = NULL, year = NULL,
    indeks = NULL) 

  # [S] Functions used in server.func ----
  
  update_graph_eda <- function(){
    # Function that update plot.
    
    output$plotEDA <- renderPlot({
    ggplot(data = df$eda_plot) +
        geom_smooth(aes(x=time, y=eda), span=0.3, color="blue", size=1, alpha=1) +
        geom_line(aes(x=time, y=eda), color="black", alpha=0.33)+
        theme_bw() +
        labs(title = "Electrodermal aktivnost (EDA) v microsiemens ("~mu~"S)",
             x = "Cas",
             y = "EDA ("~mu~"V)")
    })
  }
  
  update_graph_temp <- function(){
    # Function that update plot.
    
    output$plotTEMP <- renderPlot({
      ggplot(data = df$temp_plot) +
        geom_smooth(aes(x=time, y=temp), span=0.3, color="red", size=1, alpha=1) +
        geom_line(aes(x=time, y=temp), color="black", alpha=0.33) +
        theme_bw() +
        labs(title = "Temperature ",
             x = "Cas",
             y = ~degree~"C")
    })
  }
  update_data_ec <- function(a){
    
    t1 <- df$time_stamps[df$moveEC-1]
    indeks <- df$indeks
    t <- df$ec[,1]
    while(1==1){
      indeks <- indeks + (1 * a)
      if ((a > 0)&(as.integer(seconds(t1)) < as.integer(t[indeks]))) {
        df$indeks = indeks
        break
      }
      if ((a < 0)&(as.integer(seconds(t1)) > as.integer(t[indeks]))) {
        df$indeks = indeks
        break
      }
    }

    
    df$ec_plot <- df$ec[(indeks-240):(indeks+240),]

    
    return(0)
  }
  
  update_graph_ec <- function(){
    # Function that update plot.
    output$plotEC <- renderPlot({
      ggplot(data = df$ec_plot) +
        geom_line(aes(x=time, y=eda), color="red") +
        geom_vline(xintercept=df$ec_plot$time[181]) +
        labs(x = "Time",
             y = "EDA ("~mu~"V)")
        })
    

  }
  
  tidy_uploaded_data <- function(untidy_data, measure){
    # Functions, that tidy uploaded data.
    
    time_raw <- untidy_data[1, ] # Reading time.
    freq <- untidy_data[2, ] # Reading frequency.
    nh_eda <- untidy_data[-c(1,2), ] # Removing first two rows.
    
    # Converting and making sequence of time stamps.
    time <- as.POSIXct(untidy_data[1,], origin="1970-01-01", tz = "Europe/Prague")
    df$day <- day(time[1])
    df$month <- month(time[1])
    df$year <- year(time[1])
    time_seq <- seq.POSIXt(as.POSIXct(time), as.POSIXct((time_raw + length(nh_eda)/freq), origin="1970-01-01", tz = "Prague"), units = "seconds", by = .25)
    
    
    # Removing last rows if columns are not the same size.
    k <- length(time_seq) - length(nh_eda)
    if (k > 0) {
      time_seq <- time_seq[1:(length(time_seq)-k)]
    }
    
    # Merging data (timestamps and EDA)
    tidy_data <- data.frame(time_seq, nh_eda) 
    names(tidy_data) <- c("time", measure)
    return(tidy_data)
  }
  
  
  # [S] Upload ----
  # observeEvent(input$upload_data, {
  
  # Testing case.
  #   files <- unzip(input$file_zip$datapath, list = TRUE)
  #   
  #   eda <- read.csv(unz(input$file_zip$datapath, "EDA.csv"), header = F)
  #   temp <- read.csv(unz(input$file_zip$datapath, "TEMP.csv"), header = F)
  #   output$zipped <- renderTable({files$Name})
  # })

  
  
  observeEvent(input$upload_data, {
    
    # Uploading data.
    req(input$file_zip)
    data_eda <- read.csv(unz(input$file_zip$datapath, "EDA.csv"), header = F) # read.csv(input$file_phy$datapath, header = F)
    data_temp <- read.csv(unz(input$file_zip$datapath, "TEMP.csv"), header = F)
    
    
    df$eda <- tidy_uploaded_data(data_eda, "eda")
    df$temp <- tidy_uploaded_data(data_temp, "temp")
    df$ec <- tidy_uploaded_data(data_eda, "eda")
    
    # Old
    # time_raw <- data_eda[1, ] # Reading time.
    # freq <- data_eda[2, ] # Reading frequency.
    # nh_eda <- data_eda[-c(1,2), ] # Removing first two rows.
    
    # Converting and making sequence of time stamps.
    # time <- as.POSIXct(data_eda[1,], origin="1970-01-01", tz = "Europe/Prague")
    # time_seq <- seq.POSIXt(as.POSIXct(time), as.POSIXct((time_raw + length(nh_eda)/freq), origin="1970-01-01", tz = "Prague"), units = "seconds", by = .25)

    
    # Removing last rows if columns are not the same size.
    # k <- length(time_seq) - length(nh_eda)
    # if (k > 0) {
    #   time_seq <- time_seq[1:(length(time_seq)-k)]
    # }
    
    # Merging data (timestamps and EDA)
    # df$phy <- data.frame(time_seq, nh_eda) 
    # names(df$phy) <- c("time", "eda")
    
    df$eda_plot <- df$eda
    df$temp_plot <- df$temp
    
    # Updating plot on Inspect EDA
    update_graph_eda()
    update_graph_temp()
    
    # Updating slider on Inspect EDA
    observe(updateSliderInput(session, "time_eda", 
                              min = df$eda[1,1], 
                              max = df$eda[length(df$eda$time),1], 
                              value = c(df$eda[1,1],
                                        df$eda[length(df$eda$time),1]),
                              timeFormat = "%H:%M"))
    
    observe(updateSliderInput(session, "time_temp", 
                              min = df$temp[1,1], 
                              max = df$temp[length(df$temp$time),1], 
                              value = c(df$temp[1,1],
                                        df$temp[length(df$temp$time),1]),
                              timeFormat = "%H:%M"))
    
    observe(updateSelectInput(session, "in_game", 
                              choices = sort(paste(df$pgn[,5], df$pgn[,6], sep = " - "))))
    
    })
  
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
  

  
  # [S] Table ----
  
  
  # [S] EDA/TEMP ----
  
  observeEvent(input$update_eda, {
    min <- input$time_eda[1]
    max <- input$time_eda[2]
    df$eda_plot <- df$eda[
      row.names(df$eda[which(df$eda$time == min),]):row.names(df$eda[which(df$eda$time == max),]),]
    update_graph_eda()
  })
  
  observeEvent(input$update_temp, {
    min <- input$time_temp[1]
    max <- input$time_temp[2]
    df$temp_plot <- df$temp[
      row.names(df$temp[which(df$temp$time == min),]):row.names(df$temp[which(df$temp$time == max),]),]
    update_graph_temp()
  })
  
  # [S] Chess ----
  
  observeEvent(input$upload_game, {
    # To get right person - right game.
    tmp_string = strsplit(input$in_game, " - ")
    tmp_string = matrix(unlist(tmp_string), ncol=2, byrow=TRUE)
    
    # Some stupid string manipulation, lack of knowledge of R.
    # iksi is later used to determine the game.
    iksi <- as.integer(row.names(df$pgn[which(df$pgn$White == tmp_string[1, 1]),]))
    iksi <- df$pgn$Round[iksi]
    iksi <- as.numeric(substr(iksi, 3, nchar(iksi)))
    print(iksi)
    
    # Indeks for time stamp
    df$indeks <- 1
    # Loading time stamps.
    data_dir <- paste("./data/round", as.character(input$round_pgn) ,".csv", sep="")
    print(data_dir)
    data_time_stamps <- read.csv(file = data_dir, header = TRUE, sep = ";")

    
    x <- iksi # Change
    time_stamps <- matrix(unlist(strsplit(as.character(data_time_stamps$time[x]), ";")))
    time_stamps <- as.POSIXct(time_stamps, origin="1970-01-01", format="%H:%M:%S", tz = "Europe/Prague")

    for (i in 1:length(time_stamps)){
      # time_stamps[i] <- as.POSIXct(time_stamps[i],format="%H:%M:%S")
      day(time_stamps[i]) <- df$day
      month(time_stamps[i]) <- df$month
      year(time_stamps[i]) <- df$year
    }
    df$time_stamps <- time_stamps
    
    
    # Loading chess data
    # Old: game = row.names(df$pgn[which(df$pgn$Round == input$in_game),])
    game = row.names(df$pgn[which(df$pgn$White == tmp_string[1, 1]),])
    # For Chess tab
    df$move <- 2
    df$play <- tibble(stringsAsFactors = FALSE)
    # For EDA + Chess tab
    df$moveEC <- 2
    df$playEC <- tibble(stringsAsFactors = FALSE)
    df$game <- Chess$new()
    play_pgn <- Chess$new()
    df$game$load_pgn(df$pgn[game, "Movetext"])
    
    history <- df$game$history(verbose = TRUE)
    for (move in 1:nrow(history)){
      df$play <- rbind(df$play, toString(play_pgn$fen()))
      df$playEC <- rbind(df$play, toString(play_pgn$fen()))
      play_pgn$move(history[move,6])
    }
    
    # Update graphs on EDA + Chess
    df$indeks <- 1
    update_data_ec(1)
    update_graph_ec()

    
    output$o_game <- renderText({
      paste("Sahovska partija med: ", df$pgn[game, "White"], 
            " (beli) in ", df$pgn[game, "Black"], 
            " (crni).")
    })
    
    output$board <- renderChessboardjs({
      chessboardjs(toString(df$game$fen()))
    })
    
    output$boardEC <- renderChessboardjs({
      chssfen <- Chess$new()
      chessboardjs(toString(chssfen$fen()))
    })
  })
  
  observeEvent(input$next_move,{
    if(df$move < dim(df$play)[1]){
      df$move <- df$move + 1
    }
    chssfen <- Chess$new()
    fen <- df$play[df$move,]
    chssfen$load(toString(fen))
    output$board <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    
  })
  
  observeEvent(input$previous_move,{
    if(df$move > 2){
      df$move <- df$move - 1
    }
    chssfen <- Chess$new()
    fen <- df$play[df$move,]
    chssfen$load(toString(fen))
    output$board <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    
  })
  
  observeEvent(input$nextEC,{
    if(df$moveEC < dim(df$playEC)[1]){
      df$moveEC <- df$moveEC + 1
      update_data_ec(1)
      update_graph_ec()
    }
    chssfen <- Chess$new()
    fen <- df$playEC[df$moveEC,]
    chssfen$load(toString(fen))
    output$boardEC <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    
  })
  
  observeEvent(input$previousEC,{
    if(df$moveEC > 2){
      df$moveEC <- df$moveEC - 1
      update_data_ec(-1)
      update_graph_ec()
    }
    chssfen <- Chess$new()
    fen <- df$playEC[df$moveEC,]
    chssfen$load(toString(fen))
    output$boardEC <- renderChessboardjs({
      chessboardjs(toString(chssfen$fen()))})
    
  })
  


}

# Run the app ----
shinyApp(ui = ui, server = server)

