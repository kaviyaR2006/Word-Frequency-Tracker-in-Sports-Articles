library(shiny) 
library(shinydashboard) 
library(tidytext) 
library(dplyr) 
library(ggplot2) 
library(wordcloud) 
library(readr) 
library(DT) 
library(tidyr) 
library(RColorBrewer) 
library(gridExtra) 
circle_css <- " 
.content-wrapper { 
background: radial-gradient(circle, #fce4ec, #f3e5f5, #e1bee7); 
padding: 25px; 
}
.main-header .logo, .main-header .navbar { 
background: radial-gradient(circle, #d81b60, #8e24aa); 
color: white !important; 
font-weight: bold; 
} 
.skin-purple .main-sidebar { 
background: radial-gradient(circle, #6a1b9a, #8e24aa); 
} 
.skin-purple .sidebar a { 
color: white !important; 
font-size: 16px; 
font-weight: bold; 
} 
.circle-box { 
width: 200px; 
height: 200px; 
border-radius: 50%; 
background: radial-gradient(circle, #ec407a, #ab47bc); 
color: white; 
font-weight: bold; 
font-size: 22px; 
text-align: center; 
line-height: 1.3; 
padding: 15px; 
display: flex; 
justify-content: center; 
align-items: center; 
white-space: pre-line; 
word-wrap: break-word; 
box-shadow: 0px 0px 30px rgba(0,0,0,0.3); 
} 
.circle-card { 
border-radius: 40px; 
background: rgba(255,255,255,0.7); 
padding: 25px; 
box-shadow: 0px 8px 25px rgba(0,0,0,0.2); 
} 
.circle-plot { 
border-radius: 40px; 
background: rgba(255,255,255,0.75); 
padding: 20px; 
} 
.btn-circle { 
border-radius: 40px !important; 
padding: 12px 30px !important;
background: #d81b60 !important; 
  border: none !important; 
  color: white !important; 
  font-weight: bold; 
  font-size: 16px; 
  box-shadow: 0px 0px 15px #d81b60; 
} 
" 
 
# ---------------------- UI ------------------------- 
ui <- dashboardPage( 
  skin = "purple", 
   
  dashboardHeader(title = "Word Frequency Dashboard"), 
 
  dashboardSidebar( 
    tags$head(tags$style(HTML(circle_css))), 
 
    sidebarMenu( 
      menuItem("⭕ Upload File", tabName="upload", icon=icon("upload")), 
      menuItem("⭕ Summary", tabName="summary", icon=icon("list")), 
      menuItem("⭕ Frequency Table", tabName="table", icon=icon("table")), 
      menuItem("⭕ Donut Plot", tabName="plot", icon=icon("chart-pie")), 
      menuItem("⭕ Word Cloud", tabName="cloud", icon=icon("cloud")), 
      menuItem("⭕ Bigrams", tabName="bigram", icon=icon("link")) 
    ), 
 
    br(), 
    fileInput("file", "Choose TXT / CSV", accept=c(".txt",".csv")), 
    radioButtons("type", "File Type:", 
                 choices=c("TXT"="txt", "CSV"="csv")), 
    sliderInput("maxWords", "Word Cloud Size", 20, 300, 120), 
 
    h4("⬇ Download Options"), 
    downloadButton("downloadCSV", "Download CSV", class="btn-circle"), 
    br(), br(), 
    downloadButton("downloadPNG", "Download PNG", class="btn-circle"), 
    br(), br(), 
    downloadButton("downloadPDF", "Download PDF", class="btn-circle") 
  ), 
 
  dashboardBody( 
    tabItems( 
       
      # UPLOAD TAB 
      tabItem("upload", 
        div(class="circle-card", 
            h3("📄 Uploaded File Preview"), 
            verbatimTextOutput("preview") 
        ) 
      ),
       # SUMMARY TAB 
      tabItem("summary", 
        fluidRow( 
          column(4, div(class="circle-box", textOutput("totalWords"))), 
          column(4, div(class="circle-box", textOutput("uniqueWords"))), 
          column(4, div(class="circle-box", textOutput("topWord"))) 
        ), 
        br(), 
        div(class="circle-card", tableOutput("summaryStats")) 
      ), 
 
      # TABLE TAB 
      tabItem("table", 
        div(class="circle-card", 
            h3("📘 Word Frequency Table"), 
            DTOutput("freqTable") 
        ) 
      ), 
 
      # DONUT PLOT TAB 
      tabItem("plot", 
        div(class="circle-plot", 
            h3("⭕ Donut Plot"), 
            plotOutput("freqPlot") 
        ) 
      ), 
 
      # WORD CLOUD TAB 
      tabItem("cloud", 
        div(class="circle-plot", 
            h3("🌈 Word Cloud"), 
            plotOutput("cloud") 
        ) 
      ), 
 
      # BIGRAMS TAB 
      tabItem("bigram", 
        div(class="circle-card", 
            h3("🔗 Bigram Table"), 
            DTOutput("bigramTable") 
        ) 
      ) 
    ) 
  ) 
) 
 
# ---------------------- SERVER ------------------------- 
server <- function(input, output) { 
 
  articleText <- reactive({ req(input$file) 
    if(input$type=="txt"){ 
      txt <- read_file(input$file$datapath) 
    } else { 
      df <- read_csv(input$file$datapath, show_col_types=FALSE) 
      txt <- paste(df[[1]], collapse=" ") 
    } 
    tibble(text=txt) 
  }) 
 
  output$preview <- renderText(articleText()$text) 
 
  cleanWords <- reactive({ 
    articleText() %>% 
      unnest_tokens(word, text) %>% 
      filter(!word %in% stop_words$word) %>% 
      filter(grepl("^[a-z]+$", word)) %>% 
      count(word, sort=TRUE) 
  }) 
 
  # SUMMARY TEXT 
  output$totalWords <- renderText({ 
    paste("Total Words", sum(cleanWords()$n), sep="\n") 
  }) 
 
  output$uniqueWords <- renderText({ 
    paste("Unique Words", nrow(cleanWords()), sep="\n") 
  }) 
 
  output$topWord <- renderText({ 
    paste("Top Word", cleanWords()$word[1], sep="\n") 
  }) 
 
  output$summaryStats <- renderTable({ 
    df <- cleanWords() 
    tibble( 
      Total_Words=sum(df$n), 
      Unique_Words=nrow(df), 
      Most_Frequent=df$word[1], 
      Frequency=df$n[1] 
    ) 
  }) 
 
  output$freqTable <- renderDT({ 
    datatable(cleanWords(), options=list(pageLength=10)) 
  }) 
 
  # DONUT PLOT 
  output$freqPlot <- renderPlot({ 
    df <- cleanWords() %>% top_n(10, n) 
 
    ggplot(df, aes(x=2, y=n, fill=word)) + geom_col(width=1) + 
      coord_polar(theta="y") + 
      xlim(0.2, 2.5) + 
      theme_void() + 
      scale_fill_brewer(palette="Set3") + 
      ggtitle("Top 10 Word Donut Plot") 
  }) 
 
  # WORD CLOUD 
  output$cloud <- renderPlot({ 
    df <- cleanWords() 
    wordcloud(df$word, df$n, 
              max.words=input$maxWords, 
              colors=brewer.pal(8,"Dark2")) 
  }) 
 
  # BIGRAM TABLE 
  output$bigramTable <- renderDT({ 
    articleText() %>% 
      unnest_tokens(bigram, text, token="ngrams", n=2) %>% 
      separate(bigram, c("w1","w2"), sep=" ") %>% 
      filter(!w1 %in% stop_words$word, 
             !w2 %in% stop_words$word) %>% 
      unite(bigram, w1, w2) %>% 
      count(bigram, sort=TRUE) %>% 
      datatable() 
  }) 
 
  # DOWNLOADS 
  output$downloadCSV <- downloadHandler( 
    filename="word_frequency.csv", 
    content=function(file){ 
      write.csv(cleanWords(), file, row.names=FALSE) 
    } 
  ) 
 
  output$downloadPNG <- downloadHandler( 
    filename="donut_plot.png", 
    content=function(file){ 
      png(file, width=1000, height=800) 
      df <- cleanWords() %>% top_n(10, n) 
      print( 
        ggplot(df, aes(x=2, y=n, fill=word)) + 
        geom_col(width=1) + 
        coord_polar(theta="y") + 
        theme_void() 
      ) 
      dev.off() 
    } 
  ) 
 
  output$downloadPDF <- downloadHandler( filename="word_report.pdf", 
    content=function(file){ 
      pdf(file, width=11, height=8) 
      df <- cleanWords() %>% top_n(10, n) 
      donut <- ggplot(df, aes(x=2, y=n, fill=word)) + 
        geom_col(width=1) + 
        coord_polar(theta="y") + 
        theme_void() 
      grid.arrange(donut) 
      dev.off() 
    } 
  ) 
} 
 
shinyApp(ui, server)



