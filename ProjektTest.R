
require(shiny)
require(ggplot2)
require(dplyr)
require(readr)

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

library(data.table)
library(ggrepel)

require(leaflet)
library(rgdal)
library(geojsonio)

library(rsconnect)


data<-read_csv("rollLyrics.csv")
#data<-data[1:100,]

names<-data[,"Band"]
names<-names[!duplicated(names[,c('Band')]),]
names<-names[1:221,]
choices = setNames(names,"names")

names2<- data[,"genres"]
names2<-names2[!duplicated(names2[,c('genres')]),]
choices2 = setNames(names2,"names")

data2 <- data
data2$uWords <- lengths(lapply(strsplit(data$Lyrics, split = ' '), unique))
data2$nWords <- sapply(strsplit(data$Lyrics, " "), length)

data2$Band <- as.factor(data2$Band)
data3 <- aggregate(cbind(uWords, nWords) ~ Band, FUN = mean, data = data2, 
                   na.rm = TRUE, na.action = NULL)
gen<-data[!duplicated(data[,c('Band')]),]
data3 <- merge(data3, gen, by="Band")

data4 <-aggregate(cbind(uWords, nWords) ~ genres, FUN = mean, data = data3, 
                  na.rm = TRUE, na.action = NULL)


server <- function(input, output) {
  
  #dodanie danych do mapki
  countries <- geojsonio::geojson_read("map.geojson", what = "sp")
  xartist3 <-read_csv("cc.csv")
  
  for(i in 1:length(countries)){
    countries$adm0_dif <- 0
  }
  for(i in 1:length(xartist3$c)) {
    countries$adm0_dif[countries$admin == xartist3$c[[i]] ] <- as.double(xartist3$n[[i]])
  }
  
  map <- leaflet(countries)
  
  bins <- c(0, 1, 5, 10, 20, 40, 60, 80, 100, Inf)
  pal <- colorBin(
    palette = "Blues",
    domain = countries$adm0_dif, bins = bins)
  
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    countries$admin, countries$adm0_dif
  ) %>% lapply(htmltools::HTML)
  
  map <- map %>% addPolygons(
    fillColor = ~pal(adm0_dif),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))
  #wordcloud
  output$distPlot <- renderWordcloud2({
    text <- data[data[, "Band"] == input$input1, "Lyrics"]
    text <- sapply(text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    
    docs <- Corpus(VectorSource(text))
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    df <- df[order(df$freq, decreasing = TRUE), ]
    df <- df[input$slider[1]:input$slider[2], ]
    set.seed(1234) # for reproducibility 
    wordcloud2(data=df, size=0.6, color='random-dark', widgetsize=1, minSize = 10)
    
  })
  ##################
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  #mapka
  output$mymap <- renderLeaflet({
    map
  })
  
  output$mymap2 <- renderLeaflet({
    c <- xartist3$c
    text1 <- data[data[, "Band"] == input$input1, "Lyrics"]
    
    for(i in 1:length(c)) {
      xPattern = paste(c[i], collapse="|")
      xPattern
      xDT_result2 <- data.table(text1, result=grepl(xPattern, text1$Lyrics))
      xDT_result2
      
      xartist3[i, "n"] <- nrow(xDT_result2[xDT_result2[, result] == TRUE, -"result"])
      xartist3
    }
    
    for(i in 1:length(countries)){
      countries$adm0_dif <- 0
    }
    for(i in 1:length(xartist3$c)) {
      countries$adm0_dif[countries$admin == xartist3$c[[i]] ] <- as.double(xartist3$n[[i]])
    }
    
    map1 <- leaflet(countries)
    
    bins <- c(0, 1, 5, 10, 20, 40, 60, 80, 100, Inf)
    pal <- colorBin(
      palette = "Blues",
      domain = countries$adm0_dif, bins = bins)
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
      countries$admin, countries$adm0_dif
    ) %>% lapply(htmltools::HTML)
    
    map1 <- map1 %>% addPolygons(
      fillColor = ~pal(adm0_dif),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
    map1
  })
  #wykres z przyblizaniem
  output$plot2 <- renderPlot({
    ggplot(data4, aes(nWords, uWords)) +
      geom_point()   + 
      geom_label_repel(aes(label = genres),
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                       segment.color = 'grey50') +
      theme_classic()
  })
  
  output$plot3 <- renderPlot({
    ggplot(data4, aes(nWords, uWords)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)  +  geom_label_repel(aes(label = genres),
                                                                                               box.padding   = 0.35, 
                                                                                               point.padding = 0.5,
                                                                                               segment.color = 'grey50') +
      theme_classic()
  })
  #wykres z przyblizaniem2
  output$plot2a <- renderPlot({
    ggplot(data3[data3$genres==input$input2,], aes(nWords, uWords)) +
      geom_point()  + geom_text(aes(label=Band),hjust=0,vjust=0)+
      theme_classic()
  })
  
  output$plot3a <- renderPlot({
    ggplot(data3[data3$genres==input$input2,], aes(nWords, uWords)) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) + geom_text(aes(label=Band),hjust=0,vjust=0)+
      theme_classic()
  })
  
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
    
    brusha <- input$plot2a_brush
    if (!is.null(brusha)) {
      ranges$x <- c(brusha$xmin, brusha$xmax)
      ranges$y <- c(brusha$ymin, brusha$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
    
    
  })
  
  ########
  
}
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      
      selectInput("input1","input1", choices = choices),
      selectInput("input2","input2", choices = choices2),
      
      
      sliderInput("slider", "slider", min=1, max=100, value=c(40, 60))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      tabsetPanel(type = "tabs",
                  tabPanel("Cloud", wordcloud2Output(outputId = "distPlot")),
                  tabPanel("Unique1", plotOutput("plot2", height = 300,brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE)), plotOutput("plot3")),
                  tabPanel("Unique2", plotOutput("plot2a", height = 300,brush = brushOpts(id = "plot2a_brush",resetOnNew = TRUE)), plotOutput("plot3a")),
                  tabPanel("Summary1", leafletOutput("mymap")),
                  tabPanel("Summary2", leafletOutput("mymap2"))
                  
                  
      ),
      
    )
  )
)

shinyApp(ui = ui, server = server)