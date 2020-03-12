library(shiny)
library(tidyverse)
library(newsanchor)
library(lubridate)
library(newsanchor)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stopwords)
library(plotly)
library(sf)
library(tmap)
library(shinydashboard)


# DASHBOARD
# ---------

ui <- fluidPage(
  navbarPage("Datathon 2020",
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Interact with the data"),
                          helpText("In order to get an idea about the data that is present in the different datasets we visualize the information
                                   in the following dashboard. We make use of interactive maps and allow the user of the data to select the 
                                   data that they want to explore."),
                          
                          dateInput(inputId = "startDateOutliers",
                                    label = "Select the begin date",
                                    value = today()-days(10),
                                    format = "yyyy/mm/dd"),
                          
                          dateInput(inputId = "endDateOutliers",
                                    label = "Select the end date",
                                    value = today(),
                                    format = "yyyy/mm/dd"),
                          
                          helpText("Use this slider to determine which hour is shown"),
                          
                          sliderInput(inputId = "hourSlider",
                                      label = "Select the hour",
                                      value = 12,
                                      min = 9,
                                      max = 17),
                          
                          selectInput(inputId = "category",
                                      label = "Select the category you want to display",
                                      choices = c("Pedestrians", "Cars", "Bikers", "Lorries"),
                          ),
                          submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                          
                        ),
                        mainPanel(
                          h1("Visual representation of the dataset"),
                          textOutput("numberOfSensors"),
                          tableOutput("summarybox"),
                          div("The following graph represents the data collected by the Telraam application. 
                              Telraam works by giving Raspberry Pi-microcomputers to citizens that put these devices in front of a
                              window. Afterwards, the device measures the amount of people end vehicules passing by. Then,
                              the data is send to a central database through a Wi-Fi connection. Installing the device at home is 
                              completely voluntary."),
                          br(),
                          textOutput("categoryDisplayed"),
                          plotOutput("trafficmap"),
                          br(),
                          div("In order to identify different outliers we make use of a time series decomposition and identify those values that heavily influence
                             the seaonality or trend. The result of this analysis is summarized in the following graph. The red dots are flagged observations meaning
                             that there is some proof that these values are outliers."),
                          br(),
                          plotlyOutput("plotlyplot"),
                          br(),
                          div("The outliers identified in the graph are summarized in the following table: "),
                          br(),
                          tableOutput("outlierTable")
                        )
                        
                      )),
             tabPanel(
               "News component",
               sidebarLayout(
                 sidebarPanel(
                   h3("Interact with the data"),
                   helpText("In order to further explore the outliers detected, it can be useful to look at relevant news in the area.
                             This dashboard allows you to pick a time interval and see all the news for Leuven. Data is gathered from 52
                             different news sources and summarized into a wordcloud and a bar graph showing the frequency of the terms.
                             Since Leuven is home to a well-known research institution, the results are filtered to not contain data
                             about the university of Leuven."),
                   
                   dateInput(inputId = "startDate",
                             label = "Select the begin date",
                             value = "2020/02/01",
                             format = "yyyy/mm/dd"),
                   dateInput(inputId = "endDate",
                             label = "Select the end date",
                             value = "2020/02/19",
                             format = "yyyy/mm/dd"),
                   
                   helpText("Input words you don't want to show up in the results of the analysis. Note that stopwords or other obvious words
                            are already automatically ommited."),
                   
                   textInput(inputId = "textinput",
                             label = "Fill in word"),
                   submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                   
                 ),
                 mainPanel(
                   h1("News event analysis"),
                   div("In order to be able to analyse the relevant news story the following graphs and tables are supplied:
                       (1) a wordcloud indicating the most important words in the news, (2) a frequency table indicating the frequency
                       of certain words in the news and (3) a calendar with relevant events organized within the city of Leuven."),
                   div("The worldcloud gives a general overview of what kind of words were used in the news stories during the selected interval. 
                       It also gives an indication about the relative frequency of the words. The worldloud is created in a way that it does not include
                       stopwords or obvious terms like by example 'Leuven'."),
                   
                   plotOutput("wordcloud"),
                   div("A wordcloud does not give any information about the absolute frequency of the words so that is why a bar chart is also included. This bar chart
                       shows the frequency of the 10 most frequent words within the dataset."),
                   plotOutput("graph"),
                   br(),
                   div("Certain important events in Leuven might not be covered in depth by the news, that is why data from the event calendar is also included.
                       This table shows the events in Leuven that were organized during the interval that was selected by the user."),
                   br(),
                   tableOutput("eventtable")
                 )
               )
             ),
             tabPanel(
               "What's next?",
               sidebarLayout(
                 sidebarPanel(
                   helpText("The analysis that has been shown in this shiny dashboard is only the beginning. There are many more features that can be added and due to limitations
                            with the use of API's we did not manage to use the full potential of the data. This page contains steps that can be taken to implement the results and to 
                            get maximum value out of this prototype.")
                 ),
                 mainPanel(
                   h1("What's next?"),
                   h2("Improvements"),
                   div("In this analysis data from 52 different news sources were used; however, most of these news sources are sources that cover global news. It would be much better if local newspapers
                     can be used. These newspapers often don't have API's to autmatically update the data, but very often they do have facebook pages. Business can make use of the facebook API
                     to scrape data from the social network. This API is not available for students so we could not implement it; however, we know it has a very similar implementation
                     as the news API."),
                   br(),
                   htmlOutput("facebookapi"),
                   br(),
                   div("Many effrots have been put into gathering the data and combining them in a single dashboard; however, more data sources can be used in the future.
                       One such source is the Leuven data that was also provided to the participants of the datathon. This dataset contains data about the amount of fine particles
                       in the air within the city of Leuven. Combining this data with the data used in this dashboard will allow the user to get a more holistic overview."),
                   br(),
                   htmlOutput("animation"),
                   div("Using these new sources, more relevant information can be presented to the user. Another point of improvement is the analysis of the text data. At the moment the keywords are identified
                     based on a wordcloud and a frequency plot; however, more state of the art techniques exist (e.g Rapid Automatic Keyword Extraction). The problem we experienced with these algorithms
                     is that they identified small common words as keywords instead of nouns. Further input tweaking is required; however, for the Dutch language this is not easy. Also, only basic methods are used
                     to filter the news stories. More advanced techniques can also be developed to filter the news stories in such a way that only the relevant ones are displayed to the user."),
                   div("Implementing the methods shown in this dashboard into the existing system is very cheap and relatively straightforward. The cost for the news API would be $500 per year. The main cost
                     for implementation will be man hours."),
                   h2("The team"),
                   htmlOutput("team"),
                   br(),
                   div("Back: Francisco Javier Jara Ávila"),
                   div("Front: Ruben Kerkhofs, Jack Xia, Mitja Briscik, Zachary Jones"),
                   br(),
                   br(),
                   br(),
                   br()
                   
                   
                   
                   
                 )
               )
             )
  )
)


server <- function(input, output) {
  load("leuven_segments.Rdata")
  a <- c("2020/02/22", "Rectavit Cyclocross")
  b <- c("2020/02/23", "Belgisch Kampioenschap karate")
  c <- c("2020/02/19", "Il Traditore")
  d <- c("2020/02/19", "Komedie Politika")
  e <- c("2020/02/19", "Concerten LUCA")
  f <- c("2020/02/19", "Doe-Beurs")
  g <- c("2020/02/01", "De prozaïsten")
  h <- c("2020/02/08", "Box of Chocolates - Improvisatietheater")
  i <- c("2020/01/27", "Bingonamiddag")
  j <- c("2020/01/19", "OHL - Westerlo")
  k <- c("2020/02/01", "OHL - KSV Roeselare")
  l <- c("2020/02/08", "OHL - Lommel SK")
  m <- c("2020/02/08", "OHL - Union")
  n <- c("2020/02/21", "OHL - Union")
  events <- as.data.frame(rbind(a, b, c,d,e,f,g,h,i,j,k,l,m,n))
  colnames(events) <- c("Date", "Event Name")
  events <- events %>%
    mutate(Date = as.POSIXct(Date))
  
  api_key = "3d070e4fc13b4035b0de0460f6ed1da0"
  
  # Necessary to split iup because the API only permits 100 articles per query
  articlesNL1 <- get_everything_all("Leuven", language = "nl",
                                       from = today() - days(10), 
                                       api_key = api_key)[[2]] %>%
    mutate(description = tolower(description)) %>%
    mutate(filterOut = ifelse(grepl("universiteit", description, fixed = TRUE), F, T)) %>%
    filter(filterOut) %>%
    distinct(title, .keep_all = TRUE)
  articlesNL2 <- get_everything_all("Leuven", language = "nl",
                                       from = today() - days(20),
                                       to = today() - days(10),
                                       api_key = api_key)[[2]] %>%
    mutate(description = tolower(description)) %>%
    mutate(filterOut = ifelse(grepl("universiteit", description, fixed = TRUE), F, T)) %>%
    filter(filterOut) %>%
    distinct(title, .keep_all = TRUE)
  articlesNL3 <- get_everything_all("Leuven", language = "nl",
                                       from = today() - days(27),
                                       to = today() - days(20),
                                       api_key = api_key)[[2]] %>%
    mutate(description = tolower(description)) %>%
    mutate(filterOut = ifelse(grepl("universiteit", description, fixed = TRUE), F, T)) %>%
    filter(filterOut) %>%
    distinct(title, .keep_all = TRUE)
  
  articlesNL <- rbind(articlesNL1, articlesNL2, articlesNL3)
  
  leuven_segments_date <- leuven_segments_sf %>% mutate(date_with_no_hour = date(date))
  ts_cars <- leuven_segments_date %>% group_by(segment_id, date_with_no_hour) %>% summarise(total_y = sum(car.x)) %>% as.data.frame()
  
  
  
  
  output$wordcloud <- renderPlot({
    articles <- articlesNL %>%
      filter(articlesNL$published_at %within% interval(as.POSIXct(input$startDate), as.POSIXct(input$endDate)))
    words <- c()
    
    for(i in articles$description) {
      words <- c(words, i)
    }
    docs <- Corpus(VectorSource(words)); docs <- tm_map(docs, content_transformer(tolower));docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, c(stopwords("dutch"), input$textinput)); docs <- tm_map(docs, removeWords, c("leuven", stopwords())) 
    docs <- tm_map(docs, removePunctuation); docs <- tm_map(docs, stripWhitespace)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  
  
  output$graph <- renderPlot({
    articles <- articlesNL %>%
      filter(articlesNL$published_at %within% interval(as.POSIXct(input$startDate), as.POSIXct(input$endDate)))
    words <- c()
    
    for(i in articles$description) {
      words <- c(words, i)
    }
    docs <- Corpus(VectorSource(words)); docs <- tm_map(docs, content_transformer(tolower));docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removeWords, c(stopwords("dutch"), input$textinput)); docs <- tm_map(docs, removeWords, c("leuven", stopwords())) 
    docs <- tm_map(docs, removePunctuation); docs <- tm_map(docs, stripWhitespace)
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
            col ="lightblue", main ="Most frequent words",
            ylab = "Word frequencies")
  })
  
  
  output$summarybox <- renderTable({
    leuven_segments_date %>%
      filter(date(date) %within% interval(input$startDateOutliers,input$endDateOutliers))  -> lsdsmall
    output <- as.data.frame(matrix(ncol = 5, nrow = 2))
    firstRow <- c(sum(lsdsmall$pedestrian.x) , sum(lsdsmall$car.x), sum(lsdsmall$bike.x), sum(lsdsmall$lorry.x))
    
    lsdsmall %>% group_by(segment_id) %>%
      summarise(sumP = sum(pedestrian.x), sumC = sum(car.x), sumB = sum(bike.x), sumL = sum(lorry.x)) -> sumTable
    secondrow <- c(mean(sumTable$sumP) , mean(sumTable$sumC), mean(sumTable$sumB), mean(sumTable$sumL))
    
    output[1,2:5] <- firstRow 
    output[2,2:5] <- secondrow
    output[1,1] <- "Total"
    output[2,1] <- "Mean over segments"
    colnames(output) <- c("Description", "Pedestrians", "Cars", "Bikes", "Lorries")
    output
  })
  
  
  
  output$graphInfo <- renderText({
    paste("You have selected the interval between ", input$startDate, " and ", input$endDate,".", sep = "")
  })
  
  
  output$eventtable <- renderTable({
    events %>%
      filter(events$Date %within% interval(as.POSIXct(input$startDate), as.POSIXct(input$endDate))) %>%
      mutate(Date = as.character(Date))
  } )
  
  
  
  output$trafficmap <- renderPlot({
    if (input$category == "Pedestrians") {sizeVar <- "pedestrian.x"} else if (input$category == "Cars") {sizeVar <- "car.x"} else if (input$category == "Lorries") {sizeVar <- "lorry.x"} else {sizeVar <- "bike.x"}
    # DATA DOES NOT UPDATE AUTOMATICALLY SO WE USE OLD DATA, WHEN THE PRODUCT WAS PRESENTED IT USED THE LAST AVAILABLE DATA
    sample <- filter(leuven_segments_sf, date(date) %within% interval(as.Date("2020-02-01"),as.Date("2020-02-20"))) %>%
      mutate(hours = hour(date)) %>% 
      filter(hours == input$hourSlider)
    sample_sf <- st_as_sf(sample)
    tm_shape(shape_lines_leuven) + tm_polygons() + tm_shape(sample_sf) + tm_dots(size = 0.075)
  })
  
  
  
  output$plotlyplot <- renderPlotly( {
    test_ts_cars <- filter(ts_cars, segment_id == '506176') %>% select(-c(segment_id, geometry))
    ts_cars_as_ts <- test_ts_cars %>% zoo::read.zoo()
    outliers <- forecast::tsoutliers(ts_cars_as_ts)
    outliers_frame <- ts_cars_as_ts[outliers$index] %>% as.data.frame() %>% rownames_to_column()
    names(outliers_frame) <- c('date', 'total')
    test_ts_cars %>%
      mutate(date_with_no_hour = as.Date(date_with_no_hour)) -> test_ts_cars
    outliers_frame %>%
      mutate(date = as.Date(date)) -> outliers_frame
    colnames(outliers_frame) <- c("Date", "Total")
    colnames(test_ts_cars) <- c("Date", "Total")
    cocky_plot <- ggplot(test_ts_cars, aes(x = Date, y = Total)) + 
      geom_line() + geom_point(data = outliers_frame, mapping = aes(x = Date, y = Total), color = 'red') +
      theme_minimal() +
      labs(x = "Date", y = "Total")
    ggplotly(cocky_plot)
  })
  
  
  output$outlierTable <- renderTable( {
    test_ts_cars <- filter(ts_cars, segment_id == '506176') %>% select(-c(segment_id, geometry))
    ts_cars_as_ts <- test_ts_cars %>% zoo::read.zoo()
    outliers <- forecast::tsoutliers(ts_cars_as_ts)
    outliers_frame <- ts_cars_as_ts[outliers$index] %>% as.data.frame() %>% rownames_to_column()
    names(outliers_frame) <- c('date', 'total')
    outliers_frame
  })
  
  output$facebookapi <-
    renderText({
      c(
        '<img src="',
        "https://drive.google.com/uc?export=view&id=1v5mHUVqvpGHvlf2obgzf-zon_Iv1QlUL",
        '">'
      )
    })
  
  output$team <-
    renderText({
      c(
        '<img src="',
        "https://drive.google.com/uc?export=view&id=1FM3wg5VuDKxY_yQFbXiOl4V8J9BERwar",
        '">'
      )
    })
  
  output$animation <-
    renderText({
      c(
        '<img src="',
        "https://drive.google.com/uc?export=view&id=1OADZaCNPP3mLur4_a3eVCyxu0ePdoBLc",
        '">'
      )
    })
  
  output$numberOfSensors <- renderText({
    leuven_segments_date %>%
      filter(date(date) %within% interval(input$startDateOutliers,input$endDateOutliers)) %>%
      count(segment_id) %>% nrow() -> number
    paste("This data is generated by collecting data from ", number, " sensors spread around Leuven.")
  })
  
  output$categoryDisplayed <- renderText({
    paste("The data that is displayed comes from the category:  ", input$category,".", sep = "")
  })
  
  
}

my_app <- shinyApp(ui = ui, server = server)
runApp(my_app)
