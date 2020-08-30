## -------------stock-tweets-----------
# Authors        : Syed M. Fuad
# Data           : Twitter developer API keys and tokens 
# New Techniques : Scraping, Sentiment Analysis
#--------

#get tweets and tweet sentiments from users data object
#stock_tweets

#--- loading required libraries ---

library(shiny)
library(pacman)
pacman::p_load(rtweet, reactable, glue, stringr, httpuv, dplyr, purrr, forcats, ggplot2, graphTweets, 
               igraph, opencage, RColorBrewer, readr, rnaturalearth, sf, tidyr, visNetwork, adegenet, 
               tm, ggpubr, tidytext, textdata, lubridate, SentimentAnalysis, glue, plotly, syuzhet) 

#--- function to make twitter links clickable ---

make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

#--- adding ui controls ---

ui <- fluidPage(
  
  #--- application title ---
  titlePanel("Stock Tweets"),
  
  #--- user inputs ---
  sidebarLayout(
    sidebarPanel(
      textInput("ticker_to_search",
                "Ticker to search (for multiple tickers, separate them by a space):",
                value = "SE"),
      
      numericInput("num_tweets_to_download",
                   "Number of tweets to download:",
                   min = 100,
                   max = 18000,
                   value = 50,
                   step = 50),
      
      dateRangeInput("date_picker", label = "Select dates:", start = "2020-01-27", 
                     end = as.character(Sys.Date())),
      
      #--- scrap tweets from user specified language ---
      
      selectInput("language", "Language", choices=list("English"="en", "French"="fe", "Spanish"="spa",
                                                       "Chinese"="chi", "Hindi"="hin", "Arabic"="ara",
                                                       "Bengali"="ben", "Russian"="rus", "Portuguese"="por",
                                                       "Indonesian"="ind", "German"="ger", "Japanese"="jpn"), 
                  selected="en"),
      
      #--- filters tweets by either popularity, date tweeted or a combination of both ---
      
      selectInput("type", "Type of search result", choices=list("Recent"="recent", "Mixed"="mixed", 
                                                                "Popular"="popular"), 
                  selected="recent"),
      
      checkboxInput("bin", 
                    "Include retweets",
                    value=FALSE),
      
      actionButton("get_data", "Get data", class = "btn-primary"),
      br(),br(),
      
      downloadButton("download_data", "Download data"),
      
      width=3
      
    ),
    
    #--- show results ---
    mainPanel(
      tabsetPanel(
        tabPanel("Tweet table", reactableOutput("tweet_table")),
        tabPanel("Network graph", visNetworkOutput("network_graph")),
        tabPanel("Popular tickers", reactableOutput("popular_tweet_table")),
        tabPanel("Sentiment graphs", plotlyOutput("sentiment_graph")),
        tabPanel("Sentiment frequency chart", plotOutput("sentiment_frequency")),
        tabPanel("Interactive sentiment frequency chart", plotOutput("sentiment_frequency_plot"))
      )
    )
  )
)

#--- define server logic ---

server <- function(input, output) {
  
  #--- gets data from twitter --- 
  
  tweet_df <- eventReactive(input$get_data, {
    
    #--- creates vector from user specified tweets and adds $ before tickers ---
    
    var2 <- strsplit(input$ticker_to_search, " ")
    var3 <- unlist(var2)
    var4 <- paste0("$", var3)
    var5 <- paste0(var4, collapse=" OR ")
    
    search_tweets(
      var5, n = input$num_tweets_to_download, include_rts = input$bin, lang=input$language, type=input$type)
  })
  
  tweet_table_data <- reactive({
    
    #--- filters only selected columns ---
    
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, symbols, is_retweet, retweet_screen_name, 
             status_id, favorite_count, retweet_count, urls_expanded_url, user_id, location) %>%
      filter(between(as.Date(created_at), input$date_picker[1], input$date_picker[2])) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Symbols = symbols, RT = is_retweet, 
             RT_name = retweet_screen_name, ID = status_id, Likes = favorite_count, RTs = retweet_count, 
             URLs = urls_expanded_url, UserID = user_id, Location = location)
  })
  
  output$tweet_table <- renderReactable({
    
    #--- table output containing tweet, date created, user handle, urls ---
    
    reactable::reactable(tweet_table_data(), 
                         filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                         showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                         columns = list(
                           DateTime = colDef(defaultSortOrder = "asc", minWidth = 100),
                           User = colDef(defaultSortOrder = "asc", minWidth = 100),
                           Tweet = colDef(html = TRUE, minWidth = 300, resizable = TRUE),
                           Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE), minWidth=70),
                           RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE), minWidth=70),
                           URLs = colDef(html = TRUE, minWidth = 200)
                         )
    )
  })
  
  output$download_data <- downloadHandler(
    
    #--- downloads the tweet table in csv format --- 
    
    filename = function() {
      paste(input$ticker_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweet_table_data(), file, row.names = FALSE)
    }
  )
  
  network_graph_data <- reactive({
    
    #--- generates network graph data --- 
    
    network_data <- tweet_table_data() %>% filter(RT == TRUE) %>% gt_edges(User, RT_name) %>% gt_nodes() %>% gt_graph()
    edges_col <- igraph::edge_attr(network_data, name = "n") %>%
      cut(breaks = seq(0, 120, 20), labels = 1:6)
    network_data <- set_edge_attr(graph = network_data, name = "width", value = 2^(as.numeric(edges_col)))
    network_data <- set_vertex_attr(graph = network_data, name = "value", value = vertex_attr(network_data, name = "n"))
    network_data
    
  })
  
  output$network_graph <- renderVisNetwork({
    
    #--- generates network graph ---
    
    visIgraph(network_graph_data()) %>%
      visLayout(randomSeed = 42, improvedLayout = TRUE) %>%
      visEdges(
        color = list(
          color = greenpal(6)[5]
        ) #removed comma 
      ) %>%
      visNodes(
        color = list(
          background = "#A3A3A3",
          border = "##4D4D4D",
          highlight = "#ff0000",
          hover = "#00ff00"
        ),
        font =
          list(
            size = 40
          )
      )
  })
  
  output$popular_tweet_table <- renderReactable({
    
    #--- table output with most popular tickers ---
    
    reactable::reactable(tweet_table_data() %>% filter(!RT) %>% unnest(Symbols) %>% 
                           mutate(Tickers = tolower(Symbols)) %>% filter(!is.na(Tickers)) %>% 
                           count(Tickers) %>% arrange(desc(n)) %>% top_n(10, n) 
                         #%>%
                         #select(Tickers = Symbols_unr, Count = n)
    )
  })
  
  sentiment_graph_data <- reactive({
    
    #--- generates data for sentiment bar plot ---
    
    tidy_text <- tweet_table_data() %>% unnest_tokens(word, Tweet)
    
    #--- construct a dataframe with stop words ---
    
    my_stop_words <- tibble( 
      word = c(
        "https",
        "t.co",
        "rt",
        "amp",
        "rstats",
        "gt"
      ),
      lexicon = "twitter"
    )
    
    all_stop_words <- stop_words %>%
      bind_rows(my_stop_words) 
    
    no_numbers <- tidy_text %>%
      filter(is.na(as.numeric(word))) 
    
    no_stop_words <- no_numbers %>%
      anti_join(all_stop_words, by = "word")
    
    nrc <- get_sentiments("nrc")
    nrc_words <- no_stop_words %>%
      inner_join(nrc, by="word")
    
    pie_words<- nrc_words %>%
      group_by(sentiment) %>% 
      tally(name="Count") %>% 
      arrange(desc(n)) 
    
    pie_words
    
  })
  
  output$sentiment_graph <- renderPlotly({
    
    #--- generates sentiment bar plot --- 
    
    plot_ly(pie_words, x=~sentiment, y=~n, type="bar", color=~sentiment) %>%
      layout(xaxis=list(title=""), showlegend=FALSE,
             title="Interactive sentiment frequency plot")
    
  })
  
  tweet_data <- reactive({
    
    #--- generates data for time series of sentiment category (negative, neutral and positive) --- 
    
    req(tweet_table_data())
    
    tweet_data <- tweet_table_data() %>%
      select(DateTime, Tweet)
    
    #--- removes unnecessary symbols --- 
    
    tweet_data$Tweet <- tweet_data$Tweet %>% {gsub("^[[:space:]]*","", .)} %>% {gsub("[[:space:]]*$","", .)} %>%
    {gsub(" +"," ", .)} %>% {gsub("'", "%%", .)} %>% {iconv(., "latin1", "ASCII", sub="")} %>%
    {gsub("<(.*)>", "", .)} %>% {gsub("\\ \\. ", " ", .)} %>% {gsub("  ", " ", .)} %>% {gsub("%%", "\'", .)} %>%
    {gsub("%%", "\'", .)} %>% {gsub("https(.*)*$", "", .)} %>% {gsub("\\n", "-", .)} %>% {gsub("--", "-", .)} %>%
    {gsub("&amp;", "&", .)}
    
    sentiment <- analyzeSentiment(tweet_data$Tweet)
    sentiment2 <- sentiment$SentimentQDAP
    sentiment3 <- convertToDirection(sentiment$SentimentQDAP)
    
    #tweet_data$Date <- day(tweet_data$DateTime)
    #tweet_data$Time <- hour(tweet_data$DateTime)
    
    df <- data.frame(sent2 = sentiment2, sent3 = sentiment3, hour = tweet_data$DateTime)
    df$sent2 <- as.numeric(df$sent2)
    df$sent3 <- as.numeric(df$sent3)
    
    df$time <- format(round(df$hour, units="hours"), format="%H:%M")
    
    freq <- df %>% 
      group_by(time, sent3) %>% 
      summarise(Freq=n())
    
    freq$fact <- ifelse(freq$sent3==1, "Negative", ifelse(freq$sent3==2, "Neutral", "Positive"))
    
    freq
    
  })
  
  output$sentiment_frequency <- renderPlot({
    
    #--- generates time series of sentiment category (negative, neutral and positive) ---
    
    freq2 <- tweet_data()
    
    ggplot() + 
      geom_bar(mapping = aes(x = freq2$time, y = freq2$Freq, fill = freq2$fact), stat = "identity") +
      ylab('Sentiment Frequency') +
      xlab('Date') + labs(fill="Sentiment", title="Sentiment frequency")
  })
  
  frequency_data_p1 <- reactive({
    
    #--- generates first batch of data for time series of sentiment scores ---
    
    req(tweet_table_data())
    
    tweet_data <- tweet_table_data() %>%
      select(DateTime, Tweet)
    word.df <- as.vector(tweet_data$Tweet)
    emotion.df <- get_nrc_sentiment(word.df)
    emotion.df2 <- cbind(tweet_data, emotion.df) 
    
    sent.value <- get_sentiment(word.df)
    df_new <- data.frame(sent = sent.value, time = tweet_data$DateTime)
    df_new$hour <- format(round(df_new$time, units="hours"), format="%H:%M")
    
    df_new
    
  })
  
  frequency_data_p2 <- reactive({
    
    #--- generates second batch of data for time series of sentiment scores ---
    
    pivot_basic <- frequency_data_p1()
    
    pivot <- pivot_basic %>%
      group_by(hour) %>%
      summarise(mean_sentiment = mean(sent))
    
    pivot
    
  })
  
  output$sentiment_frequency_plot <- renderPlot({
    
    #--- generates time series of sentiment scores ---
    
    df_new2 <- frequency_data_p1()
    pivot2 <- frequency_data_p2()
    
    ggplot(pivot2[-1,], aes(x = hour, y = mean_sentiment)) + 
      geom_line(group = 1, size=1) + geom_point(size=2) + theme_minimal() + 
      labs(title = paste0('Average sentiment of tweetings mentioning "', input$ticker_to_search,'"'),
           subtitle = paste0(pivot2$hour[2],' - ',pivot2$hour[nrow(pivot2)],' on ', 
                             format(df_new2$time[1], '%d %B %Y')),
           x = 'Date', y = 'Sentiment', caption = 'Source: Twitter API')
    
  })
  
}


#--- run the application ---
shinyApp(ui = ui, server = server)
