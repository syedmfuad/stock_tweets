# stock_tweets: What #fintwitter is talking about
## R shiny app that scraps tweets on user defined ticker symbols

This R shiny app (currently undeployed, but fully functional) takes one or more ticker symbols as inputs and reproduces the most recent/popular tweets mentioning the user specified tickers, produces a social network graph detailing interactions (comments, likes and retweets) amongst users, and time series of tweet emotions and sentiments. The purpose of this webapp is to make it easier to see which stocks the twitter community is talking about and take advantage of the tremendous resources shared generously by traders and investors on the platform. 

Usual disclaimer: No content in the tweets should be interpreted as investment advise and one should always do their own due diligence before making an investment. 

## Prerequisites

* Twitter API keys (a tutorial can be found here: https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e).

* An IDE for R (RStudio)

* Shiny and Plotly (R libraries)

(App developed under R version 3.6.1)

## Deployment

Because this webapp is not currently deployed, please execute the following code (replace the X's with your API keys) before proceeding to run the main script.

```{r}
  #--- keys for accessing twitter api ---
  api_key <- "XXXXXXXXXXXXXXXXXXXXXXXX"
  api_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" 
  access_token_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  
  #--- setup access using OAUTH protocol ---
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  ```

## Frontend - shiny app features

### Tweet table

A table that displays the most recent or popular tweets that mention the tickers the user specifies. The table also displays the date the tweet was published, user handle, number of likes and retweets and any shared URLS.

### Network graph

A network graph that displays user interactions (comments, likes and retweets) with the associated tweets.

### Popular tickers

A table that displays the most common ticker symbols in the tweets mentioning the original tickers.

### Sentiment histogram

A histogram that lists a broad category of emotions and sentiments using the NRC Emotion Lexicon developed by Saif Mohammad

### Sentiment time series

A bar plot analyzing sentiment in tertiary responde values (negative, neutral and positive) of tweets using R package SentimentAnalysis

### Sentiment scores time series

A time series chart of tweet sentiment using the NRC Emotion Lexicon. The higher the score, the more positive the sentiment.

## Acknowledgements

* Sean Case
* Catherine Evans
* Carson Farmer
* Michael W. Kearney
* Marion Louveaux
* Sharon Machlis
* Chaitanya Sagar
* Leonardo Toglia
* Leah Wasser

