# stock_tweets: What #fintwitter is talking about

Twitter now has a vibrant community of investors and traders. This R shiny app (currently undeployed, but fully functional) takes one of more ticker symbols as inputs and reproduces the most recent/popular tweets mentioning the user specified tickers, produces a social network graph detailing interactions, and a time series of tweet sentiments. 

# Prerequisites

Because this app is not deployed, you would need Twitter API keys (a tutorial can be found here: https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e).

An IDE for R (RStudio)

Shiny and Plotly (R libraries)

App developed under R version 3.6.1

# Deployment

Before running the code, make sure you have your API keys. 

```{r}

  api_key <- "KYX4LqGYUeV32FZVLIF7YnB4c"
  api_secret <- "hbJWEDgitG5kb5Nl63iUaxraqaxmDQqjtfRAG1S0UZTnnZ3pSk"
  access_token <- "963166983255572480-ielV8GxJc1R6aYVc5SE1Ou8GyhDohIK" 
  access_token_secret <- "bqmBYnBl2YMvbiKFHcrU00P04HgC3JsL3bLBNiObILEjP"
  
  #--- setup access using OAUTH protocol ---
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  ```

# Acknowledgements


