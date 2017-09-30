## Script for checking everyone you follow on twitter
## max 3200 entries per account befriended
## Can take quite some while if you follow a whole bunch of accounts
## Due to Twitter API rate limiting
###################
## Configuration
how_far_back <- 100 #this can only go to 3200, how far back in each account to go
twitter_account_username <- "usernameBaseForDownload" # account to downloading reading list of
term_to_search_for <- "twitter" # some word to search for

# You have to be registered as a Twitter developer,
# and add our developer details to your own version of this script
api_key <- "yourAPIKeyGoesHere"
api_secret <- "yourAPISecretGoesHere"
access_token  <- "yourAccessTokenGoesHere"
access_token_secret <- "yourAccessTokenSecretGoesHere"


###################
## Libraries
# These need to be already installed (normally with install.package() commands)
library(twitteR)
library(dplyr)
library(lubridate)

###################
## The rest is pretty automatic
# prove who you are to Twitter
options(httr_oauth_cache=F)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
# get accounts you are following
user_details <- getUser(twitter_account_username)
# Twitter rate limites can be slow for large numbers
# get the details of who you are following
accounts_Followed <- user_details$getFriends(retryOnRateLimit=180)
# get their screennames
nome_de_Twits <- sapply(accounts_Followed, function(x){x$toDataFrame()$screenName})

# specific function for getting tweets from specified screenname
fetch_from_twitter <- function(x, maxtw=300){
  timeLine <- tryCatch(userTimeline(x, n=maxtw, includeRts=TRUE, retryOnRateLimit = 180),
                       error = function(c) NULL)
  if(length(timeLine) > 0){
    tabular_tweets <- twListToDF(timeLine)
    return(tabular_tweets)
  }
  return(NULL)
}

tweets_past <- bind_rows(lapply(nome_de_Twits,
                                fetch_from_twitter, 
                                maxtw=how_far_back))

## example of processing tweets by finding ones with the keyword in them

tweets_past$has_term <- 0
tweets_past$has_term[grep(term_to_search_for, tweets_past$text, ignore.case = TRUE)] <- 1
tweets_past %>% mutate(created = ymd_hms(created)) %>%
  arrange(desc(has_term), desc(created)) %>% select(text,screenName,created) %>%
  View()

