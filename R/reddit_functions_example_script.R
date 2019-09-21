

if (!"jsonlite" %in% installed.packages()) install.packages(jsonlite)
if (!"httr" %in% installed.packages()) install.packages(httr)

source("R/reddit_functions.R")


# You need to access the api through an account and specify the user_agent of
# the device you're using. This is an account I made for my reddit research
username = "FOSTtest2"
password = "test12345678"
user_agent <- "Windows 10"



#There are a number of scopes that give access to different endpoints.
#I have implemented four scopes so far:
# - read
# - history
# - identity
# - wikiread
# More info at https://www.reddit.com/dev/api/


# Read token

read_token <- get_token(scope = "read", useragent = user_agent,
                        username = username, password = password)


posts <- get_posts(sub = "soccer",
                   accesstoken = read_token,
                   link_flair = FALSE,
                   author_flair = FALSE,
                   user_reports = FALSE,
                   secure_media = FALSE,
                   awards = FALSE,
                   sort = "hot",
                   limit = 10,
                   time = NULL,
                   verbose = FALSE)


comms <- get_comments(sub = "soccer",
                      accesstoken = read_token,
                      limit = 10,
                      verbose = TRUE)


uinfo <- get_user_info(user = "PresidentObama",
                       accesstoken = read_token,
                       verbose = FALSE)

trophies <- get_trophies(user = "PresidentObama",
                         accesstoken = read_token,
                         verbose = FALSE) # Maybe this should be put
#together with the get_user_info function to create
# one general function to get information about a user


sub_info <- get_subreddit_info(subreddit = "soccer",
                               accesstoken = read_token,
                               verbose = FALSE) #This function returns a list
# right now because some elements of the list are length 1 and some length 2
# this was why I sent you that email



# History Token

his_token <- get_token(scope = "history", useragent = user_agent,
                       username = username, password = password)



overview <- get_user(user = "PresidentObama",
                   result = "overview",
                   accesstoken = his_token,
                   sort = "hot",
                   time = "all",
                   limit = 10,
                   before=NULL,
                   after=NULL,
                   verbose = FALSE)

ucomms <- get_user(user = "PresidentObama",
                     result = "comments",
                     accesstoken = his_token,
                     sort = "hot",
                     time = "all",
                     limit = 10,
                     before=NULL,
                     after=NULL,
                     verbose = FALSE)


submitted <- get_user(user = "PresidentObama",
                      result = "submitted",
                      accesstoken = his_token,
                      sort = "hot",
                      time = "all",
                      limit = 10,
                      before=NULL,
                      after=NULL,
                      verbose = FALSE)


gilded <- get_user(user = "PresidentObama",
                      result = "gilded",
                      accesstoken = his_token,
                      sort = "hot",
                      time = "all",
                      limit = 10,
                      before=NULL,
                      after=NULL,
                      verbose = FALSE)


# Wiki

wiki_token <- get_token(scope = "wikiread", useragent = user_agent,
                        username = username, password = password )


soccer_wiki <- get_wiki(subreddit = "soccer",
                        accesstoken = wiki_token,
                        verbose = FALSE)




