
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redditoR: An R wrapper for reddit

<!-- badges: start -->

<!-- badges: end -->

## Introduction

The goal of redditoR is to provide an R wrapper to the API of
[reddit.com](https://reddit.com). You need to have an account with
reddit to use redditR. Additionally, an app needs to be defined in the
settings of your account [here](https://www.reddit.com/prefs/apps/).

Further information about the API can be found
[here](https://www.reddit.com/dev/api/).

## Installation

You can install the current version version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jfeldhege/redditoR")
```

## Example

This is an example workflow to access data through the reddit API:

``` r
#Load the package
library(redditoR)

# Get a token for the scope "read" with the username and password of your account, 
#the client id and secret of your app and a useragent.
read_token <- get_token(scope = "read", useragent = useragent,
                        username = username, password = password,
                        client_id = client_id, client_secret = client_secret)

# Get 10 new posts from the subreddit r/soccer
soccer_posts <- get_posts(subreddit = "soccer",
                   accesstoken = read_token,
                   sort = "new",
                   limit = 10)  
```

For the second example we will get the top 10 comments made by Barack
Obama on his account U/PResidentObama.

``` r
#For the comments on his account we need an access token with the "history" scope.
history_token <- get_token(scope = "history", useragent = useragent,
                        username = username, password = password,
                        client_id = client_id, client_secret = client_secret)

#Get the top comments with the function get_user and type = "comments
top_comments <- get_user(user = "PresidentObama",
                   type = "comments",
                   accesstoken = history_token,
                   sort = "top",
                   time = "all",
                   limit = 10)
```

## Meta

  - Please report any [issues or
    bugs](https://github.com/jfeldhege/redditoR/issues).
  - License: GPL-3
  - Cite with `citation('redditoR')`
