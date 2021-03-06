% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reddit_functions.R
\name{get_posts}
\alias{get_posts}
\title{Get posts from a specified subreddit}
\usage{
get_posts(
  subreddit,
  accesstoken,
  sort = "new",
  limit = 100,
  time = c("all", "hour", "day", "week", "month", "year"),
  after = NULL,
  before = NULL,
  output = c("df", "json", "all"),
  verbose = FALSE,
  retry = FALSE
)
}
\arguments{
\item{subreddit}{The name of the subreddit from which posts are requested.}

\item{accesstoken}{The accesstoken required to access the endpoint. Scope
must be \code{"read"}.}

\item{sort}{The order in which the posts in the subreddit should be ordered
when accessing them. Possible values are:
\itemize{
  \item \code{new}: Sorts posts by the time when they were created
  \item \code{hot}: Sorts posts by posts currently on the frontpage of the 
  subreddit
  \item \code{controversial}: Posts with a higher number of up- and downvotes
  simultaneously.
  \item \code{random}:
  \item \code{rising}:
  \item \code{top}: Most upvoted posts in a certain timeframe. Timeframe can
  be specified with \code{time}.
}}

\item{limit}{The maximum number of posts to return. Must be a number between
1 and 100.}

\item{time}{The timeframe for the \code{sort} order \code{controversial} and
\code{top}. Possible values are:
\itemize{
  \item \code{hour}
  \item \code{day}
  \item \code{week}
  \item \code{month}
  \item \code{year}
  \item \code{all} for posts from all time. The default value if 
  time is not specified.
}}

\item{after}{The fullname of a post serving as anchor in the request.
Posts after this post in the listing are returned.}

\item{before}{The fullname of a post serving as anchor in the request.
Posts before this post in the listing are returned.}

\item{output}{What the function should return. Choose \code{json} for an 
unparsed json object, \code{df} for a parsed object in form of a dataframe, 
and \code{all} for a list containg the json object, a dataframe, and the 
before and after anchors (if those exist for the endpoint).}

\item{verbose}{A logical flag whether information about the data extraction
should be printed to the console.}

\item{retry}{A logical flag whether a failed api request should be retried.
Requests will be tried up to three times with varying time intervals between
requests.}
}
\value{
The requested posts as a dataframe, json object or a list 
containing both.
}
\description{
A maximum of 100 posts can be requested from a specified subreddit. The
timeframe and the sorting of the posts can be specified.
}
\details{
More info at \url{https://www.reddit.com/dev/api/}
}
\examples{
\dontrun{
read_token <- get_token(scope = "read",
                        useragent = my_useragent,
                        username = my_username,
                        password = my_password,
                        client_id = my_client_id,
                        client_secret = my_client_secret)
                        
# 10 "hot" posts from the subreddit askreddit
posts <- get_posts(subreddit = "askreddit",
                   accesstoken = read_token,
                   sort = "hot", limit = 10)

# The next set of posts can be retrieved with the argument "after"
posts_after <- get_posts(subreddit = "soccer",
                         accesstoken = read_token,
                         sort = "hot",
                         limit = 10,
                         after = after)
}
}
