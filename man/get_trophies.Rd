% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reddit_functions.R
\name{get_trophies}
\alias{get_trophies}
\title{Get trophies for a specified user}
\usage{
get_trophies(
  user,
  accesstoken,
  output = c("df", "json", "all"),
  verbose = FALSE,
  retry = FALSE
)
}
\arguments{
\item{user}{The username of the user}

\item{accesstoken}{The access token required to access the endpoint. Scope of
the acces token must be \code{"read"}.}

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
All trophies a specified user has received as a dataframe, json 
object or a list containing both.
}
\description{
Get trophies for a specified user
}
\details{
More info at \url{https://www.reddit.com/dev/api/} and 
\url{https://www.reddit.com/wiki/trophies}
}
\examples{
\dontrun{
read_token <- get_token(scope = "read",
                        useragent = my_useragent,
                        username = my_username,
                        password = my_password,
                        client_id = my_client_id,
                        client_secret = my_client_secret)
                        
trophies <- get_trophies(user = "_KeanuReeves",accesstoken = read_token)
}
}
\seealso{
\code{\link{get_user_info}}
}
