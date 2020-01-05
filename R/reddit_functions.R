#' Generate an access token for the Reddit API
#'
#' @param scope The scope of what you want to access. Must be one of these:
#' identity, read, history, wikiread
#' @param useragent The useragent for the device accessing the API
#' @param username Username for the Reddit profile accessing the API. If left
#' empty, it will attempt to read from the environment. Can be set with
#' \code{Sys.setenv(REDDIT_API_USERNAME = "")}
#' @param password Password for the Reddit profile accessing the API. If left
#' empty, it will attempt to read from the environment. Can be set with
#' \code{Sys.setenv(REDDIT_API_PASSWORD = "")}
#' @return A list containing the access token, the time until it expires, the
#' scope, the time it was generated and the useragent
#' @details Access tokens are only valid for an hour.
#' More info at \url{https://www.reddit.com/dev/api/}

get_token <- function (scope = c("identity", "read", "history", "wikiread"),
                       useragent,
                       username = NULL,
                       password = NULL) {

  assertthat::assert_that(assertthat::is.string(scope),
                          assertthat::not_empty(useragent),
                          nchar(useragent) > 0,
                          scope %in% c("identity", "read", "history", "wikiread"),
                          msg = "Invalid scope")

  assertthat::assert_that(assertthat::is.string(useragent),
                          assertthat::not_empty(useragent),
                          nchar(useragent) > 0,
                          msg = "Please supply a useragent")

  if(is.null(username)) {
    username <- Sys.getenv("REDDIT_API_USERNAME")

  }
  if(is.null(password)){
    password <- Sys.getenv("REDDIT_API_PASSWORD")
  }
  assertthat::assert_that(assertthat::is.string(username),
                          assertthat::not_empty(username),
                          nchar(username) > 0,
                          assertthat::is.string(password),
                          assertthat::not_empty(password),
                          nchar(password) > 0,
                          msg = "No username or password entered")


  token <- httr::POST(url = "https://www.reddit.com/api/v1/access_token",
                      body = list(
                        grant_type = "password",
                        username = username,
                        password = password,
                        scope = scope),
                      encode = "form",
                      httr::authenticate("cTExHaVDTw05uQ", "hwoARWG2b5Q-rBD03kFEeRu6aRw"),
                      httr::user_agent(useragent)
  )

  httr::stop_for_status(token)

  acc_token <- httr::content(token)

  access_time <- Sys.time()

  acc_token <- c(acc_token,
                 access_time = access_time,
                 useragent = useragent)

  return (acc_token)
}

#' Get posts from a specified subreddit
#'
#' @param subreddit The name of the subreddit from which posts are requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param sort The order in which the posts in the subreddit should be ordered
#' when accessing them. Possible values are:
#' \itemize{
#'   \item \code{new}: Sorts posts by the time when they were created
#'   \item \code{hot}: Sorts posts by posts currently
#'   \item \code{controversial}:
#'   \item \code{random}:
#'   \item \code{rising}:
#'   \item \code{top}: Most upvoted posts in a certain timeframe. Timeframe can
#'   be specified with \code{time}.
#' }
#' @param limit The maximum number of posts to return. Must be a number between
#' 1 and 100.
#' @param time The timeframe for the \code{sort} order \code{controversial} and
#' \code{top}. Possible values are:
#' \itemize{
#'   \item \code{hour}
#'   \item \code{day}
#'   \item \code{week}
#'   \item \code{month}
#'   \item \code{year}
#'   \item \code{all time}
#' }
#' @param before The fullname of a post serving as anchor in the request.
#' Posts before this post in the listing are returned.
#' @param after The fullname of a post serving as anchor in the request.
#' Posts after this post in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe containg the requested posts
#' @details More info at \url{https://www.reddit.com/dev/api/}

get_posts <- function (subreddit,
                       accesstoken,
                       sort ="new",
                       limit=100,
                       time = NULL,
                       after = NULL,
                       before = NULL,
                       verbose = TRUE,
                       retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "subreddit")

  if(!is.null(time)){
    if(!time %in% c("hour", "day", "week", "month", "year", "all"))
      stop("Time has to be one of these: hour, day, week, month, year, all")}

  link <- build_link(path_elements = paste0("r/", subreddit, "/", sort),
                     query_elements = paste0("limit=", limit, "&t=", time),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting posts from:",link))

  resp <- make_request(accesstoken, link, verbose, retry)

  posts <- parse_response(resp,after_before = TRUE, verbose)

  return(posts)
}


#' Get submissions for a specified user
#'
#' @param user The username of the user for which submissions are requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"history"}.
#' @param sort The order in which the submissions of the user should be ordered
#' when accessing them. Possible values are:
#' \itemize{
#'   \item \code{new}: Sorts posts by the time when they were created
#'   \item \code{hot}: Sorts posts by posts currently trending
#'   \item \code{controversial}: Sorts posts by number of upvotes and downvotes
#'   \item \code{top}: Most upvoted posts in a certain timeframe. Timeframe can
#'   be specified with \code{time}.
#' }
#' @param time The timeframe for the \code{sort} order \code{controversial} and
#' \code{top}. Possible values are:
#' \itemize{
#'   \item \code{hour}
#'   \item \code{day}
#'   \item \code{week}
#'   \item \code{month}
#'   \item \code{year}
#'   \item \code{all time}
#' }
#' @param limit The maximum number of submissions to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of a post serving as anchor in the request.
#' Posts before this post in the listing are returned.
#' @param after The fullname of a post serving as anchor in the request.
#' Posts after this post in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#'
#' @return A dataframe of posts for a specified user.
#' @export
#' @details More info at \url{https://www.reddit.com/dev/api/}


get_submissions <- function (user,
                             accesstoken,
                             sort = "new",
                             time = NULL,
                             limit = 100,
                             before = NULL,
                             after = NULL,
                             verbose = FALSE,
                             retry = TRUE) {

  check_token(accesstoken, scope = "history")

  check_args(default_arg = "user")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")

  link <- build_link(path_elements = paste0("user/", user, "/submitted"),
                     query_elements = paste0("limit=", limit,"&sort=", sort,
                                             "&t=", time),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting submissions from:", link))

  request <- make_request(accesstoken, link, verbose, retry)

  resp <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  submissions <- parse_response(resp,after_before = TRUE, verbose)

  return(submissions)
}



#' Get comments from a specified subreddit
#'
#' @param subreddit The name of the subreddit from which comments are requested
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param limit The maximum number of comments to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of a comment serving as anchor in the request.
#' Comments before this comment in the listing are returned.
#' @param after The fullname of a comment serving as anchor in the request.
#' Comments after this comment in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe with new comments from a specified subreddit.
#' @export

get_comments <- function (subreddit,
                          accesstoken,
                          limit=100,
                          before=NULL,
                          after=NULL,
                          verbose = FALSE,
                          retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "subreddit")

  link <- build_link(path_elements = paste0("r/", subreddit,"/comments"),
                     query_elements = paste0("limit=", limit),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting comments from:",link))

  resp <- make_request(accesstoken, link, verbose, retry)

  comments <- parse_response(resp,after_before = TRUE, verbose)

  return(comments)
}


#' Get comments from a specified user
#'
#' @param user The username of the user for which comments are requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"history"}.
#' @param sort The order in which the submissions of the user should be ordered
#' when accessing them. Possible values are:
#' \itemize{
#'   \item \code{new}: Returns newest comments
#'   \item \code{hot}: Returns highly upvoted comments currently
#'   \item \code{controversial}: Returns comments that received a lot of upvotes
#'   and downvotes
#'   \item \code{top}: Returns most upvoted comments in a certain timeframe.
#'   Timeframe can be specified with \code{time}.
#' }
#' @param time The timeframe for the \code{sort} order \code{controversial} and
#' \code{top}. Possible values are:
#' \itemize{
#'   \item \code{hour}
#'   \item \code{day}
#'   \item \code{week}
#'   \item \code{month}
#'   \item \code{year}
#'   \item \code{all time}
#' }
#' @param limit The maximum number of user comments to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of a comment serving as anchor in the request.
#' Comments before this comment in the listing are returned.
#' @param after The fullname of a comment serving as anchor in the request.
#' Comments after this comment in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe of comments for the specified user.
#' @export


get_user_comments <- function (user,
                               accesstoken,
                               sort = "new",
                               time = NULL,
                               limit = 100,
                               after=NULL,
                               before=NULL,
                               verbose = FALSE,
                               retry = TRUE) {

  check_token(accesstoken, scope = "history")

  check_args(default_arg = "user")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")

  link <- build_link(path_elements = paste0("user/", user,"/comments"),
                     query_elements = paste0("limit=",limit,"&sort=", sort,
                                             "&t=", time),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting comments from:", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  user_comments <- parse_response(resp,after_before = TRUE, verbose)

  return(user_comments)
}


#' Get posts and comments from a specified user
#'
#' @param user The username of the user for which posts and comments are
#' requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"history"}.
#' @param type The type of content that is requested for the user. Possible
#' valuesare:
#' \itemize{
#'   \item \code{overview} Contains posts and comments.
#'   \item \code{comments} Only comments.
#'   \item \code{submitted} Only posts.
#'   \item \code{gilded} Posts and comments that have received awards.
#'   }
#' @param sort The order in which the posts or comments of the user should be
#' ordered when accessing them. Possible values are:
#' \itemize{
#'   \item \code{new}: Returns newest comments or posts
#'   \item \code{hot}: Returns highly upvoted comments or posts currently
#'   \item \code{controversial}: Returns comments or posts that received a lot
#'   of upvotes and downvotes in a certain imeframe.Timeframe can be specified
#'   with \code{time}.
#'   \item \code{top}: Returns most upvoted comments posts in a certain
#'   timeframe.Timeframe can be specified with \code{time}.
#' }
#' @param time The timeframe for the \code{sort} order \code{"controversial"}
#' and \code{"top"}. Possible values are:
#' \itemize{
#'   \item \code{hour}
#'   \item \code{day}
#'   \item \code{week}
#'   \item \code{month}
#'   \item \code{year}
#'   \item \code{all} for items from day the user registered their account to
#'   today.
#' }
#' @param limit The maximum number of items to return. Must be a number between
#' 1 and 100.
#' @param before The fullname of an item serving as anchor in the
#' request. Items before this item in the listing are returned.
#' @param after The fullname of an item serving as anchor in the request.
#' Items after this item in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe of posts or comments for the specified user.
#' @export


get_user <- function (user,
                      accesstoken,
                      type = c("overview", "comments", "submitted", "gilded"),
                      sort = "new",
                      time = NULL,
                      limit = 100,
                      before=NULL,
                      after=NULL,
                      verbose = FALSE,
                      retry = TRUE) {

  check_token(accesstoken, scope = "history")

  check_args(default_arg = "user")

  if(!type %in% c("overview", "comments", "submitted", "gilded"))
    stop("type has to be one of these: overview, comments, submitted, gilded")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")

  link <- build_link(path_elements = paste0("user/", user,"/", type),
                     query_elements = paste0("limit=", limit, "&sort=", sort,
                                             "&t=", time),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting user data from:", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  user <- parse_response(resp,after_before = TRUE, verbose)

  return(user)
}


#' Get basic information about account for a specified user
#'
#' @param user Username of the Reddit user that is requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe with information about a specified user.
#' @export


get_user_info <- function (user,
                           accesstoken,
                           verbose = FALSE,
                           retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "user")

  link <- build_link(path_elements = paste0("user/", user, "/about"))

  if(verbose == TRUE) print(paste("Getting user info from: ", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  user_info <- parse_response(resp,
                              after_before = FALSE,
                              verbose = verbose)

  return(user_info)
}



#' Get basic information for a specified subreddit.
#'
#' @param subreddit Name of the subreddit for which info is requested.
#' @param type Which info is requested. Possible values are \code{info} for
#' information about the subreddit from the sidebar, \code{moderators} for
#' the moderators of the subreddit and \code{rules} for the rules of the
#' subreddit.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#'
#' @return A dataframe with basic information about a subreddit from the
#' sidebar or a dataframe with the moderators or the rules of the subreddif.
#' @export
#'
#' @examples
#' \dontrun{
#' read_token <- get_token(scope = "read",
#'                                       useragent = useragent,
#'                                       username = username,
#'                                       password = password)
#'
#' sub_info <- get_subreddit_info(subreddit = "soccer",
#'                                accesstoken = read_token,
#'                                verbose = FALSE)
#'                                }

get_subreddit_info <- function (subreddit,
                                type = c("info", "moderators", "rules"),
                                accesstoken,
                                verbose = FALSE,
                                retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "subreddit")

  if(type == "info") {

    link <- build_link(path_elements = paste0("r/", subreddit, "/about"))

  } else if(type == "moderators"|type == "rules"){

    link <- build_link(path_elements = paste0("r/", subreddit, "/about/", type))

  } else stop("Result has to be 'info', 'moderators' or 'rules'.")

  if(verbose == TRUE) print(paste("Getting subreddit info from:", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  sub_info <- parse_response(resp,
                             after_before = FALSE,
                             verbose = verbose)
  return(sub_info)
}


#' Get wiki pages for a specified subreddit.
#'
#' @param subreddit Name of the subreddit for which wiki pages are requested.
#' @param page Name of the requested page. If \code{"page"} is \code{"all"},
#' all wiki pages for the subreddit are returned.
#' @param accesstoken The access token required to access the endpoint. Scope of
#' the access token must be \code{"wikiread"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#'
#' @return A list containg the text of the page and all revisions.
#' If  \code{"page"} is \code{"all"}, a character vector containg the names of
#' all wiki pages.
#' @export
#'
#' @seealso \code{\link{get_subreddit_info}}

get_wiki <- function (subreddit,
                      page = "all",
                      accesstoken,
                      verbose = FALSE,
                      retry = TRUE) {

  check_token(accesstoken, scope = "wikiread")

  check_args(default_arg = "subreddit")

  if(page == "all"){
    link <- build_link(path_elements = paste0("r/", subreddit, "/wiki/pages"),
                       query_elements = NULL, before = NULL, after = NULL)
  } else{
    link <- build_link(path_elements = paste0("r/", subreddit, "/wiki/", page),
                       query_elements = NULL, before = NULL, after = NULL)
  }

  if(verbose == TRUE) print(paste("Getting wiki from", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  wiki <- parse_response(resp,
                         after_before = FALSE,
                         verbose = verbose)

  return(wiki)
}



#' Get trophies for a specified user
#'
#' @param user The username of the user
#' @param accesstoken The access token required to access the endpoint. Scope of
#' the acces token must be \code{"read"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#'
#' @return A dataframe with all trophies a specified user has received.
#' @export
#' @seealso \code{\link{get_user_info}}

get_trophies <- function (user,
                          accesstoken,
                          verbose = FALSE,
                          retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "user")

  link <- build_link(path_elements = paste0("api/v1/user/", user,
                                            "/trophies"), query_elements = NULL, before = NULL, after = NULL)

  if(verbose == TRUE) print(paste("Getting trophies from:", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  trophies <- parse_response(resp,
                             after_before = FALSE,
                             verbose = verbose)

  return(trophies)
}



#' Get a dataframe of subreddits sorted by creation date or popularity. Also
#' allows searching for subreddits
#'
#' @param type The type of list that is requested. Possible values are:
#' \itemize{
#'   \item \code{popular} Subreddits that are popular right now
#'   \item \code{new} Newly created subreddits
#'   \item \code{default} Default subreddits that users are subscribed to when
#'   they sign up for reddit.
#'    \item \code{search} Search for subreddits with a query.
#'   }
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param query Search terms for type \code{search}. Must be less than 512
#' characters.
#' @param sort The order in which results from type \code{search} should be
#' sorted. Possible values are \code{"relevance"} and \code{"activity"}.
#' @param limit The maximum number of subreddits to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of an item serving as anchor in the
#' request. Items before this item in the listing are returned.
#' @param after The fullname of an item serving as anchor in the request.
#' Items after this item in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe of subreddits.
#' @export
#'

get_subreddits <- function (type = c("popular", "new", "default", "search"),
                            accesstoken,
                            query = NULL,
                            sort = NULL,
                            limit = 100,
                            after = NULL,
                            before = NULL,
                            verbose = FALSE,
                            retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = NULL)

  if(!type %in% c("popular", "new", "default", "search"))
    stop("type has to be one of these: popular, new, default", "search")

  if(type == "search") assertthat::assert_that(!is.null(query),
                                               msg = "No query specified for type search")

  link <- build_link(path_elements = paste0("subreddits/", type),
                     query_elements = paste0("limit=",limit,  "&q=", query,
                                             "&sort", sort),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting subreddit info from:", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  subreddits <- parse_response(resp,
                               after_before = TRUE,
                               verbose = verbose)

  return(subreddits)
}




#' Get a list of user accounts
#'
#' @param type The type of list that is requested. Possible values are:
#' \itemize{
#'   \item \code{popular} User accounts that are popular right now
#'   \item \code{new} Newly created user accounts
#'   }
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param limit The maximum number of users to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of an item serving as anchor in the
#' request. Items before this item in the listing are returned.
#' @param after The fullname of an item serving as anchor in the request.
#' Items after this item in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#' @return A dataframe of users.
#' @export
#'

get_users <- function(type = c("popular", "new"),
                      accesstoken,
                      limit = 100,
                      after = NULL,
                      before = NULL,
                      verbose = FALSE,
                      retry = TRUE) {

  check_token(accesstoken, scope = "read")

  check_args()

  if(!type %in% c("popular", "new"))
    stop("type has to be one of these: popular, new")

  link <- build_link(path_elements = paste0("users/", type),
                     query_elements = paste0("limit=",limit),
                     before = before, after = after)

  if(verbose == TRUE) print(paste("Getting users from: ", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  users <- parse_response(resp,
                          after_before = TRUE,
                          verbose = verbose)

  return(users)
}



#' Search for posts in a subreddit or all of reddit
#'
#' @param query The search query. Maximum length of the query is 512 characters.
#' @param subreddit The name of the subreddit in which the search should be
#' conducted. If left empty, the search will be conducted in all subreddits.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param sort The order in which the search results should be ordered
#' when accessing them. Possible values are:
#' \itemize{
#'   \item \code{new}: Sorts search results by the time when they were created
#'   \item \code{hot}: Sorts search results currently trending
#'   \item \code{relevance}:
#'   \item \code{comments}:
#'   \item \code{top}: Most upvoted search results in a certain timeframe.
#'   Timeframe can be specified with \code{time}.
#' }
#' @param limit The maximum number of search results to return. Must be a number
#' between 1 and 100.
#' @param time The timeframe in which the search results were created. Possible
#' values are:
#' \itemize{
#'   \item \code{hour}
#'   \item \code{day}
#'   \item \code{week}
#'   \item \code{month}
#'   \item \code{year}
#'   \item \code{all time}
#' }
#' @param before The fullname of a search result serving as anchor in the
#' request. Search results before this post in the listing are returned.
#' @param after The fullname of a search result serving as anchor in the
#' request. Search results after this post in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#' @param retry A logical flag whether a failed api request should be retried.
#' Requests will be tried up to three times with varying time intervals between
#' requests.
#'
#' @return A dataframe with posts matching the search query.
#' @export
#'
#' @details Information on how to build advanced search queries can be found at
#' this link: \url{https://www.reddit.com/wiki/search}

search_reddit <- function(query,
                          accesstoken,
                          subreddit=NULL,
                          sort ="new",
                          limit=100,
                          time = NULL,
                          verbose = FALSE,
                          retry = FALSE) {

  check_token(accesstoken, scope = "read")

  check_args(default_arg = "query")

  if(!is.null(subreddit)){search_path <- paste0("r/", subreddit, "/search")}
  else{search_path <- paste0("search")}

  link <- build_link(path_elements = search_path,
                     query_elements = paste0("q=",query, "&sort=", sort,
                                             "&limit=", limit, "&t=", time,
                                             "&restrict_sr=on"),
                     before = NULL, after = NULL)

  if(verbose == TRUE) print(paste("Getting search results from: ", link))

  resp <- make_request(accesstoken, link, verbose, retry)

  search_results <- parse_response(resp,
                                   after_before = TRUE,
                                   verbose = verbose)

  return(search_results)
}


check_token <- function(x, scope){

  if(is.null(x)){
    stop("No token was specified")
  }else {
    if(is.null(x$useragent)) stop("No user agent was specified")

    if(Sys.time() - x$access_time > 3600) stop("Token is expired")

    if(x$scope != scope) stop(paste("This function requires", scope, "as scope of the token"))
  }
}


check_args <- function(default_arg = NULL){

  get_args <- function () {
    as.list(sys.frame(-2))
  }

  check_strings <- function(x, y){

    string_args <- c("user", "subreddit", "time", "page", "sort", "after",
                     "before")

    if(y %in% string_args & !is.null(x)) {
      assertthat::assert_that(assertthat::is.string(x),
                              assertthat::not_empty(x),
                              msg = paste(y, "is not a character vector"))
    }
  }

  args <- get_args()

  if(!is.null(default_arg)){
    if(default_arg == "user"){
      assertthat::assert_that("user" %in% names(args),
                              assertthat::not_empty(args$user),
                              assertthat::is.string(args$user),
                              nchar(args$user)> 0,
                              msg = "A non-empty character vector has to be supplied for the parameter 'user'")
    } else if(default_arg == "subreddit"){
      assertthat::assert_that("subreddit" %in% names(args),
                              assertthat::not_empty(args$subreddit),
                              assertthat::is.string(args$subreddit),
                              nchar(args$subreddit)> 0,
                              msg = "A non-empty character vector has to be supplied for the parameter 'subreddit'")
    } else if(default_arg == "query"){
      assertthat::assert_that("query" %in% names(args),
                              assertthat::not_empty(args$query),
                              assertthat::is.string(args$query),
                              nchar(args$query)> 0,
                              msg = "A non-empty string has to be supplied for the parameter 'query'")
    }
  }

  mapply(check_strings, args, names(args))

  if("query" %in% names(args) & !is.null(args$query)){
    assertthat::assert_that(assertthat::is.string(args$query),
                            assertthat::not_empty(args$query),
                            nchar(args$query) > 0,
                            nchar(args$query) < 513)}

  if("limit" %in% names(args) & !is.null(args$limit)){
    assertthat::assert_that(assertthat::is.count(args$limit),
                            args$limit < 101,
                            args$limit > 0,
                            msg = "'limit' has to be a number between 0 and 100")}

  if("verbose" %in% names(args) & !is.null(args$verbose)){
    assertthat::assert_that(assertthat::is.flag(args$verbose),
                            msg = "'verbose' has to be a boolean")}
}

build_link <-  function(path_elements,
                        query_elements = NULL,
                        before = NULL,
                        after = NULL){

  url <- httr::parse_url("https://oauth.reddit.com/")

  url$path <- paste0(path_elements, ".json")

  if(is.null(before) & !is.null(after)){

    url$query <- paste0(query_elements, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {

    url$query <- paste0(query_elements, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    url$query <- query_elements
  }

  link <- httr::build_url(url)

  return(link)
}

make_request <- function(accesstoken,
                         link,
                         verbose,
                         retry){
  auth <- paste("bearer", accesstoken$access_token)

  if(retry){
    request <- httr::RETRY("GET", url = link,
                           httr::add_headers(Authorization = auth),
                           httr::user_agent(accesstoken$useragent),
                           times = 3)
  } else {
    request <- httr::GET(link,
                         httr::add_headers(Authorization = auth),
                         httr::user_agent(accesstoken$useragent))
  }


  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  return(request)
}

parse_response <- function(response,
                           after_before = FALSE,
                           verbose) {

  response_json <- jsonlite::fromJSON(httr::content(response, as="text"), flatten = TRUE)

  if("data" %in% names(response_json)){
    data <- response_json$data

    if("children" %in% names(data)) {
      data <- data$children
    }
  }

  if(length(data)>0){

    if(!is.data.frame(data)){

      replace_null <- function(x) {
        lapply(x, function(x) {
          if (is.list(x)){
            replace_null(x)
          } else{
            if(is.null(x)) NA else(x)
          }
        })
      }

      data <- replace_null(data)

      replace_empty_list <- function(x){
        if (is.list(x) & length(x) == 0) NA else (x)
      }


      data <- replace_empty_list(data)

      data <- data.frame(t(unlist(data)), stringsAsFactors = FALSE)
    }

    names(data) <- sub("data.", "", names(data))

    if(verbose == TRUE) print(paste(nrow(data),"item(s) retrieved from reddit."))

    if(after_before == TRUE){
      after <<- response_json$data$after

      before <<- data[order(data$created, decreasing = T),"name"][1]
    }

    return(data)

  } else{
    if(verbose == TRUE) print("No items retrieved from reddit.")

    df_after <<- NA
    df_before <<- NA
  }
}
