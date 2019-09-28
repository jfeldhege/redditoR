#' Generate an access token for the Reddit API
#'
#' @param scope The scope of what you want to access. Must be one of these:
#' identity, read, history, wikiread
#' @param useragent The useragent for the device accessing the API
#' @param username Username for the Reddit profile accessing the API
#' @param password Password for the Reddit profile accessing the API
#' @return A list containing the access token, the time until it expires, the
#' scope, the time it was generated and the useragent
#' @details Access tokens are only valid for an hour.
#' More info at \url{https://www.reddit.com/dev/api/}

get_token <- function (scope = c("identity", "read", "history", "wikiread"),
                       useragent,
                       username,
                       password) {

  if(!scope %in% c("identity", "read", "history", "wikiread")) stop("Invalid scope")


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
#' @return A dataframe containg the requested posts
#' @details More info at \url{https://www.reddit.com/dev/api/}

get_posts <- function (subreddit,
                       accesstoken,
                       sort ="new",
                       limit=100,
                       time = NULL,
                       link_flair = FALSE,
                       author_flair = FALSE,
                       user_reports = FALSE,
                       secure_media = FALSE,
                       awards = FALSE,
                       after = NULL,
                       before = NULL,
                       verbose = TRUE) {

  #catching errors

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "read")stop("This function requires 'read' as scope of the token")
  }

  if(!is.numeric(limit)) stop("limit has to be a number")

  if(limit > 100 | limit < 1) stop("limit has to be a number between 1 and 100")

  if(!is.null(time)){
    if(!time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")}

  #make link
  if(is.null(before) & !is.null(after)){


    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit, "&after=", after, "&t=", time)

  } else if (!is.null(before) & is.null(after)) {


    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit, "&before=", before, "&t=", time)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit, "&t=", time)
  }

  if(verbose == TRUE) print(paste("Getting data from",link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  posts <- as.data.frame(result$data$children)


  if(nrow(posts)>0){
    names(posts) <- sub("data.", "", names(posts))

    if(link_flair == FALSE) {
      posts <- posts[, !grepl("link_flair", names(posts))]
    } else{
      #posts$link_flair_richtext <- do.call("rbind", posts$link_flair_richtext)
    }

    if(author_flair == FALSE) {
      posts <- posts[, !grepl("author_flair", names(posts))]
    }

    if(user_reports == FALSE){
      posts <- posts[, !grepl("user_reports", names(posts))]
    }

    if(secure_media == FALSE){
      posts <- posts[, !grepl("secure_media", names(posts))]
    }


    if(awards == FALSE){
      posts <- posts[, !grepl("all_awardings", names(posts))]
    }

    if(!is.null(result$data$after)){
      name_after <<- result$data$after
    } else {
      name_after <<- NA
    }

    name_before <<- posts[order(posts$created, decreasing = T),"name"][1]

    if(verbose == TRUE) print(paste(nrow(posts),"posts retrieved from reddit."))
    return(posts)

  } else {
    if(verbose == TRUE) print("No posts retrieved from reddit.")
  }
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
                             verbose = FALSE) {

  #Catching errors
  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope!="history") stop("This function requires 'history' as scope of the token")
  }

  if(is.null(user)) stop("No user was specified")

  if(!is.numeric(limit)) stop("limit has to be a number")

  if(limit > 100 | limit < 1) stop("limit has to be a number between 1 and 100")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/submitted/", ".json?limit=",
                   limit,"&sort=", sort, "&after=", after, "&t=", time)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/submitted/", ".json?limit=",
                   limit,"&sort=", sort, "&before=", before, "&t=", time)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/submitted/", ".json?limit=",
                   limit,"&sort=", sort,  "&t=", time)
  }

  if(verbose == TRUE) print(paste("Getting data from", link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent)
  )

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  submissions <- as.data.frame(result$data$children)

  if (nrow(submissions)>0) {

    names(submissions) <- sub("data.", "", names(submissions))

    if(!is.null(result$data$after)){
      name_after <<- result$data$after
    } else {
      name_after <<- NULL
    }

    name_before <<- submissions[order(submissions$created, decreasing = T),"name"][1]

    if(verbose == TRUE) print(paste(nrow(submissions),"submissions retrieved from reddit."))

    return(submissions)
  } else {print("No submissions available for this user")}
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
#'
#' @return A dataframe with new comments from a specified subreddit.
#' @export

get_comments <- function (subreddit,
                          accesstoken,
                          limit=100,
                          before=NULL,
                          after=NULL,
                          verbose = FALSE) {

  #Catching errors
  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope!="read") stop("This function requires 'read' as scope of the token")
  }

  #make link
  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/r/", subreddit,
                   "/comments/.json?limit=", limit, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/r/", subreddit,
                   "/comments/.json?limit=", limit, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/r/",
                   subreddit, "/comments/.json?limit=", limit)
  }

  if(verbose == TRUE) print(paste("Getting data from",link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)

  comments <- result$data$children

  if(nrow(comments)>0){

    names(comments) <- sub("data.", "", names(comments))

    if(verbose == TRUE) print(paste(nrow(comments),"comments retrieved from reddit."))

    return(as.data.frame(comments))

    name_after <<- result$data$after

    name_before <<- comments[order(comments$created, decreasing = T),"name"][1]

  } else{
    print("No comments retrieved from reddit.")

    name_after <<- NA
    name_before <<- NA
  }
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
#'
#' @return A dataframe of comments for the specified user.
#' @export


get_user_comments <- function (user,
                               accesstoken=NULL,
                               sort = "new",
                               time = NULL,
                               limit = 100,
                               after=NULL,
                               before=NULL,
                               verbose = FALSE) {

  require(httr, quietly = TRUE)
  require(jsonlite, quietly = TRUE)

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope!="history") stop("This function requires 'history' as scope of the token")
  }

  if(is.null(user)) stop("No user was specified")

  if(!is.numeric(limit)) stop("limit has to be a number")

  if(limit > 100 | limit < 1) stop("limit has to be a number between 1 and 100")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,"/comments/.json?limit=",
                   limit,"&sort=", sort, "&after=", after, "&t=", time)

  } else if (!is.null(before) & is.null(after)) {


    link <- paste0("https://oauth.reddit.com/user/", user, "/comments/.json?limit=",
                   limit,"&sort=", sort, "&before=", before, "&t=", time)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,"/comments/.json?limit=",
                   limit,"&sort=", sort, "&t=", time)
  }

  if(verbose == TRUE) print(paste("Get data from", link, collapse = ","))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                       httr::add_headers(Authorization = auth),
                       httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  if (length(result$data$children) > 0) {

    user_comments <- result$data$children

    names(user_comments) <- sub("data.", "", names(user_comments))

    if(!is.null(result$data$after)){
      name_after <<- result$data$after
    } else {
      name_after <<- NA
    }

    name_before <<-user_comments[order(user_comments$created, decreasing = T),"name"][1]

    return(user_comments)
  } else{
    print("No comments for the user retrieved from reddit.")

    name_after <<- NA
    name_before <<- NA
  }
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
#'
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
                      verbose = FALSE) {

  #Catching errors
  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope!="history") stop("This function requires 'history' as scope of the token")
  }

  if(!is.numeric(limit)) stop("limit has to be a number")

  if(limit > 100 | limit < 1) stop("limit has to be a number between 1 and 100")

  if(is.null(type)) stop("No type specified")

  if(!type %in% c("overview", "comments", "submitted", "gilded"))
    stop("type has to be one of these: overview, comments, submitted, gilded")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", type,  "/.json?limit=", limit,
                   "&sort=", sort, "&after=", after, "&t=", time)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", type,  "/.json?limit=", limit,
                   "&sort=", sort, "&before=", before, "&t=", time)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", type,  "/.json?limit=", limit,"&sort=", sort,"&t=", time)
  }

  if(verbose == TRUE) print(paste("Getting data from", link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  response <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  result <- as.data.frame(response$data$children)

  if (nrow(result)>0) {

    names(result) <- sub("data.", "", names(result))

    if(!is.null(result$data$after)){
      name_after <<- result$data$after
    } else {
      name_after <<- NULL
    }

    name_before <<- result[order(result$created, decreasing = T),"name"][1]

    if(verbose == TRUE) print(paste(nrow(result),"items retrieved from reddit."))

    return(result)
  } else {print("No result available for this user")}
}


#' Get basic information about account for a specified user
#'
#' @param user Username of the Reddit user that is requested.
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#'
#' @return A dataframe with information about a specified user.
#' @export


get_user_info <- function (user = NULL,
                           accesstoken=NULL,
                           verbose = FALSE) {

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "read") stop("This function requires 'read' as scope of the token")
  }

  if(is.null(user)) stop("No user was specified")

  auth <- paste("bearer", accesstoken$access_token)

  link <- paste0("https://oauth.reddit.com/user/", user, "/about")

  if(verbose == TRUE) print(paste("Getting user info from: ", link))

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as="text"), flatten = TRUE)

  user_info <- as.data.frame(result$data[-15])

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

get_subreddit_info <- function (subreddit = NULL,
                                type = c("info", "moderators", "rules"),
                                accesstoken = NULL,
                                verbose = FALSE) {

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "read") stop("This function requires 'read' as scope of the token")
  }

  if(is.null(subreddit)) stop("No subreddit was specified")

  if(type == "info") {

    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/about.json")

  } else if(type == "moderators"|type == "rules"){

    link <- paste0("https://oauth.reddit.com/r/", subreddit,
                   "/about/", type, ".json")

  } else stop("Result has to be 'info', 'moderators' or 'rules'.")

  if(verbose == TRUE) print(paste("Getting subreddit info from: ", link))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)

  if(type == "info") sub_info <- result$data
  else if (type == "moderators") sub_info <- result$data$children
  else if (type == "rules") sub_info <- result$rules

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
#'
#' @return A list containg the text of the page and all revisions.
#' If  \code{"page"} is \code{"all"}, a character vector containg the names of
#' all wiki pages.
#' @export
#'
#' @seealso \code{\link{get_subreddit_info}}

get_wiki <- function (subreddit = NULL,
                      page = "all",
                      accesstoken = NULL,
                      verbose = FALSE) {

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "wikiread") stop("This function requires 'wikiread' as scope of the token")
  }

  if(is.null(subreddit)) stop("No subreddit was specified")

  auth <- paste("bearer", accesstoken$access_token)

  if(page == "all"){
    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/wiki/pages")
  } else{
    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/wiki/", page)
  }

  if(verbose == TRUE) print(paste("Getting wiki from", link))

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as = "text"),
                               flatten = T)

  wiki <- result$data

  return(wiki)
}




#' Get trophies for a specified user
#'
#' @param user The username of the user
#' @param accesstoken The access token required to access the endpoint. Scope of
#' the acces token must be \code{"read"}.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#'
#' @return A dataframe with all trophies a specified user has received.
#' @export
#' @seealso \code{\link{get_user_info}}

get_trophies <- function (user = NULL,
                          accesstoken=NULL,
                          verbose = FALSE) {

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "read") stop("This function requires 'read' as scope of the token")
  }

  auth <- paste("bearer", accesstoken$access_token)

  link <- paste0("https://oauth.reddit.com/api/v1/user/", user,
                 "/trophies", ".json")

  if(verbose == TRUE) print(paste("Getting trophies from:", link))

  request <- httr::GET(link,
                 httr::add_headers(Authorization = auth),
                 httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  result <- jsonlite::fromJSON(httr::content(request, as="text"),
                               flatten = FALSE)

  trophies <- as.data.frame(result$data$trophies$data)

  return(trophies)
}




#' Get a list of subreddits
#'
#' @param type The type of list that is requested. Possible values are:
#' \itemize{
#'   \item \code{popular} Subreddits that are popular right now
#'   \item \code{new} Newly created subreddits
#'   \item \code{default} Default subreddits that users are subscribed to when
#'   they sign up for reddit.
#'   }
#' @param accesstoken The accesstoken required to access the endpoint. Scope
#' must be \code{"read"}.
#' @param limit The maximum number of subreddits to return. Must be a number
#' between 1 and 100.
#' @param before The fullname of an item serving as anchor in the
#' request. Items before this item in the listing are returned.
#' @param after The fullname of an item serving as anchor in the request.
#' Items after this item in the listing are returned.
#' @param verbose A logical flag whether information about the data extraction
#' should be printed to the console.
#'
#' @return A dataframe of subreddits.
#' @export
#'

get_subreddits <- function (type = c("popular", "new", "default"),
                            accesstoken = NULL,
                            limit = 100,
                            after = NULL,
                            before = NULL,
                            verbose = FALSE) {

  if(is.null(accesstoken)){
    stop("No token was specified")
  }else {
    if(is.null(accesstoken$useragent)) stop("No user agent was specified")

    if(Sys.time() - accesstoken$access_time > 3600) stop("Token is expired")

    if(accesstoken$scope != "read") stop("This function requires 'read' as scope of the token")
  }

  if(!type %in% c("popular", "new", "default"))
    stop("type has to be one of these: popular, new, default")

  if(!is.null(after) & is.null(before)){

    link <- paste0("https://oauth.reddit.com/subreddits/", type, ".json?limit=",
                   limit, "&after=", after)

  } else if(is.null(after) & !is.null(before)){

    link <- paste0("https://oauth.reddit.com/subreddits/", type, ".json?limit=",
                   limit, "&before=", before)

  }else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/subreddits/", type, ".json?limit=",
                   limit)
  }

  if(verbose == TRUE) print(paste("Getting subreddit info from: ", link))

  auth <- paste("bearer", accesstoken$access_token)

  request <- httr::GET(link,
                       httr::add_headers(Authorization = auth),
                       httr::user_agent(accesstoken$useragent))

  httr::stop_for_status(request)

  if(verbose == TRUE) print(httr::http_status(request)$message)

  response <- jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)

  subreddits <- response$data$children

  if(nrow(subreddits)>0){

    names(subreddits) <- sub("data.", "", names(subreddits))

    if(!is.null(response$data$after)){
      subreddits_after <<- response$data$after
    } else {
      subreddits_after <<- NULL
    }

    subreddits_before <<- subreddits$name[order(subreddits$created, decreasing = T)][1]

    return(subreddits)
  } else {print("No subreddits available")}
}
