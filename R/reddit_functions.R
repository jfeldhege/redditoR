
require(httr, quietly = TRUE)
require(jsonlite, quietly = TRUE)

get_token <- function (scope = c("identity", "read", "history", "wikiread"),
                       useragent,
                       username,
                       password) {

  if(!scope %in% c("identity", "read", "history", "wikiread")) stop("Invalid scope")


  token <- POST("https://www.reddit.com/api/v1/access_token",
                body = list(
                  grant_type = "password",
                  username = username,
                  password = password,
                  scope = scope),
                encode = "form",
                authenticate("cTExHaVDTw05uQ", "hwoARWG2b5Q-rBD03kFEeRu6aRw"),
                user_agent(useragent)
  )

  stop_for_status(token)

  acc_token <- content(token)

  access_time <- Sys.time()
  acc_token <- c(acc_token,
                 access_time = access_time,
                 useragent = useragent)

  return (acc_token)
}


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

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

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

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent)
  )

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

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



get_comments <- function (sub,
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

    link <- paste0("https://oauth.reddit.com/r/", sub,
                   "/comments/.json?limit=", limit, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/r/", sub,
                   "/comments/.json?limit=", limit, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/r/",
                   sub, "/comments/.json?limit=", limit)
  }

  if(verbose == TRUE) print(paste("Getting data from",link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as = "text"), flatten = TRUE)

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


get_user_comms <- function (user,
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

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

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


get_overview <- function (user,
                          accesstoken,
                          sort = "new",
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

  if(is.null(user)) stop("No user was specified")

  if(!is.numeric(limit)) stop("limit has to be a number")

  if(limit > 100 | limit < 1) stop("limit has to be a number between 1 and 100")


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/overview/", ".json?limit=", limit,"&sort=", sort, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/overview/", ".json?limit=", limit,"&sort=", sort, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/overview/", ".json?limit=", limit,"&sort=", sort)
  }

  if(verbose == TRUE) print(paste("Getting data from", link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

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



get_user <- function (user,
                      accesstoken,
                      result = c("overview", "comments", "submitted", "gilded"),
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

  if(is.null(result)) stop("No result specified")

  if(!result %in% c("overview", "comments", "submitted", "gilded"))
    stop("Result has to be one of these: overview, comments, submitted, gilded")

  if(!is.null(time) & !time %in% c("hour", "day", "week", "month", "year", "all"))
    stop("Time has to be one of these: hour, day, week, month, year, all")


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", result,  "/.json?limit=", limit,
                   "&sort=", sort, "&after=", after, "&t=", time)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", result,  "/.json?limit=", limit,
                   "&sort=", sort, "&before=", before, "&t=", time)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/", result,  "/.json?limit=", limit,"&sort=", sort,"&t=", time)
  }

  if(verbose == TRUE) print(paste("Getting data from", link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  response <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

  result <- as.data.frame(response$data$children)

  if (nrow(result)>0) {

    names(result) <- sub("data.", "", names(result))

    if(!is.null(result$data$after)){
      name_after <<- result$data$after
    } else {
      name_after <<- NULL
    }

    name_before <<- result[order(result$created, decreasing = T),"name"][1]

    if(verbose == TRUE) print(paste(nrow(result),"result retrieved from reddit."))

    return(result)
  } else {print("No result available for this user")}
}


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

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

  user_info <- as.data.frame(result$data[-15])

  return(user_info)
}



get_subreddit_info <- function (subreddit = NULL,
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

  link <- paste0("https://oauth.reddit.com/r/", subreddit, "/about.json")

  if(verbose == TRUE) print(paste("Getting subreddit info from: ", link))

  auth <- paste("bearer", accesstoken$access_token)

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as = "text"), flatten = TRUE)

  sub_info <- result$data

  return(sub_info)
}


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

  link <- paste0("https://oauth.reddit.com/r/", subreddit, "/wiki/pages")


  if(verbose == TRUE) print(paste("Getting wiki from", link))

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as = "text"), flatten = TRUE)

  wiki <- result$data

  return(wiki)
}




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

  request <- GET(link,
                 add_headers(Authorization = auth),
                 user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as="text"), flatten = TRUE)

  trophies <- as.data.frame(result$data$trophies)

  return(trophies)
}


