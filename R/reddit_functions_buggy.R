


get_token <- function (scope = c("identity", "read", "history", "wikiread"),
                       useragent,
                       username,
                       password) {

  require(httr, warn.conflicts = FALSE, quietly = TRUE)

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
                       link_flair = FALSE,
                       author_flair = FALSE,
                       user_reports = FALSE,
                       secure_media = FALSE,
                       awards = FALSE,
                       after = NULL,
                       before = NULL,
                       verbose = TRUE) {

  library(httr, warn.conflicts = FALSE, quietly = TRUE)
  library(jsonlite, warn.conflicts = FALSE, quietly = TRUE)

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

  #make link
  if(is.null(before) & !is.null(after)){


    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {


    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/", sort,
                   ".json?limit=", limit)
  }

  if(verbose == TRUE) print(paste("Getting data from",link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(req)

  if(verbose == TRUE) print(http_status(req)$message)

  req <- content(req, as="text")

  res <- jsonlite::fromJSON(req, flatten = TRUE)

  posts <- as.data.frame(res$data$children)


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

    if(!is.null(res$data$after)){
      name_after <<- res$data$after
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
                             limit = 100,
                             before=NULL,
                             after=NULL,
                             verbose = FALSE) {

  require(httr, warn.conflicts = FALSE, quietly = TRUE)
  require(jsonlite, warn.conflicts = FALSE, quietly = TRUE)

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
                   "/submitted/", ".json?limit=", limit,"&sort=", sort, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/submitted/", ".json?limit=", limit,"&sort=", sort, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/submitted/", ".json?limit=", limit,"&sort=", sort)
  }

  if(verbose == TRUE) print(paste("Getting data from", link, collapse=":"))

  auth <- paste("bearer", accesstoken$access_token)

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent)
  )

  stop_for_status(req)

  if(verbose == TRUE) print(http_status(req)$message)

  res <- content(req, as="text")

  res <- jsonlite::fromJSON(res, flatten = TRUE)

  submissions <- as.data.frame(res$data$children)

  if (nrow(submissions)>0) {

    names(submissions) <- sub("data.", "", names(submissions))

    if(!is.null(res$data$after)){
      name_after <<- res$data$after
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

  require(httr, warn.conflicts = FALSE, quietly = TRUE)
  require(jsonlite, warn.conflicts = FALSE, quietly = TRUE)

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

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(req)

  if(verbose == TRUE) print(http_status(req)$message)

  res <- jsonlite::fromJSON(content(req, as = "text"), flatten = TRUE)

  comments <- res$data$children

  if(nrow(comments)>0){

    names(comments) <- sub("data.", "", names(comments))

    if(verbose == TRUE) print(paste(nrow(comments),"comments retrieved from reddit."))

    return(as.data.frame(comments))

    name_after <<- res$data$after

    name_before <<- comments[order(comments$created, decreasing = T),"name"][1]

  } else{
    print("No comments retrieved from reddit.")

    name_after <<- NA
    name_before <<- NA
  }
}


get_user_comms <- function (user = NULL,
                            accesstoken=NULL,
                            sort = "new",
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


  if(is.null(before) & !is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/comments/", ".json?limit=", limit,"&sort=", sort, "&after=", after)

  } else if (!is.null(before) & is.null(after)) {


    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/comments/", ".json?limit=", limit,"&sort=", sort, "&before=", before)

  } else if(!is.null(before) & !is.null(after)){

    stop ('Only one of "before" or "after" should be specified')

  } else if(is.null(before) & is.null(after)){

    link <- paste0("https://oauth.reddit.com/user/", user,
                   "/comments/", ".json?limit=", limit,"&sort=", sort)
  }

  if(verbose == TRUE) print(paste("Get data from", link, collapse = ","))

  auth <- paste("bearer", accesstoken$access_token)

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(req)


  if(verbose == TRUE) print(http_status(req)$message)

  res <- jsonlite::fromJSON(content(req, as="text"), flatten = TRUE)

  user_comments <- res$data$children

  if (nrow(user_comments)>0) {

    names(user_comments) <- sub("data.", "", names(user_comments))

    if(!is.null(res$data$after)){
      name_after <<- res$data$after
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

  if(verbose == TRUE) print(paste("Getting user info from", link, collapse=":"))

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(req)

  if(verbose == TRUE) print(http_status(req)$message)

  res <- jsonlite::fromJSON(content(req, as="text"), flatten = TRUE)

  user_info <- as.data.frame(res$data[-15])

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

  auth <- paste("bearer", accesstoken$access_token)

  link <- paste0("https://oauth.reddit.com/r/", subreddit, "/about.json")

  if(verbose == TRUE) print(paste("Getting subreddit info from", link))

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(req)

  if(verbose == TRUE) print(http_status(req)$message)

  res <- jsonlite::fromJSON(content(req, as = "text"), flatten = TRUE)

  df <- as.data.frame(res$data)

  return(df)
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

  if (page == "all") {
    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/wiki/pages")
  } else {
    link <- paste0("https://oauth.reddit.com/r/", subreddit, "/wiki/", page)
  }

  if(verbose == TRUE) print(paste("Getting wiki from", link))

  request <- GET(link,
             add_headers(Authorization = auth),
             user_agent(accesstoken$useragent))

  stop_for_status(request)

  if(verbose == TRUE) print(http_status(request)$message)

  result <- jsonlite::fromJSON(content(request, as = "text"), flatten = TRUE)

  wiki <- res$data

  return(wiki)
}


unnest_lists <- function(x) {

  if (names(x[1]) == "kind") {number <- 2
  } else {number <- 1}


  if (is.list(x[[number]])) {

    x <- lapply(x, function(y) ifelse(is.null(y), NA, y))

    x <- lapply(x, unnest_lists)

    } else {
      x <- unlist(x)
    }
}









get_postscores <- function (sub,
                            link_id,
                            accesstoken=NULL,
                            useragent=NULL,
                            sort ="new",
                            limit=0) {

  require(httr, quietly = TRUE)
  require(jsonlite, quietly = TRUE)

  #catching errors
  if(is.null(sub))stop("No subreddit was supplied")

  if(is.null(link_id))stop("No link id was supplied")

  if(is.null(useragent))stop("No user agent was specified")

  if(is.null(accesstoken)){
    accesstoken <- reddit_auth(scope="read", useragent = useragent)

    print("getting new token")

    token <<- accesstoken
  }else {

    timedif<- difftime(Sys.time(),as.POSIXct(accesstoken$access_time+3600, origin= "1970-01-01"))

    if(timedif>0 || (accesstoken$scope!="read" )) {

      accesstoken <- reddit_auth(scope="read", useragent = useragent)

      print("getting new token")

      token <<- accesstoken
    }
  }

  auth <- paste("bearer", accesstoken$access_token)

  link <- paste0("https://oauth.reddit.com/r/",sub, "/comments/", link_id,
                 ".json?limit=", limit,"&sort=", sort)

  print(link)
  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(useragent))

  stop_for_status(req)

  res <- content(req, as="text")

  res <- jsonlite::fromJSON(res)

  names <- c("id","name","score","ups","downs","upvote_ratio","num_comments","edited",
             "gilded", "locked", "stickied", "distinguished")

  df <- as.data.frame(res$data$children[[1]]$data[,names])


  ups <- if (df$upvote_ratio != 0.5) round((df$upvote_ratio*df$score)/(2*df$upvote_ratio - 1)) else round(df$score/2)
  df$downs <- ups - df$score
  df$ups <- ups

  return(df)
  Sys.sleep(2)
}



get_trophies <- function (user = NULL,
                          accesstoken=NULL,
                          useragent = NULL) {

  require(httr, quietly = TRUE)
  require(jsonlite, quietly = TRUE)

  if(is.null(useragent))stop("No user agent was specified")

  if(is.null(user)) stop("No user was specified")

  if(is.null(accesstoken)){

    accesstoken <- reddit_auth(scope="read", useragent = useragent)

    print("getting new token")

    token <<- accesstoken
  } else {

    timedif<- difftime(Sys.time(),as.POSIXct(accesstoken$access_time+3600, origin= "1970-01-01"))

    if(timedif>0 || (accesstoken$scope!="read" )) {

      accesstoken <- reddit_auth(scope="read", useragent = useragent)

      print("getting new token")

      token <<- accesstoken
    }
  }

  auth <- paste("bearer", accesstoken$access_token)

  link <- paste0("https://oauth.reddit.com/api/v1/user/", user,
                 "/trophies", ".json")

  print(paste("Getting trophies from", link, collapse = ":"))

  req <- GET(link,
             add_headers(Authorization = auth),
             user_agent(useragent)
  )

  if(http_error(req)==T){
    print(http_status(req)$message)
    api_error_ucomms <<- http_status(req)$message
  }


  res <- content(req, as="text")

  res <- jsonlite::fromJSON(res)

  if (length(res$data$trophies$data)!=0) {

    df <- as.data.frame(res$data$trophies$data)

    df <- cbind(user = rep(user, nrow(df)), df)

    vars <- c("user", "award_id", "id", "name", "description")

    df <- df[,vars]

    return(df)
  }
}


