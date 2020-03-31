#' Checks token for their validity
#' 
#' This is an internal function.
#'
#' @param token The token to check
#' @param scope The scope of the token
#'
#' @return The function returns NULL if the token is valid.
#' @export
#'
#' @keywords internal

check_token <- function(token, scope){
  
  if(is.null(token)){
    stop("No token was specified")
  }else {
    if(is.null(token$useragent)) stop("No user agent was specified")
    
    if(Sys.time() - token$access_time > 3600) stop("Token has expired")
    
    if(token$scope != scope) stop(paste("This function requires", scope, "as scope of the token"))
  }
}


#' Checks whether valid arguments are supplied to a function
#' 
#' This is an internal function.
#'
#' @param default_arg Specifies an argument that has to be set by default.
#'
#' @return The function returns NULL if the arguments are valid.
#' @export
#'
#' @keywords internal

check_args <- function(default_arg = NULL){
  
  get_args <- function () {
    as.list(sys.frame(-2))
  }
  
  check_strings <- function(x, y){
    
    string_args <- c("user", "subreddit", "page", "after",
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


#' Builds the link to be passed to the API
#'
#' @param path_elements Specifies elements of the path 
#' @param query_elements Specifies elements of the query
#' @param before The fullname of an item serving as anchor in the request.
#' Items before this item in the listing are returned.
#' @param after The fullname of an item serving as anchor in the request.
#' Items after this item in the listing are returned.
#'
#' @return A string with the link to be passed to the API.
#' @export
#'
#' @keywords internal

build_link <-  function(path_elements,
                        query_elements = NULL,
                        before = NULL,
                        after = NULL){
  
  base_url <- ("https://oauth.reddit.com/")
  
  path <- paste0(path_elements, ".json")
  
  if(is.null(before) & !is.null(after)){
    
    query <- paste0(query_elements, "&after=", after)
    
  } else if (!is.null(before) & is.null(after)) {
    
    query <- paste0(query_elements, "&before=", before)
    
  } else if(!is.null(before) & !is.null(after)){
    
    stop ('Only one of "before" or "after" should be specified')
    
  } else if(is.null(before) & is.null(after)){
    
    query <- query_elements
  }
  
  link <- httr::modify_url(base_url, path = path, query = query)
  
  return(link)
}


#' GET method for the reddit API
#' 
#' This is an internal function.
#'
#' @param accesstoken The token for the GET method
#' @param link The link used in the GET method
#' @param verbose Boolean. A logical flag whether information about the data 
#' extraction should be printed to the console.
#' @param retry Boolean. A logical flag whether a failed api request should be 
#' retried if it fails.Requests will be tried up to three times with varying 
#' time intervals between requests.
#'
#' @return A JSON element
#' @export
#'
#' @keywords internal

get_reddit <- function(accesstoken,
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
  
  if (httr::http_type(request) != "application/json") {
    stop("reddit API did not return a json object", call. = FALSE)
  }
  
  if(verbose == TRUE) print(httr::http_status(request)$message)
  
  return(request)
}


#' Parse the request from the reddit API
#' 
#' The function parses the JSON object of the request of the reddit API and 
#' turns it into a dataframe or vector if so desired.
#'
#' @param request The request made to the reddit API
#' @param verbose Boolean. A logical flag whether information about the data 
#' extraction should be printed to the console.
#' @param output What the function should return. Choose \code{json} for an 
#' unparsed json object, \code{df} for a parsed object in form of a dataframe, 
#' and \code{all} for a list containg the json object, a dataframe, and the 
#' before and after anchors.
#' @param after_before Boolean. A logical flag whether \code{output = "all"} 
#' should return the before and after anchors.
#' 
#' @return A dataframe or vector.
#' @export
#'
#' @keywords internal

parse_request <- function(request,
                          verbose, 
                          output,
                          after_before = TRUE) {
  
  response_json <- httr::content(request, as="text")
  
  response_data <- jsonlite::fromJSON(response_json, 
                                      flatten = TRUE)
  
  if("dist" %in% names(response_data$data)) {
    if(response_data$data$dist == 0) {
      stop("reddit API returned an empty object. Please check the arguments of
    your request.")
    }
  }
  
  if(output == "json") {
    out <- response_json
  } else {
    
    if("data" %in% names(response_data)){
      data <- response_data$data
    } else {
      is_df <- sapply(response_data, is.data.frame)
      
      if(any(is_df)){
        data <- response_data[which(is_df)]
      }
    }
    
    if(length(data)>0){
      
      if(!is.data.frame(data)){
        
        is_df <- sapply(data, is.data.frame)
        
        if(any(is_df)){
          data <- data[[which(is_df)]]
        } else {
          
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
      }
      
      names(data) <- sub("data.", "", names(data))
      
      if(verbose == TRUE) print(paste(nrow(data),"item(s) retrieved from reddit."))
    } else {
      stop('Unable to locate data in request. Try rerunning it with output ==
           "json" and checking the json file.')
    }
    
    if(output == "all"){
      if(after_before == TRUE){
        
        after <- response_data$data$after
        
        before <- data[order(data$created, decreasing = T),"name"][1]
        
        out <- list(json = response_json, data.frame = data, after = after, 
                    before = before)
      } else{
        out <- list(json = response_json, data.frame = data)
      }
      
    } else if(output == "df"){
      out <- data
    }
  }
  return(out)
}


#' Checks credentials
#' 
#' Checks whether credentials such as username, password, client id, 
#' client secret are valid strings
#'
#' @param cred The credential to be checked
#'
#' @return The function returns NULL if the credential is valid.
#' @export
#'
#' @keywords internal
#' 
check_credentials <- function(cred) {
  assertthat::assert_that(assertthat::is.string(cred),
                          assertthat::not_empty(cred),
                          nchar(cred) > 0,
                          msg = paste(deparse(substitute(cred)),
                                      "needs to be passed as a character vector"))
}
