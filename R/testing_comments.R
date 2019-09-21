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

id <- posts$id[4]

sub <-
article <- id
limit <- 10
depth <- 1
showmore <- 1


link <- paste0("https://oauth.reddit.com/r/",
               sub, "/comments/", article, ".json?limit=", limit,
               "&depth=", depth, "&showmore=", showmore)


auth <- paste("bearer", read_token$access_token)

request <- GET(link,
               add_headers(Authorization = auth),
               user_agent(accesstoken$useragent))


stop_for_status(request)


result <- jsonlite::fromJSON(content(request, as = "text"), flatten = TRUE)


comments <- result$data.children[[2]]
