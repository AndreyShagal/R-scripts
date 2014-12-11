library(httr)
library(httpuv)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")
myapp <- oauth_app("github", key="5ee5ff132e89e89fe09a", secret="e04f5224f846dfe25f9c23a2ab6e16a8f7ee2ca4")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

stop_for_status(req)
content(req)


library(jsonlite)


jsonData <- fromJSON(req$url)
names(jsonData)

jsonData$name[5]
jsonData$created_at[5]




# OR:
req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
stop_for_status(req)
content(req)

