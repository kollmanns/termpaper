# https://github.com/pablobarbera/Rfacebook/tree/master/Rfacebook/R
## Libraries ########
library(httr)
library(RJSONIO)
library(RCurl)

oauthEndpoint <- oauth_endpoint(
  authorize = "https://www.facebook.com/dialog/oauth",
  access = "https://graph.facebook.com/oauth/access_token")

# API Credentials
app_name <- "TermPapeR"
app_id <- ""
app_secret <- ""
scope <- paste("user_birthday,user_hometown,user_location,user_relationships,",
               "publish_actions,user_status,user_likes", collapse="")
oauthApp <- oauth_app(app_name, app_id, app_secret)

Sys.setenv("HTTR_SERVER_PORT" = "1410/")
oauthToken <- oauth2.0_token(oauthEndpoint, oauthApp,
                             scope=scope, type = "application/x-www-form-urlencoded", cache=FALSE)

if (GET("https://graph.facebook.com/me", config(token=oauthToken))$status==200)
  print("Facebook authentication successful.")


## Establish Mongo DB Connection ########
source("helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()
mongoCollectionGeneral <- paste(db, "generalUser", sep=".")
mongoCollectionTemp <- paste(db, "TEMP_facebook", sep=".")


if (c(mongoCollectionTemp, mongoCollectionGeneral) %in% mongo.get.database.collections(mongo, db)) {
  
  # Get likes
  # maximum of 100 likes are received, therefore it is necessary to iterate until the end is reached
  urlToCall = paste("https://graph.facebook.com/v2.5/me/likes?",
                    "fields=id,about,category,category_list,description,general_info,",
                    "likes,link,location,name,talking_about_count,username,website&limit=100",
                    sep="")
  
  repeat {
    json <- rawToChar(GET(urlToCall, config(token=oauthToken))$content)
    jsonList = fromJSON(json)
    
    if (length(jsonList$data) != 0) {
      bson <- mongo.bson.from.JSON(json)
      mongo.insert(mongo, mongoCollectionTemp, bson)
    }
    
    # if the end of likes is reached, break the loop
    if (is.null(jsonList$paging$'next')) {
      break
    }
    else {
      urlToCall = jsonList$paging$'next'
    }
  }
  
  # Get user information
  json <- rawToChar(GET(paste("https://graph.facebook.com/?ids=me",
                              "&fields=id,name,first_name,middle_name,last_name,gender,locale,birthday,",
                              "location,hometown,relationship_status,picture.type(large)", sep=""),
                        config(token=oauthToken))$content)
  jsonList <- fromJSON(json)
  jsonList <- c(documentTypeId = "Facebook", jsonList$me)
  bson <- mongo.bson.from.list(jsonList)
  mongo.insert(mongo, mongoCollectionGeneral, bson)
  
  # aggregate to new table
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$data" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": { "_id": "$data.id", "about": "$data.about", "category": "$data.category", "description": "$data.description", "likes": "$data.likes", "link": "$data.link", "name": "$data.name", "username": "$data.username", "website": "$data.website", "talking_about_count": "$data.talking_about_count"} }')
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "facebook" }')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTemp, cmd_list)
}

mongo <- disconnectMongoDb()


