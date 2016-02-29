## Libraries ########
library(httr)
library(RJSONIO)
library(RCurl)

## OAuth redirect URI, necessary for API
#full_url <- oauth_callback()
#full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
#print(full_url)

oauthEndpoint <- oauth_endpoint(
  authorize = "https://soundcloud.com/connect",
  access = "https://api.soundcloud.com/oauth2/token")

# API Credentials
app_name <- "termpapeR"
client_id <- ""
client_secret <- ""
oauthApp <- oauth_app(app_name, client_id, client_secret)

oauthToken <- oauth2.0_token(oauthEndpoint, oauthApp, type = "application/x-www-form-urlencoded", cache=FALSE)
token <- strsplit(toString(names(oauthToken$credentials)), '"')[[1]][4]


## Establish Mongo DB Connection ########
source("helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()
mongoCollectionGeneral <- paste(db, "generalUser", sep=".")
mongoCollectionTemp <- paste(db, "TEMP_soundcloud", sep=".")


if (c(mongoCollectionTemp, mongoCollectionGeneral) %in% mongo.get.database.collections(mongo, db)) {
  
  # Get general user info
  json <- getURL(paste('https://api.soundcloud.com/me?oauth_token=',token, sep=""))
  jsonList <- fromJSON(json)
  soundcloud_user_uri <- jsonList$uri
  jsonList <- c(documentTypeId = "Soundcloud", jsonList)
  bson <- mongo.bson.from.list(jsonList)
  mongo.insert(mongo, mongoCollectionGeneral, bson)
  
  # get Followings
  urlToCall <- paste(soundcloud_user_uri, '/followings?client_id?',client_id,
                     '&oauth_token=', token, sep="")
  
  repeat {
    json <- getURL(urlToCall)
    jsonList = fromJSON(json)
    
    if (length(jsonList$collection) != 0) {
      bson <- mongo.bson.from.list(jsonList)
      mongo.insert(mongo, mongoCollectionTemp, bson)
    }
    
    # if the end of likes is reached, break the loop
    if (is.null(jsonList$next_href)) {
      break
    }
    else {
      urlToCall = jsonList$next_href
    }
  }
  
  # aggregate to new table
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$collection" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": { "_id": "$collection.id", "first_name": { "$ifNull": [ "$collection.first_name", ""] }, "last_name": { "$ifNull": [ "$collection.last_name", ""] }, "full_name": { "$ifNull": [ "$collection.full_name", ""] }, "city": { "$ifNull": [ "$collection.city", ""] }, "description": { "$ifNull": [ "$collection.description", ""] }, "followers_count": "$collection.followers_count", "country": { "$ifNull": [ "$collection.country", ""] }, "track_count": "$collection.track_count", "followings_count": "$collection.followings_count", "website": { "$ifNull": [ "$collection.website", ""] }, "avatar_url": "$collection.avatar_url", "permalink_url": "$collection.permalink_url", "username": "$collection.username", "permalink": "$collection.permalink", "last_modified": "$collection.last_modified" } }')
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "soundcloud" }')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTemp, cmd_list)

}

mongo <- disconnectMongoDb()


