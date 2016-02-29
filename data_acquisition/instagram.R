# http://thinktostart.com/analyze-instagram-r/
## Libraries ########
library(httr)
library(RJSONIO)
library(RCurl)

## OAuth redirect URI, necessary for API
#full_url <- oauth_callback()
#full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
#print(full_url)

oauthEndpoint <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")

# API Credentials
app_name <- "Term Paper"
client_id <- ""
client_secret <- ""
scope <- "basic" #level of authorization 
oauthApp <- oauth_app(app_name, client_id, client_secret)

oauthToken <- oauth2.0_token(oauthEndpoint, oauthApp, scope="basic", type = "application/x-www-form-urlencoded", cache=FALSE)
token <- strsplit(toString(names(oauthToken$credentials)), '"')[[1]][4]


## Get Profile ID, necessary for further requests
username <- "le_kols"
user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
received_profile <- user_info$data[[1]]
userId <- 1598775526 #received_profile["id"]


## Establish Mongo DB Connection ########
source("helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()
mongoCollectionGeneral <- paste(db, "generalUser", sep=".")
mongoCollectionTemp <- paste(db, "TEMP_instagram", sep=".")

if (c(mongoCollectionTemp, mongoCollectionGeneral)  %in% mongo.get.database.collections(mongo, db)) {
  
  # Get basic User Information
  json <- getURL(paste('https://api.instagram.com/v1/users/',userId,'/?access_token=',token,sep=""))
  json <- gsub('\\n',' ', json) # remove unwanted characters
  jsonList <- fromJSON(json)
  jsonList <- c(documentTypeId = "Instagram", jsonList$data)
  bson <- mongo.bson.from.list(jsonList)
  mongo.insert(mongo, mongoCollectionGeneral, bson)
  
  #mongo.find.one(mongo, mongoCollection)
  # cursor <- mongo.find(mongo, mongoCollection, query = '{ "data.username": "le_kols" }')
  # df <- (mongo.cursor.to.list(cursor))
  # df[[1]][['data']][['username']]
  
  # Get most recent media (20 Pictures)
  # maximum of 20 pictures are received, therefore it is necessary to iterate until the end is reached
  urlToCall = paste('https://api.instagram.com/v1/users/',userId,'/media/recent/?access_token=',token,sep="")
  
  repeat {
    json <- getURL(urlToCall)

    jsonList = fromJSON(json)
    
    if (length(jsonList$data) != 0) {
      json <- gsub('\\n',' ', json) # remove unwanted characters
      bson <- mongo.bson.from.JSON(json)
      mongo.insert(mongo, mongoCollectionTemp, bson)
    }
    
    # if the end of likes is reached, break the loop
    urlToCall = jsonList$pagination[['next_url']]
    if (is.null(urlToCall)) {
      break
    }
  }
  
  
  # aggregate to new table
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$data" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": { "_id": "$data.id", "attribution": "$data.attribution", "tags": "$data.tags", "type": "$data.type", "location": "$data.location", "comments": "$data.comments", "filter": "$data.filter", "created_time": "$data.created_time", "link": "$data.link", "images": "$data.images", "users_in_photo": "$data.users_in_photo", "caption": "$data.caption" } }')
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "instagram"}')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTemp, cmd_list)
  
}

mongo <- disconnectMongoDb()






