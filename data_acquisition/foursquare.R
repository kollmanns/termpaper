## Libraries ########
library(httr)
library(RJSONIO)
library(RCurl)

## OAuth redirect URI, necessary for API
#full_url <- oauth_callback()
#full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
#print(full_url)

oauthEndpoint <- oauth_endpoint(
  authorize = "https://foursquare.com/oauth2/authenticate",
  access = "https://foursquare.com/oauth2/access_token")

# API Credentials
app_name <- "TermPaperFSMapTEst"
client_id <- ""
client_secret <- ""
scope <- "basic" #level of authorization 
oauthApp <- oauth_app(app_name, client_id, client_secret)

oauthToken <- oauth2.0_token(oauthEndpoint, oauthApp, scope="basic", type = "application/x-www-form-urlencoded", cache=FALSE)
token <- strsplit(toString(names(oauthToken$credentials)), '"')[[1]][4]


## Establish Mongo DB Connection ########
source("helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()
mongoCollectionTempCheckins <- paste(db, "TEMP_foursquareCheckins", sep=".")
mongoCollectionTempVenueHistory <- paste(db, "TEMP_foursquareVenueHistory", sep=".")
mongoCollectionTempVenueLikes <- paste(db, "TEMP_foursquareVenueLikes", sep=".")
mongoCollectionTempTodoList <- paste(db, "TEMP_foursquareTodoList", sep=".")


if (c(mongoCollectionTempVenueHistory, mongoCollectionTempVenueLikes,
      mongoCollectionTempTodoList, mongoCollectionTempCheckins)
      %in% mongo.get.database.collections(mongo, db)) {
  
  # Get venue history
  json <- getURL(paste('https://api.foursquare.com/v2/users/self/venuehistory?oauth_token=',token,'&&v=20160123&m=foursquare',sep=""))
  bson <- mongo.bson.from.JSON(json)
  mongo.insert(mongo, mongoCollectionTempVenueHistory, bson)
  
  # aggregate to new table - foursquareVenueHistory
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$response.venues.items" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": { "_id": "$response.venues.items.venue.id", "documentTypeId": { "$literal": "VenueHistory" }, "beenHere": "$response.venues.items.beenHere", "name": "$response.venues.items.venue.name", "location": "$response.venues.items.venue.location", "categories": "$response.venues.items.venue.categories", "stats": "$response.venues.items.venue.stats" } }')
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "foursquareVenueHistory"}')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTempVenueHistory, cmd_list)
  
  # Get venue likes
  json <- getURL(paste('https://api.foursquare.com/v2/users/self/venuelikes?oauth_token=',token,'&v=20160123&m=foursquare', sep=""))
  bson <- mongo.bson.from.JSON(json)
  mongo.insert(mongo, mongoCollectionTempVenueLikes, bson)
  # aggregate to new table - foursquareVenueLikes
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$response.venues.items" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": {	"_id": "$response.venues.items.id", "documentTypeId": { "$literal": "VenueLikes" }, "name": "$response.venues.items.name", "location": "$response.venues.items.location", "categories": "$response.venues.items.categories", "stats": "$response.venues.items.stats", "like": "$response.venues.items.like", "ratedAt": "$response.venues.items.ratedAt", "tipHint": "$response.venues.items.tipHint"} }')
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "foursquareVenueLikes"}')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTempVenueLikes, cmd_list)
  
  # Get todo list
  json <- getURL(paste('https://api.foursquare.com/v2/lists/self/todos?oauth_token=',token,'&v=20160123&m=foursquare',sep=""))
  bson <- mongo.bson.from.JSON(json)
  mongo.insert(mongo, mongoCollectionTempTodoList, bson)
  # aggregate to new table - foursquareTodoList
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$response.list.listItems.items" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$project": {	"_id": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.id", "$response.list.listItems.items.tip.venue.id" ]}, "documentTypeId": { "$literal": "TodoList" }, "name": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.name", "$response.list.listItems.items.tip.venue.name" ] }, "location": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.location", "$response.list.listItems.items.tip.venue.location" ] }, "categories": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.categories", "$response.list.listItems.items.tip.venue.categories" ] }, "stats": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.stats",  "$response.list.listItems.items.tip.venue.stats" ] }, "price": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.price",  "$response.list.listItems.items.tip.venue.price" ] }, "tastes": { "$cond": [ { "$eq": ["$response.list.listItems.items.type", "venue"] }, "$response.list.listItems.items.venue.tastes", "$response.list.listItems.items.tip.venue.tastes" ] }}}') 
  pipe_3 <- mongo.bson.from.JSON('{ "$out": "foursquareTodoList"}')
  cmd_list <- list(pipe_1, pipe_2, pipe_3)
  res <- mongo.aggregation(mongo, mongoCollectionTempTodoList, cmd_list)
  
  # Get checkins
  # maximum of 250 checkins are received, therefore it is necessary to iterate until the end is reached
  baseUrl <- paste0('https://api.foursquare.com/v2/users/self/checkins?oauth_token=',
                    token, '&v=20160123&m=foursquare&limit=200')
  urlToCall <- baseUrl
  i <- 0
  
  repeat {
    json <- getURL(urlToCall)
    jsonList = fromJSON(json)
    
    numberOfItems <- length(jsonList$response$checkins$items)
    if (numberOfItems > 0) {
      bson <- mongo.bson.from.JSON(json)
      mongo.insert(mongo, mongoCollectionTempCheckins, bson)
    }
    
    # if the end of likes is reached, break the loop
    if (numberOfItems == 200) {
      i <- i+200
      urlToCall <- paste0(baseUrl, '&offset=', i)
    }
    else {
      break
    }
  }
  
  # aggregate to new table - foursquareCheckins
  pipe_1 <- mongo.bson.from.JSON('{ "$unwind": "$response.checkins.items" }')
  pipe_2 <- mongo.bson.from.JSON('{ "$unwind": "$response.checkins.items.venue.categories" }')
  pipe_3 <- mongo.bson.from.JSON('{ "$project": { "_id": "$response.checkins.items.id", "documentTypeId": { "$literal": "Checkin" }, "createdAt": "$response.checkins.items.createdAt", "venue": "$response.checkins.items.venue.name", "address": "$response.checkins.items.venue.location.address", "longitude": "$response.checkins.items.venue.location.lng", "latitude": "$response.checkins.items.venue.location.lat", "postalCode": "$response.checkins.items.venue.location.postalCode", "city": "$response.checkins.items.venue.location.city", "country": "$response.checkins.items.venue.location.country", "category": "$response.checkins.items.venue.categories.name", "icon": { "$concat": [ "$response.checkins.items.venue.categories.icon.prefix", "$response.checkins.items.venue.categories.icon.suffix" ] } } }')
  pipe_4 <- mongo.bson.from.JSON('{ "$out": "foursquareCheckins"}')
  cmd_list <- list(pipe_1, pipe_2, pipe_3, pipe_4)
  res <- mongo.aggregation(mongo, mongoCollectionTempCheckins, cmd_list)
  
  
 }

mongo <- disconnectMongoDb()


