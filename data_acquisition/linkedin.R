## Libraries ########
library(httr)
library(RJSONIO)
library(RCurl)

## OAuth redirect URI, necessary for API
#full_url <- oauth_callback()
#full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
#print(full_url)

oauthEndpoint <- oauth_endpoint(
  authorize = "https://www.linkedin.com/uas/oauth2/authorization",
  access = "https://www.linkedin.com/uas/oauth2/accessToken")

# API Credentials
app_name <- "TermPaper"
client_id <- ""
client_secret <- ""
scope <- "basic" #level of authorization 
oauthApp <- oauth_app(app_name, client_id, client_secret)

Sys.setenv("HTTR_SERVER_PORT" = "1410/")
oauthToken <- oauth2.0_token(oauthEndpoint, oauthApp, type = "application/x-www-form-urlencoded", cache=FALSE)
token <- strsplit(toString(names(oauthToken$credentials)), '"')[[1]][4]


## Establish Mongo DB Connection ########
source("helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()
mongoCollectionGeneral <- paste(db, "generalUser", sep=".")

if (mongoCollectionGeneral %in% mongo.get.database.collections(mongo, db)) {
  
  # Get basic User Information
  # r_fullprofile is only available for premium partners who pay for that
  #https://apigee.com/console/linkedin?req=%7B"resource"%3A"%2Fpeople%2F~%3A(id%2Cnum-connections%2Cpicture-url)%3Fformat%3Djson-get"%2C"params"%3A%7B"query"%3A%7B"format"%3A"json"%2C"parameters_name_1"%3A"format"%2C"parameters_value_1"%3A"json-get"%2C"parameters_name_2"%3A"format"%2C"parameters_value_2"%3A"json-get"%7D%2C"template"%3A%7B%7D%2C"headers"%3A%7B%7D%2C"body"%3A%7B"attachmentFormat"%3A"mime"%2C"attachmentContentDisposition"%3A"form-data"%7D%7D%2C"verb"%3A"get"%7D
  json <- rawToChar(GET("https://api.linkedin.com/v1/people/~:(id,first-name,last-name,formatted-name,headline,industry,num-connections,summary,specialties,positions:(title,summary,start-date,end-date,is-current,company:(name,type,industry)),public-profile-url,picture-url,educations)?format=json",
                        add_headers(Authorization=paste("Bearer", token, sep=" ")))$content)
  bson <- mongo.bson.from.JSON(json)
  mongo.insert(mongo, mongoCollectionGeneral, bson)

}

mongo <- disconnectMongoDb()
