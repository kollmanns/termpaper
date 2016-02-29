
##################################################
# General User Data ##############################
##################################################
# get data from MongoDB, Tags missing
pipe_1 <- mongo.bson.from.JSON('{ "$match" : { "documentTypeId" : "Facebook" } }')
pipe_2 <- mongo.bson.from.JSON('{ "$project": { "_id": 0, "Firstname": "$first_name", "Lastname": "$last_name", "Gender": "$gender", "Birthday": "$birthday", "Picture": "$picture.data.url", "Hometown": "$hometown.name" }}')
rslt <- mongo.aggregation(mongo, "sebastianKollmann.generalUser", list(pipe_1, pipe_2))
generalList <- mongo.bson.to.list(rslt)$result
# convert to data frame
generalData <- rbind.fill(lapply(generalList, as.data.frame))
generalData$Birthday <- as.Date(generalData$Birthday, format="%m/%d/%Y")

# add data which a bank would have manually
generalData$Mail <- ""
generalData$Address <- ""
generalData$Phone <- ""


##################################################
# Product Data - manually ########################
##################################################
products <- data.frame(
  name = "Debit Card",
  type = "Maestro",
  validUntil = as.Date("2016-10-10", format="%Y-%m-%d"),
  Number = 0000000000000000,
  IBAN = "0000 0000 0000 0000 0000",
  BIC = "GIBXXTWWXXX",
  NFCenabled = TRUE
  
)

##################################################
# Transactions ###################################
##################################################
# get data from MongoDB
transactionList <- getDataFromMongoDbAgg("transactions",
                                 paste0('{ "$project": { "_id": "$data.id", "booking": "$booking",',
                                        '"partnerName": "$parterName", "Parter_IBAN": "$partnerAccount.iban",',
                                        '"Partner_BIC": "$partnerAccount.bic",',
                                        '"amount": { "$divide": [ "$amount.value", 100 ] },',
                                        '"currency": "$amount.currency", "reference": "$reference" } }'))
# convert to data frame
transactionsDf <- rbind.fill(lapply(transactionList, as.data.frame))
# rename columns
colnames(transactionsDf)[names(transactionsDf) == "booking"] <- "Date"
colnames(transactionsDf)[names(transactionsDf) == "amount"] <- "Amount"
colnames(transactionsDf)[names(transactionsDf) == "reference"] <- "Text"
colnames(transactionsDf)[names(transactionsDf) == "currency"] <- "Currency"
colnames(transactionsDf)[names(transactionsDf) == "Partner_IBAN"] <- "IBAN (Partner)"
colnames(transactionsDf)[names(transactionsDf) == "Partner_BIC"] <- "BIC (Partner)"
# reorder columns
transactionsDf <- transactionsDf[c(1, 3, 2, 4, 5, 6)]
# convert to Date
transactionsDf$Date <- as.Date(substr(transactionsDf$Date, 1, 10), format="%Y-%m-%d")

##################################################
# Instagram ######################################
##################################################
# get data from MongoDB, Tags missing
igList <- getDataFromMongoDbAgg("instagram",
                                 paste0('{ "$project": { "Location": "$location.name", "location_lat": "$location.latitude", "location_long": "$location.longitude", "Filter": "$filter",  "Link": "$link", "image": "$images.standard_resolution.url", "image_width": "$images.standard_resolution.width", "image_height": "$images.standard_resolution.height", "imageThumb": "$images.thumbnail.url", "imageThumb_width": "$images.thumbnail.width", "imageThumb_height": "$images.thumbnail.height", "caption": "$caption.text", "created_time": "$caption.created_time" } } '))
# convert to data frame
instagramDf <- rbind.fill(lapply(igList, as.data.frame))
# convert to Datetime
instagramDf$created_time <- as.POSIXct(as.numeric(as.character(instagramDf$created_time)), origin="1970-01-01")


##################################################
# Instagram - Tagcloud ###########################
##################################################
instagramCaptions <- Corpus(VectorSource(encodeCorrect(instagramDf$caption)))
instagramCaptions <- tm_map(instagramCaptions, stripWhitespace)
instagramCaptions <- tm_map(instagramCaptions, content_transformer(tolower))
instagramCaptions <- tm_map(instagramCaptions, removePunctuation)
instagramCaptions <- tm_map(instagramCaptions, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(instagramCaptions, control = list(minWordLength = 1))
# terms which occur more than 10 times
# findFreqTerms(dtm, 3)

instagramTagcloudM <- as.matrix(dtm)
instagramTagcloudV <- sort(colSums(instagramTagcloudM), decreasing = TRUE, na.last = NA)

##################################################
# Facebook #######################################
##################################################
# get data from MongoDB, Tags missing
fbList <- getDataFromMongoDbAgg("facebook",
                                paste0('{ "$project": {  "_id": 0, "Name": "$name", "Username": "$username", "About": "$about", "Category": "$category", "Description": "$description", "Likes": "$likes", "FacebookURL": "$link", "Website": "$website" } }'))
# convert to data frame
facebookDf <- rbind.fill(lapply(fbList, as.data.frame))

# Most popular categories for Likes
face_LikesCategory <- facebookDf %>%
  group_by(Category) %>% summarize(count = n()) %>% arrange(desc(count))


##################################################
# SoundCloud #####################################
##################################################
# get data from MongoDB, Tags missing
scList <- getDataFromMongoDbAgg("soundcloud",
                                paste0('{ "$project": { "_id": 0, "Name": "$username", "Avatar": "$avatar_url", "City": "$city", "Country": "$country", "Description": "$description", "Tracks": "$track_count", "Followers": "$followers_count", "Website": "$website", "URL": "$permalink_url" } }'))

# convert to data frame
soundcloudDf <- rbind.fill(lapply(scList, as.data.frame))
soundcloudDf[c("Name", "Avatar", "Description", "URL")] <-
  lapply(soundcloudDf[c("Name", "Avatar", "Description", "URL")], as.character)

##################################################
# Foursquare - Checkins ##########################
##################################################
# get data from MongoDB, Tags missing
fCheckinsList <- getDataFromMongoDbAgg("foursquareCheckins",
                                paste0('{ "$project": { "_id": 0, "Time": "$createdAt", "Venue": "$venue", "Category": "$category", "Address": "$address", "Postal Code": "$postalCode", "City": "$city", "Country": "$country", "Longitude": "$longitude", "Latitude": "$latitude", "Category Icon": "$icon"} }'))

# convert to data frame
fCheckinsDf <- rbind.fill(lapply(fCheckinsList, as.data.frame))
# convert to Datetime
fCheckinsDf$Time <- as.POSIXct(as.numeric(as.character(fCheckinsDf$Time)), origin="1970-01-01")
fCheckinsDf[c("Venue")] <- lapply(fCheckinsDf[c("Venue")], as.character)

# Most popular categories
four_PopularCategory <- fCheckinsDf %>%
  group_by(Category) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

##################################################
# Foursquare - VenueHistory ######################
##################################################
# get data from MongoDB, Tags missing
fVenuesList <- getDataFromMongoDbAgg("foursquareVenueHistory",
                                       paste0('{ "$project": { "_id": 0, "Name": "$name", "Number of Checkins": "$beenHere", "Address": "$location.address", "Postal Code": "$location.postalCode", "City": "$location.city", "Country": "$location.country", "Longitude": "$location.lng", "Latitude": "$location.lat"} }'))

# convert to data frame
fVenuesDf <- head(rbind.fill(lapply(fVenuesList, as.data.frame)), 100)
colnames(fVenuesDf)[names(fVenuesDf) == "Number.of.Checkins"] <- "Count"
fVenuesDf[c("Name")] <- lapply(fVenuesDf[c("Name")], as.character)

##################################################
# LinkedIn #######################################
##################################################
# get data from MongoDB, Tags missing
pipe_1 <- mongo.bson.from.JSON('{ "$match" : { "documentTypeId" : "LinkedIn" } }')
pipe_2 <- mongo.bson.from.JSON('{ "$unwind": "$educations.values" }')
pipe_3 <- mongo.bson.from.JSON('{ "$unwind":  "$positions.values" }')
pipe_4 <- mongo.bson.from.JSON('{ "$project": { "_id": 0, "Firstname": "$firstName", "Lastname": "$lastName", "Picture": "$pictureUrl", "ProfileURL": "$publicProfileUrl", "GeneralIndustry": "$industry", "Connections": "$numConnections", "Degree": "$educations.values.degree", "SchoolName": "$educations.values.schoolName", "DegreeEndDate": "$educations.values.endDate.year", "DegreeStartDate": "$educations.values.startDate.year", "DegreeField": "$educations.values.fieldOfStudy", "Notes": "$educations.values.notes", "Position": "$positions.values.title", "PositionIsCurrent": "$positions.values.isCurrent", "Company": "$positions.values.company.name", "Industry": "$positions.values.company.industry", "PositionEndDateYear": "$positions.values.endDate.year", "PositionEndDateMonth": "$positions.values.endDate.month", "PositionStartDateYear": "$positions.values.startDate.year", "PositionStartDateMonth": "$positions.values.startDate.month" } }')
rslt <- mongo.aggregation(mongo, "sebastianKollmann.generalUser", list(pipe_1, pipe_2, pipe_3, pipe_4))
linkedInList <- mongo.bson.to.list(rslt)$result
# convert to data frame
linkedInDf <- rbind.fill(lapply(linkedInList, as.data.frame))

# group and remove redundant information
linkedIn_General <- linkedInDf %>%
  group_by(Firstname, Lastname, Picture, ProfileURL, GeneralIndustry, Connections) %>% summarize()

linkedIn_General[c("Firstname", "Lastname", "Picture", "ProfileURL", "GeneralIndustry")] <-
  lapply(linkedIn_General[c("Firstname", "Lastname", "Picture", "ProfileURL", "GeneralIndustry")], as.character)

linkedIn_Education <- linkedInDf %>%
  group_by(Degree, DegreeStartDate, DegreeEndDate, SchoolName, DegreeField, Notes) %>% summarize()
linkedIn_Education[c("Degree", "SchoolName", "DegreeField", "Notes")] <-
  lapply(linkedIn_Education[c("Degree", "SchoolName", "DegreeField", "Notes")], as.character)

linkedIn_Positions <- linkedInDf %>%
  group_by(Position, PositionIsCurrent, Company, Industry,
           PositionStartDateYear, PositionStartDateMonth,
           PositionEndDateYear, PositionEndDateMonth) %>% summarize()
linkedIn_Positions[c("Position", "Company", "Industry")] <-
  lapply(linkedIn_Positions[c("Position", "Company", "Industry")], as.character)

linkedIn_Positions$PositionEndDateYear[is.na(linkedIn_Positions$PositionEndDateYear)] <-
  as.numeric(format(Sys.Date(), "%Y"))
linkedIn_Positions$PositionEndDateMonth[is.na(linkedIn_Positions$PositionEndDateMonth)] <-
  as.numeric(format(Sys.Date(), "%m"))

linkedIn_Positions$StartYearMonth <- 
  as.Date(paste0(linkedIn_Positions$PositionStartDateYear, "-", linkedIn_Positions$PositionStartDateMonth, "-01"))
linkedIn_Positions$EndYearMonth <- 
  as.Date(paste0(linkedIn_Positions$PositionEndDateYear, "-", linkedIn_Positions$PositionEndDateMonth, "-01"))

##################################################
# Twitter ########################################
##################################################
# get data from MongoDB, Tags missing
tweetList <- getDataFromMongoDbAgg("twitter",
                                paste0('{ "$project": { "_id": 0, "Text": "$text", "CreatedAt": "$created_at", "User": "$user.name", "User_Screenname": "$user.screen_name", "User_Location": "$user.location", "User_Description": "$user.description", "User_Followers": "$user.followers_count", "User_Friends": "$user.friends_count", "User_CreatedAt": "$user.created_at", "User_ProfileImage": "$user.profile_image_url", "Retweets": "$retweet_count", "Favorites": "$favorite_count", "Language": "$language", "RetweetedStatus_Text": "$retweeted_status.text", "RetweetedStatus_CreatedAt": "$retweeted_status.created_at", "RetweetedStatus_User": "$retweeted_status.user.name", "RetweetedStatus_User_Screenname": "$retweeted_status.user.screen_name", "RetweetedStatus_User_Location": "$retweeted_status.user.location", "RetweetedStatus_User_Description": "$retweeted_status.user.description", "RetweetedStatus_User_Followers": "$retweeted_status.user.followers_count", "RetweetedStatus_User_Friends": "$retweeted_status.user.friends_count", "RetweetedStatus_User_CreatedAt": "$retweeted_status.user.created_at", "RetweetedStatus_User_ProfileImage": "$retweeted_status.user.profile_image_url", "RetweetedStatus_Retweets": "$retweeted_status.retweet_count", "RetweetedStatus_Favorites": "$retweeted_status.user.favorite_count" } }'))
# convert to data frame
twitterDf <- rbind.fill(lapply(tweetList, as.data.frame))

twitter_userInfo <- twitterDf %>%
  group_by(User, User_Screenname, User_Location, User_Description,
           User_ProfileImage,User_Followers, User_Friends, User_CreatedAt) %>% summarize()

# convert data, need to set local to C before as the weekday and month name are abbreviated in english
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
lct <- Sys.getlocale("LC_TIME") # "German_Austria.1252"
Sys.setlocale("LC_TIME", "C")
# help from https://github.com/SMAPPNYU/smappR
twitterDf$CreatedAt <- as.POSIXct(twitterDf$CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")
twitterDf$RetweetedStatus_User_CreatedAt <- as.POSIXct(twitterDf$RetweetedStatus_User_CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")
twitterDf$RetweetedStatus_CreatedAt <- as.POSIXct(twitterDf$RetweetedStatus_CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")

twitter_userInfo$User_CreatedAt <- as.POSIXct(twitter_userInfo$User_CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")

# set locale back to old value
Sys.setlocale("LC_TIME", lct)



##################################################
# Twitter - Tagcloud #############################
##################################################
# Removing punctuation...
tweetText <- gsub("|\\\\|\\.|\\,|\\;|\\:|\\'|\\&|\\-|\\?|\\!|\\)|\\(|-|‘|\\n|\\’|\\“|\\[", "",twitterDf$Text) 
tweetText <- gsub('\\"', "", tweetText) 

# preparing corpus of words
twitterText <- Corpus(VectorSource(encodeCorrect(tweetText)))
# convert to lower case
twitterText <- tm_map(twitterText, content_transformer(tolower))
# remove numbers
twitterText <- tm_map(twitterText, content_transformer(removeNumbers))
# remove URLS
removeURL <- function(x) gsub('"(http.*) |(http.*)$|\n', "", x)
twitterText <- tm_map(twitterText, content_transformer(removeURL))
# remove stop words
twitterText <- tm_map(twitterText, removeWords, stopwords("english"))
twitterText <- tm_map(twitterText, removeWords, stopwords("german"))
twitterText <- tm_map(twitterText, removeWords, c("fur", "via", "dass", "bzw", "warum"))

# building document term matrix
dtm <- DocumentTermMatrix(twitterText, control = list(minWordLength = 3))

# preparing word frequencies
twitterTagcloudM <- as.matrix(dtm)
twitterTagcloudV <- sort(colSums(twitterTagcloudM), decreasing = TRUE, na.last = NA)

##################################################
# Twitter - Friends ##############################
##################################################
# get data from MongoDB, Tags missing
twitterFList <- getDataFromMongoDbAgg("twitterFriends",
                                   paste0('{ "$project": { "_id": 0, "Name": "$name", "Screenname": "$screen_name", "Location": "$location", "Description": "$description", "Followers": "$followers_count", "Friends": "$friends_count", "CreatedAt": "$created_at", "NumberofTweets": "$statuses_count", "RecentStatus": "$status.text", "RecentStatus_CreatedAt": "$status.created_at", "RecentStatus_Retweets": "$status.retweet_count", "RecentStatus_Favorites": "$status.favorite_count", "Profile_BackgroundColor": "$profile_background_color", "Profile_BackgroundImage": { "$ifNull": [ "$profile_background_image_url", ""] }, "Profile_Image": "$profile_image_url", "Profile_TextColor": "$profile_text_color" } }'))
# convert to data frame
twitterFriendsDf <- rbind.fill(lapply(twitterFList, as.data.frame))
lct <- Sys.getlocale("LC_TIME") # "German_Austria.1252"
Sys.setlocale("LC_TIME", "C")
# help from https://github.com/SMAPPNYU/smappR
twitterFriendsDf$CreatedAt <- as.POSIXct(twitterFriendsDf$CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")
twitterFriendsDf$RecentStatus_CreatedAt <- as.POSIXct(twitterFriendsDf$RecentStatus_CreatedAt, format="%a %b %d %H:%M:%S %z %Y", tz = "GMT")
# set locale back to old value
Sys.setlocale("LC_TIME", lct)

##################################################
# Timeline for Dashboard #########################
##################################################
latestTransactions <- transactionsDf %>% filter(Date >= Sys.Date()-45)
for (i in 1: nrow(latestTransactions)) {
  latestTransactions$fullText[i] <- paste0(
    "<b>", latestTransactions$Amount[i], " ", latestTransactions$Currency[i],
    "</b>", br(), encodeCorrect(latestTransactions$Text[i])
  )
}

timeline <- data.frame(latestTransactions$Date, 
                       latestTransactions$fullText,
                       "credit-card", 
                       ifelse(latestTransactions$Amount >= 0, "success", "minusAmount"),
                       stringsAsFactors = FALSE
)
colnames(timeline) <- c("Date", "Text", "Icon", "Class")

igTimeline <- instagramDf %>% filter(as.Date(created_time) >= Sys.Date()-45)
for (i in 1: nrow(igTimeline)) {
  igTimeline$fullText[i] <- paste0("Posted a picture at <b>", encodeCorrect(igTimeline$Location[i]),
                                   "</b>", br(),
                                   img(src = igTimeline$imageThumb[i],
                                       width = igTimeline$imageThumb_width[i],
                                       height = igTimeline$imageThumb_height[i],
                                       class = "img-rounded")
  )
}

timeline2 <- data.frame(as.Date(igTimeline$created_time), igTimeline$fullText,
                        "instagram", "igBrown", stringsAsFactors = FALSE
)

colnames(timeline2) <- c("Date", "Text", "Icon", "Class")

timeline <- rbind(timeline, timeline2)

tweetTimeline <- twitterDf %>% filter(as.Date(CreatedAt) >= Sys.Date()-45)
for (i in 1: nrow(tweetTimeline)) {
  if (is.na(tweetTimeline$RetweetedStatus_Text[i])) {
    tweetTimeline$fullText[i] <- paste0("Tweeted about", br(),
                                        encodeCorrect(tweetTimeline$Text[i])
    )
  }
  else {
    tweetTimeline$fullText[i] <- paste0("Retweeted something from <b>",
                                        encodeCorrect(tweetTimeline$RetweetedStatus_User[i]),
                                        "</b>", br(),
                                        encodeCorrect(tweetTimeline$RetweetedStatus_Text[i])
    )
  }
  
}

timeline2 <- data.frame(as.Date(tweetTimeline$CreatedAt), tweetTimeline$fullText,
                        "twitter", "twitter", stringsAsFactors = FALSE
)

colnames(timeline2) <- c("Date", "Text", "Icon", "Class")

timeline <- rbind(timeline, timeline2)


timeline <- timeline %>% arrange(desc(Date))

##################################################
# Clean up #######################################
##################################################
rm(generalList, pipe_1, pipe_2, pipe_3, pipe_4, rslt, transactionList, igList,
   fbList, scList, fCheckinsList, fVenuesList, linkedInList, linkedInDf,
   instagramCaptions, dtm, instagramTagcloudM, tweetList, tweetText, twitterText, twitterTagcloudM,
   twitterFList, latestTransactions, igTimeline, tweetTimeline, timeline2, i)

# test to show negative account amount
# transactionsDf <- rbind(transactionsDf, c("2016-02-01", "test", -10, "EUR", "AT", "GIBa"))
# transactionsDf$Amount <- as.numeric(transactionsDf$Amount)


