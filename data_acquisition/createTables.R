source("./helper.R")

db <- "sebastianKollmann"
mongo <- connectToMongoDb()

createTable("generalUser", db)
createTable("facebook", db)
createTable("TEMP_facebook", db)
createTable("instagram", db)
createTable("TEMP_instagram", db)
createTable("twitter", db)
createTable("twitterFriends", db)
createTable("foursquare", db)
createTable("TEMP_foursquareCheckins", db)
createTable("TEMP_foursquareVenueHistory", db)
createTable("TEMP_foursquareVenueLikes", db)
createTable("TEMP_foursquareTodoList", db)
createTable("soundcloud", db)
createTable("TEMP_soundcloud", db)
createTable("transactions", db)
createTable("linkedin", db)


mongo <- disconnectMongoDb()



