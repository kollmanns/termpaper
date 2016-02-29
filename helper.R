# https://cran.r-project.org/web/packages/rmongodb/vignettes/rmongodb_introduction.html
library(rmongodb)

connectToMongoDb <- function(host="127.0.0.1:27017") {
  mongo <<- mongo.create(host) # global Variable
  if (mongo.is.connected(mongo))
      return(mongo)
  else
    return(NULL)
}

encodeCorrect <- function(x) {
  return(
    iconv(
      iconv(x, from = "UTF-8", to = "Windows-1251"),
      from = "Windows-1251",
      to = "UTF-8"
    )
  )
}

disconnectMongoDb <- function() {
  return(mongo.destroy(mongo))
}

createTable <- function(name_of_the_table, dbName="sebastianKollmann"){

  if (mongo.is.connected(mongo) == TRUE) {
    allColl <- mongo.get.database.collections(mongo, dbName)
    
    if (name_of_the_table %in% allColl) {
      print("createTable: Table already exists.")
      return(TRUE)
    }
    else {
      if (is.null(mongo.simple.command(mongo, dbName, cmdstr = "create", arg=name_of_the_table))) {
        print("createTable: Creation of Table failed.")
        return(FALSE)
      }
      else {
        print("createTable: Table successfully created.")
        return(TRUE)
      }
    }
  }
  else {
    print("createTable: Not connected. Call connectToMongoDb() first.")
    return(FALSE)
  }
}

getDataFromMongoDbAgg <- function(collection="", jsonSearchString="{}",  db="sebastianKollmann") {
  ns <- paste(db, collection, sep=".")
  
  if (mongo.is.connected(mongo) == TRUE) {
    
    bson <- list(mongo.bson.from.JSON(jsonSearchString))
    bsonReturn <- mongo.aggregation(mongo = mongo, ns = ns, pipeline = bson)
    
    return(mongo.bson.to.list(bsonReturn)$result)
    
  }
  else {
    print("getDataFromMongoDbAgg: Not connected. Call connectToMongoDb() first.")
    return(NULL)
  }
}

