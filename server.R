## server.R ##

shinyServer(function(input, output) {
  
  # call modules
  callModule(dashboard, "dashboardContent", mongo)
  callModule(facebook, "facebookContent", facebookDf, face_LikesCategory)
  callModule(linkedin, "linkedinContent", linkedIn_General, linkedIn_Education, linkedIn_Positions)
  callModule(instagram, "instagramContent", instagramDf, instagramTagcloudV)
  callModule(soundcloud, "soundcloudContent", soundcloudDf)
  callModule(foursquare, "foursquareContent", fCheckinsDf, fVenuesDf, four_PopularCategory)
  callModule(transactions, "transactionsContent", transactionsDf)
  callModule(twitter, "twitterContent", twitterDf, twitterTagcloudV, twitterFriendsDf, twitter_userInfo)

})
