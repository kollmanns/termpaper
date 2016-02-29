## ui.R ##

header <- dashboardHeader(
  title = "360° Customer View"
)

sidebar <- dashboardSidebar(
  sidebarUserPanel(
    name = paste(generalData$Firstname, generalData$Lastname, sep=" "),
    subtitle = generalData$Hometown,
    image = generalData$Picture
  ),
  sidebarMenu(
    # https://rstudio.github.io/shinydashboard/appearance.html#icons
    # Icons are from http://fontawesome.io/icons/
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Facebook", tabName = "facebook", icon = icon("facebook")),
    menuItem("Foursquare", tabName = "foursquare", icon = icon("foursquare")),
    menuItem("Instagram", tabName = "instagram", icon = icon("instagram")),
    menuItem("Linkedin", tabName = "linkedin", icon = icon("linkedin")),
    menuItem("Soundcloud", tabName = "soundcloud", icon = icon("soundcloud")),
    menuItem("Twitter", tabName = "twitter", icon = icon("twitter")),
    menuItem("Transactions", tabName = "transactions", icon = icon("credit-card"))
  )
)

body <- dashboardBody(
  # overwrite changes from leaflet.css to the user-panel
  # & style for timeline
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            dashboardUI("dashboardContent")
    ),
    tabItem(tabName = "facebook",
            h2("Facebook Likes"),
            facebookUI("facebookContent")
    ),
    tabItem(tabName = "foursquare",
            h2("Foursquare"),
            foursquareUI("foursquareContent")
    ),
    tabItem(tabName = "instagram",
            h2("Latest Pictures"),
            instagramUI("instagramContent")
    ),
    tabItem(tabName = "linkedin",
            linkedinUI("linkedinContent")
    ),
    tabItem(tabName = "soundcloud",
            h2("Favourite Music / Artists"),
            soundcloudUI("soundcloudContent")
    ),
    tabItem(tabName = "twitter",
            h2("Twitter"),
            twitterUI("twitterContent")
    ),
    tabItem(tabName = "transactions",
            h2("Transactions"),
            transactionsUI("transactionsContent")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  header,
  sidebar,
  body,
  skin = "red"
)