## global.R ##

## Load Libraries
library(shiny)
library(shinydashboard)
library(leaflet)        # map
library(DT)             # displaying data tables
library(plyr)           # functions for lists, tables, data.frames, etc.
library(dplyr)          # only for data.frames
library(tm)             # text mining
library(wordcloud)      # display word clouds
library(ggplot2)        # enhanced plot package
library(scales)         # scale functions for visualizations

## Load Helper
source("./helper.R")
## Connect to Database
mongo <<- connectToMongoDb()
## Load Data
source("data.R")

## Load Modules
## http://shiny.rstudio.com/articles/modules.html
source("./server/dashboard.R")
source("./server/facebook.R")
source("./server/linkedin.R")
source("./server/instagram.R")
source("./server/soundcloud.R")
source("./server/foursquare.R")
source("./server/transactions.R")
source("./server/twitter.R")
