# cannot use API as it is not possible to register phone number due to an error
# manually gathered the data via the Twitter API Console Tool:
#   https://dev.twitter.com/rest/tools/console
#
# Used API Endpoints:
#   https://dev.twitter.com/rest/reference/get/statuses/user_timeline
#     - Returns a collection of the most recent Tweets posted by the user
#       indicated by the screen_name or user_id parameters.
#     - https://api.twitter.com/1.1/statuses/user_timeline.json?count=200
#   https://dev.twitter.com/rest/reference/get/friends/list
#     - Returns a cursored collection of user objects for every user
#       the specified user is following (otherwise known as their “friends”).
#     - https://api.twitter.com/1.1/friends/list.json?count=200