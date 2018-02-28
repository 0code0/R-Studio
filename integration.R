library(Rlinkedin)

appName = 'analytics';
ClientID = "75qpzoksh68lgf";
ClientSecret = "GvHTR78DE4VVgrYo";

outh<-inOAuth(application_name = appName,consumer_key = ClientID,consumer_secret = ClientSecret)

getProfile(outh$endpoint,connections = T)


appcreate<-oauth_app(appname = appName,key = ClientID,secret = ,ClientSecret)

Token<-outh$credentials[[1]]

is.recursive(Token)

is.atomic(Token)

getMyConnections(outh)

library(XML)
