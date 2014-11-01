#run this if you don't have one or more of the next packages:
#install.packages(c("devtools", "RCurl", "rjson", "bit64","httr","ROAuth"))
#library(devtools)
#install_github("geoffjentry/twitteR")
#install_github('ramnathv/rCharts')
library(RCurl)
library(data.table)
require("googleVis")
require(ggplot2)
require("OpenStreetMap")
require("sp")
require("rgdal")
library(RColorBrewer)

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

library(twitteR)

#Authentication for the Twitter API
#api_key <- ""
#api_secret <- ""
#access_token <- ""
#access_token_secret <- ""
source("secret_keys.R")
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

city.extent<-list(xmin=-114.25,xmax=-113.91,ymin=50.89,ymax=51.16)
grid.points<-expand.grid(x=seq(city.extent$xmin,city.extent$xmax,by=0.01),y=seq(city.extent$ymin,city.extent$ymax,by=0.01))

tweet.table<-data.table()
for(i in 1:nrow(grid.points)){
grid.points[i,]
tweets<-searchTwitter("#yyckidcount",n=100,locale = paste0(grid.points[i,1],",",grid.points[i,2],"0.25km"))
new.tweets<-rbindlist(lapply(tweets,FUN = function(x){x$toDataFrame()}))
new.tweets[,x:=grid.points[i,1]]
new.tweets[,y:=grid.points[i,2]]
tweet.table<-rbindlist(list(tweet.table,new.tweets))
}

save(tweet.table,file="tweet.table.Rda")

tweet.table[,number:=as.numeric(regmatches(text,regexpr(pattern = "[0-9]+",text)),by=c("id"))]

plot.data<-tweet.table[,list(kidcount=mean(number,na.rm=TRUE)),by=c("x","y")]
ggplot(plot.data,aes(x=x,y=y,fill=kidcount))+geom_tile()


dt.spatial<-copy(plot.data)
coordinates(dt.spatial)=~x+y
proj4string(dt.spatial)<-CRS("+proj=longlat")