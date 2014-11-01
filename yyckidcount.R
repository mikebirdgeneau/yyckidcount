library(RCurl)
library(data.table)
require(ggplot2)
require("sp")
require("rgdal")
library(RColorBrewer)
library("gstat")

refreshData<-TRUE # Set to TRUE to update data, otherwise set to FALSE to use saved data.

# Check if we should update the data
if(refreshData | !file.exists("all.tweets.Rda")){
  
  # Set SSL certs globally
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  library(twitteR)
  
  #Authentication for the Twitter API, copy these four lines into secret_keys.R & populate with your API key & access tokens:
  #api_key <- ""
  #api_secret <- ""
  #access_token <- ""
  #access_token_secret <- ""
  source("secret_keys.R") # includes the four lines above with API keys.
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  # Fetch tweets from Twitter with #yyckidcount hashtag
  tweets<-searchTwitter("#yyckidcount",n=5000,since = "2014-10-30",lang = NULL)
  all.tweets<-rbindlist(lapply(tweets,FUN = function(x){x$toDataFrame()}))
  
  save(all.tweets,file="all.tweets.Rda")
}

load("all.tweets.Rda")

# Load community names & shapefiles
communities<-readOGR("shp/CALGIS_ADM_COMMUNITY_DISTRICT/CALGIS_ADM_COMMUNITY_DISTRICT.shp","CALGIS_ADM_COMMUNITY_DISTRICT",verbose = TRUE)
community.names<-as.character(communities$NAME[which(communities$CLASS=="Residential")])

# Filter to tweets with neighbourhood names:
neighbourhood.tweets <- all.tweets[unique(grep(paste(community.names,collapse="|"),all.tweets$text,ignore.case=TRUE)),]

# Sort by date / time (or ID), and only get the latest from each person:
setkeyv(neighbourhood.tweets,c("id"))
neighbourhood.tweets<-neighbourhood.tweets[isRetweet==FALSE,]
neighbourhood.tweets[,last.tweet:=max(id),by=c("screenName")]
neighbourhood.tweets<-neighbourhood.tweets[last.tweet==id,]

matches <- gregexpr(paste(community.names,collapse="|"),neighbourhood.tweets$text,ignore.case=TRUE,)

neighbourhood.tweets[,community:=toupper(regmatches(neighbourhood.tweets$text,matches)),]

# Get tweets with only 1 number in them, and save the number - assume it's # of kids (otherwise this becomes time consuming!)
neighbourhood.tweets[,count.numbers:=as.numeric(length(gregexpr(pattern = "[0-9]+",text)[[1]])),by=c("id")]
neighbourhood.tweets[count.numbers==1 ,number:=as.numeric(regmatches(text,regexpr(pattern = "[0-9]+",text))),by=c("id")]

write.csv(neighbourhood.tweets,file="neighbourhood_tweets.csv",row.names=FALSE)

# Summarize by neighbourhood
ggplot(neighbourhood.tweets,aes(x=community,y=number))+geom_boxplot()
plot.data<-neighbourhood.tweets[,list(kidcount=median(number,na.rm=TRUE)),by=c("community")]

# Maniupulate data to store with shapefile!
communities@data<-data.frame(communities@data, data.frame(communities@data, plot.data[match(communities@data$NAME, plot.data$community),]))

# Format & Plot Data
plot.shp<-data.table(fortify(communities,region = "NAME"))
plot.df<-data.table(communities@data)
plot.df[,id:=NAME,]

p<-ggplot() +geom_map(data=plot.df, aes(map_id=id, fill=kidcount), map=plot.shp) + geom_path(data=plot.shp, aes(x=long, y=lat, group=group), colour="black", size=0.25)+ggtitle("#yyckidcount - Halloween 2014")+theme_minimal()+scale_fill_gradientn(name="Kidcount",colours = rev(brewer.pal(11,"Spectral")))+xlab("")+ylab("")+scale_y_continuous(labels=NULL)+scale_x_continuous(labels=NULL)+coord_fixed()+theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Save to file
png(filename = "yyckidcount_map.png",height = 700, width = 600,res = 84)
print(p)
dev.off()

print(p)

