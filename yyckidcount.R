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
library("gstat")

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

library(twitteR)

#Authentication for the Twitter API
#api_key <- ""
#api_secret <- ""
#access_token <- ""
#access_token_secret <- ""
source("secret_keys.R") # includes the four lines above with API keys.
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

city.extent<-list(xmin=-114.3,xmax=-113.8,ymin=50.8,ymax=51.2)
grid.points<-expand.grid(x=seq(city.extent$xmin,city.extent$xmax,by=0.01),y=seq(city.extent$ymin,city.extent$ymax,by=0.01))

if(file.exists("tweet.table.Rda")){
  load("tweet.table.Rda")
} else {
  tweet.table<-data.table()
}

pb<-txtProgressBar(min=1,max=nrow(grid.points),style = 3)
setTxtProgressBar(pb,value = 0)
for(i in 1:nrow(grid.points)){
  setTxtProgressBar(pb,value = i)
  if(nrow(tweet.table[x==grid.points[i,1] & y==grid.points[i,2],])<1){
    tweets<-searchTwitter("#yyckidcount",n=100,geocode = paste0(formatC(grid.points[i,2],digits=4,format="f"),",",formatC(grid.points[i,1],digits=4,format="f"),",5km"),since = "2014-10-30",lang = NULL)
    if(length(tweets)!=0){
      new.tweets<-rbindlist(lapply(tweets,FUN = function(x){x$toDataFrame()}))
      new.tweets[,i:=i,]
      new.tweets[,x:=ifelse(is.finite(longitude),longitude,grid.points[i,1]),]
      new.tweets[,y:=ifelse(is.finite(latitude),latitude,grid.points[i,2]),]
      tweet.table<-rbindlist(list(tweet.table,new.tweets))
    }
  }
}
setkeyv(tweet.table,c("id"))
tweet.table<-unique(tweet.table)
save(tweet.table,file="tweet.table.Rda")

if(file.exists("map.Rda")){
  load("map.Rda")
}
if(!exists("map")){
  map <- openmap(c(city.extent$ymax,city.extent$xmin),c(city.extent$ymin,city.extent$xmax),zoom=NULL,type='osm-bw',minNumTiles=32)
  save(map,file="map.Rda")
}

tweet.table$number<-NULL
tweet.table$number<-0
tweet.table$number<-NA
tweet.table[,count:=as.numeric(length(gregexpr(pattern = "[0-9]+",text)[[1]])),by=c("id")]
tweet.table[count==1 ,number:=as.numeric(regmatches(text,regexpr(pattern = "[0-9]+",text))),by=c("id")]

plot.data<-tweet.table[count==1 & is.finite(number),list(kidcount=mean(number,na.rm=TRUE),tweetcount=sum(number*0+1)),by=c("x","y")]
ggplot(plot.data,aes(x=x,y=y,colour=kidcount))+geom_point()

# Convert to Spatial Data
dt.spatial<-copy(plot.data)
coordinates(dt.spatial)=~x+y
proj4string(dt.spatial)<-CRS("+proj=longlat")
dt.spatial<-spTransform(dt.spatial,CRS(map$tiles[[1]]$projection@projargs))
plot.data2<-data.frame(x=dt.spatial@coords[,1],y=dt.spatial@coords[,2],kidcount=dt.spatial$kidcount)

cext<-expand.grid(x=c(city.extent$xmin,city.extent$xmax),y=c(city.extent$ymin,city.extent$ymax))
coordinates(cext) = ~x+y
proj4string(cext)<-CRS("+proj=longlat")
cext<-spTransform(cext,CRS(map$tiles[[1]]$projection@projargs))

dt.newdata<-expand.grid(x=seq(bbox(cext)[1,1],bbox(cext)[1,2],length.out=100),y=seq(bbox(cext)[2,1],bbox(cext)[2,2],length.out=100))
gridded(dt.newdata) = ~x+y
#proj4string(dt.newdata)<-CRS(map$tiles[[1]]$projection@projargs)

v1<-variogram(kidcount~1,data = dt.spatial)
plot(v1)
v1.fit<-fit.variogram(v1,vgm(1,"Exp",8000,1))
dt.result<-krige(kidcount~1,locations=~x+y,data=plot.data2,newdata=dt.newdata,beta=0,model=v1.fit)
dt.result<-data.table(kidcount=dt.result$var1.pred,x=dt.result@coords[,1],y=dt.result@coords[,2])

p<-autoplot(map)+geom_tile(data=dt.result,aes(x=x,y=y,fill=kidcount),alpha=0.35)+scale_fill_gradientn(name="KidCount",colours = rev(brewer.pal(n = 11,name = "Spectral")))+ggtitle("#YYCKidCount - 2014 Haloween")+xlim(bbox(cext)[1,1],bbox(cext)[1,2])+ylim(bbox(cext)[2,1],bbox(cext)[2,2])
png(filename = "yyckidcount.png",width = 600,height=800)
print(p)
dev.off()
print(p)

