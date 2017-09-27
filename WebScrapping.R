# References:
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
# https://github.com/hadley/rvest/blob/master/demo/tripadvisor.R
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

# load libraries
library(RCurl)
library(XML)
library(lubridate)
library(ggplot2)

options(stringsAsFactors=FALSE)

getOnePage=function(urllink){
  
  # get html page content
  doc=htmlTreeParse(urllink,useInternalNodes=TRUE)
  
  ## get node sets
  # review id
  ns_id=getNodeSet(doc,"//div[@class='quote isNew' or @class='quote ' or @class='quote']/a[@href]") 
  # top quote for a review
  ns_topquote=getNodeSet(doc,"//div[@class='quote isNew' or @class='quote ' or @class='quote']/a[@href]/span") 
  # get partial entry for review that shows in the page
  ns_partialentry=getNodeSet(doc,"//div[@class='col2of2']//p[@class='partial_entry'][1]")
  # date of rating
  ns_ratingdt=getNodeSet(doc,"//div[@class='col2of2']//span[@class='ratingDate relativeDate' or @class='ratingDate']")
  # rating (number of stars)
  ns_rating=getNodeSet(doc,"//div[@class='col2of2']//span[@class='rate sprite-rating_s rating_s']/img[@alt]")
  
  # get actual values extracted from node sets
  # review id
  id=sapply(ns_id,function(x) xmlAttrs(x)["id"])
  # top quote for the review
  topquote=sapply(ns_topquote,function(x) xmlValue(x))
  # rating date (couple of formats seem to be used and hence a and b below)
  ratingdta=sapply(ns_ratingdt,function(x) xmlAttrs(x)["title"])
  ratingdtb=sapply(ns_ratingdt,function(x) xmlValue(x))
  # rating (number of stars)
  rating=sapply(ns_rating,function(x) xmlAttrs(x)["alt"])
  # partial entry for review
  partialentry=sapply(ns_partialentry,function(x) xmlValue(x))
  
  # get rating date in date format
  ratingdt.pick=ratingdta
  ratingdt.pick[is.na(ratingdta)]=ratingdtb[is.na(ratingdta)]
  ratingdt=mdy(gsub("Reviewed ","",ratingdt.pick))
  
  # put all the fields in a dataframe
  dfrating=data.frame(id=id,topquote=topquote,ratingdt=ratingdt,rating=rating,partialentry=partialentry)
  dfrating$ratingnum=as.numeric(substr(dfrating$rating,1,1),1,1)
  
  return(dfrating)
  
}

# Sample URL list from TripAdvisor
urlmainlist=c(
  lagvivmon = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d5006148-Reviews-Hotel_Laghetto_Viverone_Moinhos-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  porretbou = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d7153353-Reviews-Porto_Retro_Flat_Boutique-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  devpripoa = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d306290-Reviews-Hotel_Deville_Prime_Porto_Alegre-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  ibiscentro = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d7392609-Reviews-Ibis_Styles_Porto_Alegre_Centro-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  sheraton = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d304505-Reviews-Sheraton_Porto_Alegre_Hotel-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  radisson = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d300916-Reviews-Radisson_Porto_Alegre-Porto_Alegre_State_of_Rio_Grande_do_Sul.html",
  ritter = "https://www.tripadvisor.com.br/Hotel_Review-g303546-d7179012-Reviews-Ritter_Hotel-Porto_Alegre_State_of_Rio_Grande_do_Sul.html"
)

#Sequence to navigate through each comment page (X comments per page, X total comments, X to update the link)
morepglist=list(
  lagvivmon = seq(5,1190,5),
  porretbou = seq(5,95,5),
  devpripoa = seq(5,2105,5),
  ibiscentro = seq(5,595,5),
  sheraton = seq(5,595,5),
  radisson = seq(5,660,5),
  ritter = seq(5,730,5)
)
#----------------------------------------------------------------------------------------------------------

# Use these lists to scrap the comments

# url link for first search page
urllinkmain=urlmainlist[pickhotel]
# counter for additional pages
morepg=as.numeric(morepglist[[pickhotel]])

urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]

urllink=rep(NA,length(morepg)+1)

#Test if the URL is retrieving the data
urllink[1]=urllinkmain
for(i in 1:length(morepg)){
  urllink[i+1]=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
}
head(urllink)

#Create a Data Frame to store the data
DF <- as.data.frame(matrix(ncol = 4,nrow=1))
colnames(DF) <- c("id","quote","review","date")

for(j in 1:length(morepg)){
  urllink[j+1]=paste(urllinkpre,"-or",morepg[j],"-",urllinkpost,sep="")
url <- urllink[j]

reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble")

id <- reviews %>%
  html_node(".quote a") %>%
  html_attr("id")

quote <- reviews %>%
  html_node(".quote span") %>%
  html_text()

date <- reviews %>%
  #html_node(".rating .ratingDate") %>%
  #html_node(".ratingDate") %>%
  #html_node(".ratingDate relativeDate") %>%
  html_node(".relativeDate") %>%
  html_attr("title") #%>%
#strptime("%b %d, %Y") %>%
#as.POSIXct()

review <- reviews %>%
  html_node(".entry .partial_entry") %>%
  html_text()

newRow <- data.frame(id,quote,review,date,stringsAsFactors = FALSE)
DF <- rbind(DF,newRow)
}
DF_full <- rbind(DF, DF_full)
