# scraping multiple pages
url<-paste0('https://www.zomato.com/bangalore/delivery?page=', 1:30)
name1<-function(name){
     m1<-read_html(name)
     m2<-html_nodes(m1,"a.fontsize0")
     m3<-html_text(m2)
}
Res_name<-lapply(url,name1)
Res_name
Restaurant_names<-unlist(Res_name)
Restaurant_names
Restaurant_names1<-gsub("\n ","",Restaurant_names)
Restaurant_names2<-gsub(" ","",Restaurant_names1)
Restaurant_names2
name1<-function(name){
  m1<-read_html(name)
  m2<-html_nodes(m1,".res-cost .pl0")
  m3<-html_text(m2)
}
Cost<-lapply(url,name1)
Cost
Cost1<-unlist(Cost)
costT1<-gsub("\u20b9","",Cost1) # data cleaning
costT1
costT2<-gsub(",","",costT1) # data cleaning
costT2<-as.numeric(costT2) # converting cost data from string to numeric
costT2
length(costT2)
name1<-function(name){
  m1<-read_html(name)
  m2<-html_nodes(m1,".nowrap.pl0")
  m3<-html_text(m2)
}
Cuisine <- lapply(url,name1)
Cuisine
Cuisine1<-unlist(Cuisine)
Cuisine1
Cuisine2<-gsub(",","",Cuisine1)
Cuisine2
name1<-function(name){
  m1<-read_html(name)
  m2<-html_nodes(m1,"#orig-search-list .right")
  m3<-html_text(m2)
}
rating <- lapply(url,name1)
rating
rating1<-unlist(rating)
rating1
rating2<-gsub("\n","",rating1)
rating3<-gsub(" ","",rating2)
rating3<-as.numeric(rating3)
rating3
# requires 
name1<-function(name){
  m1<-read_html(name)
  m2<-html_nodes(m1,".col-s-4 .fontsize5")
  m3<-html_text(m2)
}
votes<- lapply(url,name1)
votes
votes1<-unlist(votes)
votes1
votes2<-gsub("votes","",votes1)  # datacleaning
votes3<-gsub(" ","",votes2)   # datacleaning
votes3<-as.numeric(votes2)
votes3
Zomato_df<-data.frame(Restaurant_name=Restaurant_names2,Cost=costT2,Cuisine=Cuisine2,rating=rating3)
View(Zomato_df)
GenreCorpus <- data.frame(Zomato_df$Cuisine, stringsAsFactors = FALSE) # data frame for column of interest
GenreCorpus <- data.frame(doc_id=row.names(Zomato_df),text=Zomato_df$Cuisine) #converting the data frame to format which makes it easy to convert to corpus
Genrecorpus1 <- Corpus(DataframeSource(GenreCorpus )) # converting text to corpus
Genrecorpus1
wordcloud(Genrecorpus1, max.words = 100,random.order = FALSE,colors=brewer.pal(8, "Dark2"))
