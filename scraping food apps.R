library(rvest)
url<-"https://www.zomato.com/bangalore/delivery" # assigning the url
webpage<-read_html(url) # reading html elements of url
webpage
R_n<-html_nodes(webpage,'a.fontsize0') # extracting nodes for Restaurant name
R_nt<-html_text(R_n)  # Converting names to text format
R_nt
R_nt1<-gsub("\n"," ",R_nt) # data cleaning
R_nt1
R_nt2<-gsub(" ","",R_nt1) # data cleaning
R_nt2
length(R_nt2)
R_cost<-html_nodes(webpage,'.res-cost .pl0') # extracting nodes for cost
R_costT<-html_text(R_cost)
R_costT
R_costT1<-gsub("\u20b9","",R_costT) # data cleaning
R_costT1
R_costT2<-gsub(",","",R_costT1) # data cleaning
R_costT2<-as.numeric(R_costT2) # converting cost data from string to numeric
R_costT2
length(R_costT2)
R_cuisine<-html_nodes(webpage,'.nowrap.pl0') # extracting nodes for cuisine type
R_cuisineT<-html_text(R_cuisine)
R_cuisineT
R_rating<-html_nodes(webpage,'#orig-search-list .right') #extracting nodes for rating
R_ratingT<-html_text(R_rating)
R_ratingT
R_ratingT1<-gsub("\n","",R_ratingT) # data cleaning 
R_ratingT1
R_ratingT2<-gsub(" ","",R_ratingT1) # data cleaning
R_ratingT2<-as.numeric(R_ratingT2) # converting data type
R_ratingT2
R_votes<-html_nodes(webpage,'.col-s-4 .fontsize5') #extracting nodes for votes
R_votesT<-html_text(R_votes)
R_votesT
R_votesT1<-gsub("votes","",R_votesT)  # datacleaning
R_votesT2<-gsub(" ","",R_votesT1)   # datacleaning
R_votesT2<-as.numeric(R_votesT2)
# forming a dataframe with the above data
Zomato_df<-data.frame(Restaurant_name=R_nt2,Cost=R_costT2,Cuisine=R_cuisineT,rating=R_ratingT2,votes=R_votesT2)
View(Zomato_df)
# plots for rating vs cost in each restaurant
ggplot(Zomato_df,aes(x=rating,y=Cost))+geom_point(aes(size=votes,col=Restaurant_name))
qplot(data = Zomato_df,rating,fill = Restaurant_name,bins = 10)
m<-table(Zomato_df$Restaurant_name,Zomato_df$rating)
barplot(m,main="rating",xlab="Res_name")
Zomato_df$Cuisine<-gsub(",","",Zomato_df$Cuisine) # removing commas for text analytics
GenreCorpus <- data.frame(Zomato_df$Cuisine, stringsAsFactors = FALSE) # data frame for column of interest
GenreCorpus <- data.frame(doc_id=row.names(Zomato_df),text=Zomato_df$Cuisine) #converting the data frame to format which makes it easy to convert to corpus
Genrecorpus1 <- Corpus(DataframeSource(GenreCorpus )) # converting text to corpus
Genrecorpus1
wordcloud(Genrecorpus1, max.words = 100,random.order = FALSE,colors=brewer.pal(8, "Dark2")) # wordcloud
# from wordcloud it can be said that north indian and continental food is available in most of the restaurants used for analysis
