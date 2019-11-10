library(rvest) # library for webscraping
url<-"https://www.imdb.com/search/title/?release_date=2016,2016&title_type=feature" # URL for scraping
webpage<-read_html(url) # reading html for the website
webpage  
# How to get select data from webpage that needs to be scraped
# Add selector tool as Chrome extension
# Click on the selector tool and select the data you want to collect
rank<-html_nodes(webpage,'.text-primary') # scraping 
rank_text<-html_text(rank) # reading text 
rank_text<-as.numeric(rank_text) #converting string into numeric
rank_text # This views the data in the vector
name<-html_nodes(webpage,'.lister-item-header a')
name_text<-html_text(name)
name_text
description<-html_nodes(webpage,'.ratings-bar+ .text-muted')
description_text<-html_text(description)
description_text
description_text<-gsub("\n","",description_text) # data cleaning 
description_text<-gsub("  ","",description_text)
description_text
runtime <- html_nodes(webpage,'.runtime')
runtime_text <- html_text(runtime_data_html)
runtime_text<- gsub("min","",runtime_text)
runtime_text<-as.numeric(runtime_text)
runtime_text
genre<- html_nodes(webpage,'.genre')
genre_text <- html_text(genre)
genre_text
genre_text<-gsub("\n","",genre_text)
genre_text<-gsub(" ","",genre_text)
genre_text<-gsub(",.*","",genre_text)
genre_text<-as.factor(genre_text)
genre_text
votes<-html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_text<-html_text(votes)
votes_text<-gsub(",","",votes_text)
votes_text<-as.numeric(votes_text)
votes_text
gross<-html_nodes(webpage,'.ghost~ .text-muted +span')
gross_text<-html_text(gross)
gross_text
gross_text<-gsub("M","",gross_text)
gross_text
gross_text<-substring(gross_text,2,6) # Extracting relevant data 
gross_text
gross_text<-as.numeric(gross_text)
gross_text
for (i in c(17,20,29,31,46)){
  
  a<-gross_text[1:(i-1)]
  
  b<-gross_text[i:length(gross_text)]
  
  gross_text<-append(a,list("NA"))
  
  gross_text<-append(gross_text,b)
  
} # missing value treatment
length(gross_text)
gross_text<-as.numeric(gross_text)
directors <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
directors_text <- html_text(directors)
directors_text<-gsub("</a>","",directors_text)
directors_text
metascore<-html_nodes(webpage,'.metascore')
metascore_text<-html_text(metascore)
metascore_text
for(i in c(17,39,46)){
  c<-metascore_text[1:(i-1)]
  d<-metascore_text[i:length(metascore_text)]
  metascore_text<-append(a,list("NA"))
  metascore_text<-append(metascore_text,b)
}
length(metascore_text)
metascore_text<-as.numeric(metascore_text)
metascore_text
summary(metascore_text)
rating<-html_nodes(webpage,".ratings-imdb-rating strong")
rating_text<-html_text(rating)
rating_text<-as.numeric(rating_text)
rating_text
#Forming a data frame with data collected
movies_df<-data.frame(Rank = rank_text, Name = name_text,Description = description_text,
                      Runtime = runtime_text,Genre = genre_text, Rating = rating_text,Metascore = metascore_text,
                      Votes = votes_text,Gross_Earning_in_Mil = gross_text,Director = directors_text)
View(movies_df)
library(ggplot2)
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
ggplot(movies_df,aes(x=Runtime,y=Rating))+geom_point(aes(size=Votes,col=Genre))
#Text analytics
library(tm) # text mining
library(SnowballC) # for text stemming
library(wordcloud) # word-cloud generator 
library(RColorBrewer) # color palettes
library(NLP) 
str(movies_df$Genre)
movies_df$Genre<-gsub(" ","",movies_df$Genre)
movies_df$Genre
str(movies_df$Genre)
#Corpus : A corpus is a large body of natural language text used for accumulating statistics on natural language text. The plural is corpora. Corpora often include extra information such as a tag for each word indicating its part-of-speech, and perhaps the parse tree for each sentence.
GenreCorpus <- data.frame(movies_df$Genre, stringsAsFactors = FALSE) 
GenreCorpus <- data.frame(doc_id=row.names(movies_df),text=movies_df$Genre)
Genrecorpus1 <- Corpus(DataframeSource(GenreCorpus ))
Genrecorpus1
wordcloud(Genrecorpus1, max.words = 100,random.order = TRUE,colors=brewer.pal(8, "Dark2"))
GenreCorpus2 <- data.frame(doc_id=row.names(movies_df),text=movies_df$Description,stringsAsFactors = FALSE)
GenreCorpus3 <- Corpus(DataframeSource(GenreCorpus2))
GenreCorpus3<-tm_map(GenreCorpus3,removeWords,stopwords("english"))
wordcloud(GenreCorpus3, max.words = 100,random.order = TRUE,colors=brewer.pal(8, "Dark2"))