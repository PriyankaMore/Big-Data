#Group Project 2 - Text Analytics in R
#Due Date: 18 of July 2016

#install the package to deal with text:
install.packages("tm")
install.packages("NLP")
install.packages("qdap")
install.packages("qdapDictionaries")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("Rgraphviz")
install.packages("SnowballC")
install.packages("biclust")
install.packages("cluster")
install.packages("igraph")
install.packages("fpc")
install.packages("Rcampdf", repos="http://datacube.wu.ac.at/",type="source")
install.packages("textmineR")

#install the package to deal with plots:
install.packages("ggplot2")
install.packages("wordcloud")


#to activate the libraries:
library("NLP")
library("tm")
library("ggplot2")
library("wordcloud")
library("qdap") # Quantitative discourse analysis of transcripts.
library("qdapDictionaries")
library("dplyr") # Data wrangling, pipe operator %>%().
library("RColorBrewer") # Generate palette of colours for plots.
library("scales") # Include commas in numbers.
library("Rgraphviz") # Correlation plots.
library("stringi")#to deal with Text
library("stringr")
library("stringdist")
library("SnowballC")
library("biclust")
library("cluster")
library("igraph")
library("fpc")
library("Rcampdf")
library("textmineR") #For textminR Package
library("slam")#To convert to A document term matrix of class dgCMatrix
library("Matrix")

#Create a corpus of documents to be processed.

#50 Exemplary News Articles from the Reuters-21578 Data Set of Topic
data("acq")
acq

#Inspect the VCorpus
inspect(acq)

#______________________________________________________________________________________
#______________________________________________________________________________________
#______________________________________________________________________________________
#Pri: 

#obtain vetor/matrix of corpuses
texts <- sapply(acq$content, "[[", "content")
?sapply

################################################################
#Implementing merge sort for obtaining top 10 longest documents
################################################################

mmerge<-function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && nchar(a[ai])<nchar(b[bi])) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1          
    }
  }
  r
}
mmergesort<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q+1):length(A)])
    mmerge(a,b)
  } else {
    A
  }
}

#calling merge sort
a<-mmergesort(texts)
nchar(a)

#Arranging documents in decreasing order
topten <- a[50:41]
temp<-topten

#top ten longest document set
nchar(temp)

#Remove all \n from documents
temp<-str_replace_all(temp, "[\r\n]" , " ")

#______________________________________________________________________________________
#______________________________________________________________________________________
#______________________________________________________________________________________





#1- Function to find the length: 
lengthFun <- function(x1){
  a<- temp[x1]
  cat("\n\n")
  cat(paste("\n**Content of document: ", x1 , " \n"))
  cat(a, sep="\n\n")
}









x=1

for (x in 1:10){
  
  #To store plots as jpg we need to creat file names for the plots
  
  Plot1Saved <- paste("FreqWordDiag",x,".jpg")
  Plot1Saved1 <-gsub(" ", "", Plot1Saved, fixed = TRUE)
  
  Plot2Saved <- paste("Hirarchial_Cluster",x,".jpg")
  Plot2Saved1 <-gsub(" ", "", Plot2Saved, fixed = TRUE)
  
  Plot3Saved <- paste("PlotCloud_Black",x,".jpg")
  Plot3Saved1 <-gsub(" ", "", Plot3Saved, fixed = TRUE)
  
  Plot4Saved <- paste("PlotCloud_Color",x,".jpg")
  Plot4Saved1 <-gsub(" ", "", Plot4Saved, fixed = TRUE)
  
  #lengthFun(temp[x])
  
  
  #Conver the text to Corpus:
  mycorpus <- Corpus(VectorSource(temp[x]))
  
  inspect(mycorpus)
  
  #-----------------------------------
  #Cleaning Process
  #----------------------------------- 
  
  
  #Removing punctuation:
  mycorpus <- tm_map(mycorpus, removePunctuation)
  #Removing numbers:
  mycorpus <- tm_map(mycorpus,removeNumbers)
  #Removing special characters
  #removeSpecialCharFun(mycorpus)
  #Removing "stopwords" (common words) that usually have no analytic value
  mycorpus <- tm_map(mycorpus,removeWords,stopwords("english"))
  #Removing particular words
  mycorpus<- tm_map(mycorpus,removeWords,c("department","email"))
  #Removing common word endings (e.g., "ing", "es", "s") 
  mycorpus<- tm_map(mycorpus,stemDocument)
  #Removing unnecesary whitespace from your documents: 
  mycorpus <- tm_map(mycorpus,stripWhitespace)
  #To Finish Be sure to use the following script once you have completed preprocessing. This tells R to treat your preprocessed documents as text documents.
  mycorpus <- tm_map(mycorpus,PlainTextDocument)
  
  
  #to convert to lowercase
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))
  
  #To print a summary of the document:                
  inspect(mycorpus)
  
  
  
  #-----------------------------------
  #Stage the Data Process:
  #-----------------------------------  
  
  #create a document term matrix
  dtm <- DocumentTermMatrix(mycorpus)
  mycorpus
  
  inspect(dtm[1,1:11])#View first 20 terms 
  
  #transpose of this matrix
  tdm <- TermDocumentMatrix(mycorpus)
  inspect(tdm[1:10,1])
  
  
  
  #-----------------------------------
  #Explore data:
  #-----------------------------------  
  
  #Organize terms by their frequency:
  freq <- colSums(as.matrix(dtm))
  length(freq)
  ord<- order(freq)
  
  #to export the matrix to Excel:
  m<- as.matrix(dtm)
  dim(m)
  filN <- paste("dtm",x,".csv")
  filN1 <-gsub(" ", "", filN, fixed = TRUE)
  write.csv(m,file=filN1)
  
  #Start by removing sparse terms
  dtms <- removeSparseTerms(dtm,0.1) #This makes a matrix that is 10% empty space,maximum
  inspect(dtms)
  
  
  #-----------------------------------
  #Word Frequency:
  #-----------------------------------    
  #check out some of the most and least frequently occurring words  
  freq[head(ord)]
  freq[tail(ord)]
  
  #Check out the frequency of frequencies
  head(table(freq),20)
  tail(table(freq),20)
  
  freq <- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
  head(freq,14)
  
  #An alternate view of term frequency: This will identify all terms that appear frequently (in this case, 50 or more times)
  
  freq.terms<- findFreqTerms(dtm,lowfreq=3)
  cat(c("\n\n***The frequents terms in document (", x , ") are:\n"))
  cat(freq.terms)
  cat("\n")
  #Change"50" to whatever is most appropriate for your text data
  
  wf <- data.frame(word=names(freq),freq=freq)
  head(wf)
  
  
  #-----------------------------------
  #Plot Word Frequencies :
  #-----------------------------------    
  #Plot words that appear at least 5 times  
  p<- ggplot(subset(wf,freq>5),aes(word,freq))
  p<- p+geom_bar(stat="identity")
  p<- p+theme(axis.text.x=element_text(angle=45,hjust=1))
  jpeg(Plot1Saved1)
  plot(p)
  dev.off()
  
  
  
  
  #-----------------------------------
  #Relationships Between Terms :
  #-----------------------------------  
  #Term Correlations:
  #If words always appear together, then correlation=1.0
  findAssocs(dtm, c("said" , "rumor"), corlimit=0.98)
  # specifying a correlation limit of 0.98 
  
  #
  findAssocs(dtms,"contrast",corlimit=0.90)
  #specifying a correlation limit of 0.95
  
  #-----------------------------------
  #Word Clouds! 
  #-----------------------------------    
  
  #Plot words that occur at least 25 times
  set.seed(142)
  jpeg(Plot3Saved1, width=12, height=8, units="in", res=500)
  wordcloud(names(freq),freq,min.freq=50)
  dev.off()
  
  #Add some color and plot words occurring at least 20 times
  set.seed(142)
  jpeg(Plot4Saved1, width=12, height=8, units="in", res=500)
  wordcloud(names(freq),freq,min.freq=40,scale=c(5,.1),colors=brewer.pal(6,"Dark2"))
  dev.off()
  
  
  
  #-----------------------------------
  #Clustering by Term Similarity
  #-----------------------------------  
  
  #first remove a lot of the uninteresting or infrequent words
  dtmss<-removeSparseTerms(dtm,0.15)#This makes a matrix that is only 15% empty space, maximum.
  inspect(dtmss)
  
  #Hierarchal Clustering
  #First calculate distance between words & then cluster them according to similarity
  plot.new()
  d<-dist(t(dtmss),method="euclidian")
  fit<-hclust(d=d,method="ward")
  jpeg(Plot2Saved1, width=30, height=15, units="in", res=800)
  plot(fit,hang= -1)
  dev.off()
  
  
  
  groups<-cutree(fit,k=5)#"k="defines the number of clusters you are using
  rect.hclust(fit,k=5,border="red")# draw dendogram with red borders around the 5 clusters
  
  
  
  
}#/end of for



#install the package to deal with text:
install.packages("tm")
install.packages("NLP")
install.packages("qdap")
install.packages("qdapDictionaries")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("Rgraphviz")
install.packages("SnowballC")
install.packages("biclust")
install.packages("cluster")
install.packages("igraph")
install.packages("fpc")
install.packages("Rcampdf", repos="http://datacube.wu.ac.at/",type="source")
install.packages("textmineR")

#install the package to deal with plots:
install.packages("ggplot2")
install.packages("wordcloud")


#to activate the libraries:
library("NLP")
library("tm")
library("ggplot2")
library("wordcloud")
library("qdap") # Quantitative discourse analysis of transcripts.
library("qdapDictionaries")
library("dplyr") # Data wrangling, pipe operator %>%().
library("RColorBrewer") # Generate palette of colours for plots.
library("scales") # Include commas in numbers.
library("Rgraphviz") # Correlation plots.
library("stringi")#to deal with Text
library("stringr")
library("stringdist")
library("SnowballC")
library("biclust")
library("cluster")
library("igraph")
library("fpc")
library("Rcampdf")
library("textmineR") #For textminR Package
library("slam")#To convert to A document term matrix of class dgCMatrix
library("Matrix")

#Create a corpus of documents to be processed.
#50 Exemplary News Articles from the Reuters-21578 Data Set of Topic
data("acq")
acq

#Inspect the VCorpus
inspect(acq)

#______________________________________________________________________________________
#______________________________________________________________________________________
#______________________________________________________________________________________
#To extract a document
Test1 <- acq[[1]]
Test1


#To know the size of the corps(The character length)
nchar(acq)

#-----------------------------------
#Cleaning Process
#----------------------------------- 


#Removing punctuation:
acq <- tm_map(acq, removePunctuation)
nchar(acq)
#Removing numbers:
acq <- tm_map(acq,removeNumbers)
nchar(acq)
#Removing "stopwords" (common words) that usually have no analytic value
acq <- tm_map(acq,removeWords,stopwords("english"))
nchar(acq)
#Removing particular words
acq<- tm_map(acq,removeWords,c("department","email"))
nchar(acq)
#Removing common word endings (e.g., "ing", "es", "s") 
acq<- tm_map(acq,stemDocument)
nchar(acq)
#Removing unnecesary whitespace from your documents: 
acq <- tm_map(acq,stripWhitespace)
nchar(acq)
#to convert to lowercase
acq <- tm_map(acq, content_transformer(tolower))
#Removing anything than English letters or Spaces from your documents: 
removeNunPunct <- gsub("[^[:alpha:][:space:]]*","",acq)
nchar(acq)
#To Finish tell R to treat the preprocessed documents (acq) as text documents and that will reduce the size as well
acq <- tm_map(acq,PlainTextDocument)
nchar(acq)

#Removing unnucessory words from the corpus:
acq <- tm_map(acq, removeWords, c("said","also", "article", "Article", 
                                  "download", "google", "figure",
                                  "fig", "groups","Google", "however",
                                  "high", "human", "levels",
                                  "larger", "may", "number",
                                  "shown", "study", "studies", "this",
                                  "using", "two", "the", "Scholar",
                                  "pubmedncbi", "PubMedNCBI",
                                  "view", "View", "the", "biol",
                                  "via", "image", "doi", "one", 
                                  "analysis","inc","will"))
#To print a summary of the document:                
inspect(acq)



#-----------------------------------
#Stage the Data Process:
#-----------------------------------  

#create a document term matrix
dtm <- DocumentTermMatrix(acq)
acq

inspect(dtm[1:6,1:5])#View first 20 terms 

#transpose of this matrix
tdm <- TermDocumentMatrix(acq)
inspect(tdm[1:6,1:5])






#-----------------------------------
#Explore data:
#-----------------------------------  

#Organize terms by their frequency:
freq <- colSums(as.matrix(dtm))
length(freq)
ord<- order(freq)

#to export the matrix to Excel:
m<- as.matrix(dtm)
dim(m)
write.csv(m,"dtm.csv")

#Start by removing sparse terms
dtms <- removeSparseTerms(dtm,0.1) #This makes a matrix that is 10% empty space,maximum
inspect(dtms)


#-----------------------------------
#Word Frequency:
#-----------------------------------    
#check out some of the most and least frequently occurring words  
freq[head(ord)]
freq[tail(ord)]

#Check out the frequency of frequencies
head(table(freq),20)
tail(table(freq),20)

freq <- sort(colSums(as.matrix(dtm)),decreasing=TRUE)
head(freq,14)

#An alternate view of term frequency: This will identify all terms that appear frequently (in this case, 50 or more times)

freq.terms<- findFreqTerms(dtm,lowfreq=3)
cat(freq.terms)

#Change"50" to whatever is most appropriate for your text data

wf <- data.frame(word=names(freq),freq=freq)
head(wf)

#-----------------------------------
#Plot Word Frequencies :
#-----------------------------------    
#Plot words that appear at least 5 times  
p<- ggplot(subset(wf,freq>30),aes(word,freq))
p<- p+geom_bar(stat="identity")
p<- p+theme(axis.text.x=element_text(angle=45,hjust=1))
plot(p)




#-----------------------------------
#Relationships Between Terms :
#-----------------------------------  
#Term Correlations:
#If words always appear together, then correlation=1.0
findAssocs(dtm, c("said" , "rumor"), corlimit=0.98)
# specifying a correlation limit of 0.98 
findAssocs(dtms,"contrast",corlimit=0.90)


#-----------------------------------
#Word Clouds! 
#-----------------------------------    

#Plot words that occur at least 25 times
set.seed(142)
wordcloud(names(freq),freq,min.freq=10)


#Add some color and plot words occurring at least 20 times
set.seed(142)
wordcloud(names(freq),freq,min.freq=10,scale=c(5,.1),colors=brewer.pal(6,"Dark2"))



#-----------------------------------
#Clustering by Term Similarity
#-----------------------------------  

#first remove a lot of the uninteresting or infrequent words
dtmss<-removeSparseTerms(dtm,0.1)#This makes a matrix that is only 15% empty space, maximum.
inspect(dtmss)

#Hierarchal Clustering
#First calculate distance between words & then cluster them according to similarity
plot.new()
d<-dist(t(dtmss),method="euclidian")
fit<-hclust(d=d,method="ward")
plot(fit,hang= -1)


groups<-cutree(fit,k=5)#"k="defines the number of clusters you are using
rect.hclust(fit,k=5,border="red")# draw dendogram with red borders around the 5 clusters


install.packages("tm")

#getwd()

#50 Exemplary News Articles from the Reuters-21578 Data Set of Topic
data("acq")
acq

#Inspect the VCorpus
inspect(acq)

#obtain vetor/matrix of corpuses
texts <- sapply(acq$content, "[[", "content")

################################################################
#Implementing merge sort for obtaining top 10 longest documents
################################################################

mmerge<-function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && nchar(a[ai])<nchar(b[bi])) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1          
    }
  }
  r
}
mmergesort<-function(A) {
  if(length(A)>1) {
    q <- ceiling(length(A)/2)
    a <- mmergesort(A[1:q])
    b <- mmergesort(A[(q+1):length(A)])
    mmerge(a,b)
  } else {
    A
  }
}

#calling merge sort
a<-mmergesort(texts)
nchar(a)

#Arranging documents in decreasing order
topten <- a[50:41]
temp<-topten

#top ten longest document set
nchar(temp)
####################################################
#Remove all \n from documents
##################################################

temp<-str_replace_all(temp, "[\r\n]" , " ")

#########################################
#find longest word in each document
#########################################
for(x in 1:10)
{
  shadvector <- unlist(strsplit(temp[x], split=" "))
  shadvlength <- lapply(shadvector,nchar) #applying nchar function to the document
  shadmaxind <- which.max(shadvlength) ## Maximum element
  shadmax <- nchar(shadvector[shadmaxind])
  cat("Length of longest word in document ",shadmax,"\n") #prints length of longest word
  cat("Longest word is",shadvector[shadmaxind],"\n") #prints longest word
}

help(lapply)

###########################################
#to find nos of sentences
##########################################
for(x in 1:10)
{
  lengsent<-length(gregexpr('[[:alnum:] ][.!?]', temp)[[x]])
  cat("Number of sentences in document",x,"is",lengsent,"\n")
}

#################################################
#finding longest sentences in top ten documents
#################################################

for(y in 1:10)
{
  sentencesVector<-unlist(strsplit(temp[y],"\\.!?"))
  sentvlength <- lapply(sentencesVector,nchar)
  sentmaxind <- which.max(sentvlength) ## Maximum element
  sentmax <- nchar(sentencesVector[sentmaxind])
  cat("Position of longest sentence is : ",sentmaxind,"\n") # prints the position of sentence in document
  cat("Length of longest sentence is :",sentmax,"\n") #prints length of longest word
  cat("Longest sentence in document", y, "is:",sentencesVector[sentmaxind],"\n")  #prints the longest sentence
  removePunctuation(sentencesVector[sentmaxind], preserve_intra_word_dashes = FALSE)
  cat(sentencesVector[sentmaxind])
}


###############################################
#printing length of each sentence
###############################################

for(c in 1:10)
{
  sentencesVector<-unlist(strsplit(temp[c],"\\.!?"))
  sentvlength <- lapply(sentencesVector,nchar)
  cat("Number of sentences in document ",c, "is", length(sentencesVector),"\n")
  
  for(d in 1:length(sentencesVector))
  {
    sentlength<-str(sentvlength[d])
    cat("is length of sentence ",d,sentlength,"\n")
  }
}

#####################################################
#Removing punctuations in each sentence
#####################################################

for(y in 1:10)
{
  sentencesVector<-unlist(strsplit(temp[y],"\\.!?"))
  for(d in 1:length(sentencesVector))
  {
    cat("Sentence before removing punctuations: ",sentencesVector[d],"\n")
    setsent<-removePunctuation(sentencesVector[d], preserve_intra_word_dashes = FALSE)
    cat("Sentence after removing punctuations: ",setsent,"\n")
  }
}


#####################################################


### convert the Materix to Vecotr 

# gdata package

ve<-unmatrix(m, byrow=FALSE)
?unmatrix
is.vector(ve)

##### languageR package

your.spc <- text2spc.fnc(ve)
class(your.spc)


# zipfR package 

####### use spc to represent a word frequency spectrum
#smaple size 
N(your.spc) 
# number of types of its vocabulary
V(your.spc) 
#spectrumelementm,number of types inthesample with frequency m
Vm(your.spc, 3) 

?spc
My.spc<-spc(Vm=your.spc$Vm, m=your.spc$m)
My.spc

#####

my.lnre<- lnre("gigp", My.spc)

VV(my.lnre, N=N(my.lnre))

VVm(my.lnre,3, N=N(my.lnre))


######
?lnre.spc

lnre.spc(my.lnre, N(your.spc), variances=FALSE, m.max=100)

####  plot spc
plot(My.spc)
####
My.spc.interp <-spc.interp(My.spc, N(My.spc), 
                           m.max=max(My.spc$m), allow.extrapolation=TRUE)
My.spc.interp
plot(My.spc.interp)

######## 
str(dtm)
M1 <- as(m, "dgCMatrix")
class(M1)
M1
model <- FitLdaModel(M1 , k = 5, iterations = 200)
model
class(model)
write.csv(model, "FitLdaModel_model.csv")


###### part of speech



if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh(c(
  "trinker/termco", 
  "trinker/tagger",
  "trinker/textshape"
))
library(openNLP)

for(r in 1:10)
{
  shadvector <- unlist(strsplit(temp[r], split=" "))
  
  for(j in 1:length(shadvector))
  {    
    tagged <- tagPOS(tokenize_sentences(sentencesVector[j]))
    cat("Part of speech ",tagged$POStagged,"\n") #prints length oftagged
  }
}

