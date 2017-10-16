
#-----------------------Network analysis of Tweets data------------------#
#Useful to understand how many connections a specific word has with other words. 

# Using network terminology, 
#Our keywords are the 'nodes' in a network, which are called vertices.
#Connections are named edges. 

#install.packages("igraph")
library(igraph)

#Reading "tweets" about "Obama" and Clean the document
setwd("E:\\Text mining\\codes\\network analysis\\")

#Cleaning of the data in "Tm Package with R" session
source("cleaning_data.R")
##############################################################
getwd()
#Read the twitter data

obama1<-read.csv("obama1.csv",h=FALSE)
obama2<-read.csv("obama2.csv",h=FALSE)
obama3<-read.csv("obama3.csv",h=FALSE)
obama4<-read.csv("obama4.csv",h=FALSE)

obama1<-read.csv("nytimes.csv",h=FALSE)

View(obama1)

#Row bind all the files

obama<-rbind(obama1)
str(obama)
names(obama)
View(obama)

#Converting to a dataframe ; only second column. The V2 Column
tweets<-data.frame(obama$V3)

str(tweets)
names(tweets)
View(tweets)

#Renaming the column
names(tweets)<-"Tweet_Text"

#Lets look at the Structure of tweets?
str(tweets)

#Data Pre-processing using tm package. The Text Mining Package
#if not installed install tm package
install.packages("tm")
library(tm)

# VectorSource(x) x -- A vector giving the texts.
#Building a Text Corpus
#Source for the corpus

tweets.corpus<-Corpus(VectorSource(tweets$Tweet_Text))
summary(tweets.corpus)
inspect(tweets.corpus[1:100]) #Inspecting First 5 "text documents" elements in Corpus

#Cleaning the data
#Removing stop words
#trying to remove the http which refer to the url's
#You can also try and remove frequent words like "Obama","Barack" that occur in the document


inspect(tweets.corpus[1:50])
#Data Transformations as R is Case Sensitive
#The below are the steps of Cleansing data
tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
head(stopwords("english"),10) #Examples of stop words
my_stopwords<-c(stopwords('english'),'http*') #Can add more words apart from standard list
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)


#Building term document matrix
tweets.tdm<-TermDocumentMatrix(tweets.corpus)
tweets.tdm
dim(tweets.tdm) #Dimensions of term document matrix
inspect(tweets.tdm[1:10,1:10]) #Inspecting the term document matrix

#Removing sparse terms
#Words that occur infrequenctly 
#This function call removes those terms which have 
#at least a 97 percentage of sparse (i.e., terms occurring 0 times in a document) elements
tweets.imp<-removeSparseTerms(tweets.tdm,0.97)
tweets.imp
inspect(tweets.imp[1:10,1:10])
#############################################################################################
# Convert corpus to tdm
obama.tdm <- TermDocumentMatrix(tweets.corpus, 
                                control=list(wordLengths=c(1, Inf)))

#word length more than 1 is given
#obama.tdm [1:10,1:10]  							  

# Remove sparse terms
obama.tdm.rm <- removeSparseTerms(obama.tdm, sparse=0.99)

#here TDM is
obama.tdm.rm[1:20,1:20]

inspect(obama.tdm.rm[1:20,1:20])
#Transform TDM transform into a matrix. 
obama.m <- as.matrix(obama.tdm.rm)
obama.m[1:20,1:20]

#Transform the matrix into a Boolean matrix 
#Contains 1/0.It indicates 1 for existing values other than zero.

#Therefore, we are just modifying the matrix to indicate yes or no for the terms existing
#in a document.

#Convert to boolean matrix
obama.m[obama.m>=1] <- 1
obama.m[1:20,1:20]
###################################################################################################################################################
#Adjacency matrix : 
#It will show how many 'connections' each term has.
#This will require the product of two matrices, using the '%*%' matrix operator. 
#Through the inner product of the terms, 
#we will arrive at the number of times each term appears together in a document.
################################################################################################################################################### 
#------------------------------------------------------Adjacency matrix---------------------------------------------------------------------------#
#%*% is product of 2 matrices

#Build a Term Adjacency matrix
obama.m2 <- obama.m %*% t(obama.m)
obama.m2[1:20,1:20]
#How many times each word has occured with the other words

#Let's choose a subset of obama.m2
obama.test<-obama.m2[1:20,1:20]
View(obama.test)
#--------------------------------------------Build an adjacency graph ---------------------------------------------------------------------------#

#w="TRUE"
#d="undirected"

#weighted = "TRUE", mode="undirected"
#weighted = NULL, mode="undirected"
#weighted = "TRUE", mode="directed"
obama.graph <- graph.adjacency(obama.test,weighted="TRUE",mode="directed")

class(obama.graph)
#Vertices/nodes
V(obama.graph)
#List of onnections/edges
E(obama.graph)
get.edgelist(obama.graph)

#Checking if weights exist 
E(obama.graph)$weight

#Number of Connections for each person
degree(obama.graph,loops = TRUE)
degree(obama.graph,loops = FALSE)

#Summary of weights
summary_weights<-cbind(get.edgelist(obama.graph),round(E(obama.graph)$weight,3))

#Final choice
obama.graph <- graph.adjacency(obama.test,weighted="TRUE",mode="undirected")

#Set the layout of the graph
version
install.packages("layout.fruchterman.reingold")
?layout
layout1 <- layout.fruchterman.reingold(obama.graph,dim=2)

plot(obama.graph,
     layout=layout1,
     vertex.size=10, 
     vertex.label.color="darkred")

#-----------------------------Now for the a bigger set of words-------------
obama.m3<-obama.m2[1:20,1:20]
obama.g <- graph.adjacency(obama.m3,weighted="TRUE",mode="undirected")
obama.g

#Remove Loops
obama.g<-simplify(obama.g)
#Vertices
V(obama.g)

E(obama.g)
#Number of edges  & vertices
ecount(obama.g)
vcount(obama.g)

#List of connections
E(obama.g)$weight
E(obama.g)$weight <- runif(ecount(obama.g))

# Number of Connections for each person
degree(obama.g,mode="out",loops = TRUE)

# set labels and degrees of vertices
V(obama.g)$label <- V(obama.g)$name
V(obama.g)$degree <- degree(obama.g)

# plot layout fruchterman.reingold
layout1 <- layout.fruchterman.reingold(obama.g,dim=2)

plot(obama.g,layout=layout1, vertex.size=2, vertex.label.color="darkred")

plot(obama.g,layout=layout.kamada.kawai)

#Adding more modifications

V(obama.g)$label.cex <- 2 * V(obama.g)$degree/ max(V(obama.g)$degree)
V(obama.g)$label.color <- rgb(0, 0, .2, .8)
V(obama.g)$label.color <- "blue"
V(obama.g)$frame.color <- NA

#Only if weights exist
edge_weight <-  (-log(E(obama.g)$weight)) / max(-log(E(obama.g)$weight))

E(obama.g)$color <- rgb(0.3,0.3, 0, edge_weight)
E(obama.g)$width <- edge_weight

#E(obama.g)$color<-"grey"

# plot the graph in layout1
plot(obama.g, layout=layout1,
     vertex.color="red")
#------------------------------------------------------spinglass.community ---------------------------------------------------------------------------#

#This function tries to find communities/groups in the netowork via a spin-glass model 
#A community is a set of nodes with many edges inside the community and 
#few edges between outside it 
#Spins : Maximum number of groups that it can find
spc <- spinglass.community(obama.g,spins=10)
spc

#Membership:
#Gives the division of the vertices, into communities.
#It returns a numeric vector,one value for each vertex
#,the id of its community

plot(obama.g, layout=layout1, vertex.size=.3, 
     vertex.label.cex=1.5, 
     edge.color=rgb(.4,.4,0,.3),
     vertex.color=spc$membership+0.5, 
     vertex.label.color=spc$membership+0.5, 
     asp=FALSE)

#------------------------------------------------------3D PLOTS ---------------------------------------------------------------------------#

V(obama.g)$label <- V(obama.g)$name
V(obama.g)$degree <- degree(obama.g)

library(rgl)
coords <- layout.kamada.kawai(obama.g, dim=3)
open3d()
rglplot(obama.g, vertex.size=10,edge.arrow.size=0.6, 
        layout=coords,
        vertex.label.dist=0.5, 
        vertex.color="blue",
        edge.color="green")

#------------------------------------------------------Interactive plots ---------------------------------------------------------------------------#

tkplot(obama.g, layout=layout.kamada.kawai, vertex.size=10,edge.arrow.size=0.6,vertex.label.dist=0.5, vertex.color="blue",edge.color="green")

