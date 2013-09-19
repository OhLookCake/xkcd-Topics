

setwd("C:/etc/Projects/Data/_Ongoing/xkcd Topics")

require(RTextTools)
require(topicmodels)


dfTranscripts <- read.table("data/transcriptsEdit.csv",header=F,sep="\t",colClasses=c("character","character"),col.names=c("url","text"),quote="")

dfTranscripts$comicnumber <- sapply(dfTranscripts$url, function(x) strsplit(x,"/")[[1]][4])



########this is where we retain only the 'text' part

###Text Cleaning

#Remove alt-text
dfTranscripts$text <- sub("\\{\\{.*\\}\\}","",dfTranscripts$text)

#Remove scene-description.
#This might initially seem like a bad idea, but scne descriptions contain stuff like [[Man standing in a room]] ,etc. 
#I'll revisit this, to see if there's a better solution
dfTranscripts$text <- sub("\\[\\[.*\\]\\]","",dfTranscripts$text)


#remove speaker id
#for each pipe-surrounded string, check if there is a : there. If there is, discard the part before the :
#this is only moderately accurate, but it's the best way I could think of, without manual intervention

RemoveSpeakers <- function(trans){
	trans <- paste0("|",trans,"|")
	frames <- strsplit(trans,"\\|")[[1]]
	processed.frames <- sapply(frames[grep("\\:",frames)], function(f) {
																dialogue<-strsplit(f,":")[[1]]
																do.call(paste,as.list(c(
																	dialogue[2:length(dialogue)],sep="\\:")
																					  ))
															})
	frames[grep("\\:",frames)] <- processed.frames
	do.call(paste,as.list(c(frames,sep="|")))
}

dfTranscripts$text <- sapply(dfTranscripts$text,RemoveSpeakers)


##### dtm

dtm <- create_matrix(dfTranscripts$text,language="english",
					 stripWhitespace=T, toLower=T,stemWords=T,removeStopwords=T,
					 removeNumbers=TRUE, removePunctuation=T,removeSparseTerms=1-(2/nrow(dfTranscripts))
)
#					 weighting=weightTfIdf)

# Drop documents with no text (left)
dtm <- dtm[rowSums(as.matrix(dtm))>0,]


#### LDA
set.seed(11)
trainpoints <- sample(1:nrow(dtm),0.8*nrow(dtm),replace=F)

k <- 10
lda <- LDA(dtm[trainpoints,], k)

terms(lda,3)

t<-topics(lda)
names(t)<-1:length(t)
hist(t,breaks=c(0,1:k),labels=terms(lda))









