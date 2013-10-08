

setwd("C:/etc/Projects/Data/_Ongoing/xkcd Topics")

require(tm)
require(topicmodels)


dfTranscripts <- read.table("data/transcripts.csv",header=F,sep="\t",colClasses=c("character","character"),col.names=c("url","text"),quote="")

dfTranscripts$comicnumber <- sapply(dfTranscripts$url, function(x) strsplit(x,"/")[[1]][4])


#### Text Cleaning ####

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
	processed.frames <- sapply(frames[grep("\\:",frames)], 
							   function(f) {
							   	dialogue<-strsplit(f,":")[[1]]
							   	do.call(paste,as.list(c(
							   		dialogue[2:length(dialogue)],sep="\\:")
							   	))
							   })
	frames[grep("\\:",frames)] <- processed.frames
	do.call(paste,as.list(c(frames,sep="|")))
}

dfTranscripts$text <- sapply(dfTranscripts$text,RemoveSpeakers)


#### Create dtm ####

dtm.control <- list(
	tolower 			= T,
	removePunctuation 	= T,
	removeNumbers 		= T,
	stopwords 			= c(stopwords("english"),
					 extendedstopwords),
	stemming 			= T,
	wordLengths 		= c(3,Inf)
)

dtm <- DocumentTermMatrix(Corpus(VectorSource(dfTranscripts$text)),
						  control = dtm.control)


# Drop documents with little or no text (left)
dtm <- dtm[rowSums(as.matrix(dtm))>3,]


#### Topic Modeling - LDA ####
set.seed(1024)
trainpoints <- sample(1:nrow(dtm),0.8*nrow(dtm),replace=F)

k <- 4
lda <- LDA(dtm[trainpoints,], k)
terms(lda,10)


#### Examining Results ####

ldatopics<-topics(lda)
names(ldatopics)<-1:length(ldatopics)
hist(ldatopics,breaks=c(0,1:k),labels=terms(lda))


termgenerator <- posterior(lda)$terms
y<-apply(termgenerator,1,function(x) x[order(x,decreasing=T)[1:100]])
plot(1:100,y[,1],type="l",col=1,ylim=c(min(y),max(y)))
lines(1:100,y[,2],type="l",col=2)
lines(1:100,y[,3],type="l",col=3)
lines(1:100,y[,4],type="l",col=4)


