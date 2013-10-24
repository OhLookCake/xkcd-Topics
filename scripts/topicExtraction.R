
require(tm)
require(topicmodels)


dfTranscripts <- read.table("data/transcripts.csv",header=F,sep="\t",colClasses=c("character","character"),col.names=c("url","text"),quote="")

dfTranscripts$comicnumber <- sapply(dfTranscripts$url, function(x) strsplit(x,"/")[[1]][4])


#### Text Cleaning ####

#Remove alt-text (optional)
dfTranscripts$text <- gsub("\\{\\{.*\\}\\}","",dfTranscripts$text)

#Remove scene-description.
#This might initially seem like a bad idea, but scne descriptions contain stuff like [[Man standing in a room]] ,etc. 
#I'll revisit this, to see if there's a better solution
dfTranscripts$text <- gsub("\\[\\[.*?\\]\\]","",dfTranscripts$text)


#remove speaker id
#for each pipe-surrounded string, check if there is a : there. If there is, discard the part before the first :
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

source('scripts/extendedStopwords.R')
dtm.control <- list(
	tolower 			= T,
	removePunctuation 	= T,
	removeNumbers 		= T,
	stopwords 			= c(stopwords("english"),extendedstopwords),
	stemming 			= T,
	wordLengths 		= c(3,Inf),
	weighting 			= weightTf
)

dtm <- DocumentTermMatrix(Corpus(VectorSource(dfTranscripts$text)),
						  control = dtm.control)
dim(dtm)
dtm <- removeSparseTerms(dtm,0.999)
dim(dtm)

# Drop documents with little or no text (left)
dtm <- dtm[rowSums(as.matrix(dtm))>0,]


#### Topic Modeling - LDA ####
set.seed(51)
trainpoints <- sample(1:nrow(dtm),1.0*nrow(dtm),replace=F) # to train on a subsample, change 1.0 to a lower value, say 0.8


SpecificTerms <- function(lda.model,n=1) {
	p <- posterior(lda.model)$terms
	n <- min(n,ncol(p))
	cumulativep <- colSums(p)
	specificp <- t(apply(p,1,function(x) ((x) + (x/cumulativep) )))
	
	topterms <- t(apply(specificp, 1, function(x)
				(colnames(specificp)[order(x,decreasing=T)[1:n]]) ))
	
	topterms
}

k <- 4
lda.model <- LDA(dtm[trainpoints,], k)

t(SpecificTerms(lda.model,10))
terms(lda.model,10)

save(lda.model,file="results/lda.model")


#### Examining Results ####
require(reshape2)
require(ggplot2)
require(RColorBrewer)

lda.topics <- topics(lda.model,1)
termgenerator <- posterior(lda.model)$terms

###1: relative probabilities of words in each topic ###
termimportance <- apply(termgenerator,1,
						function(x)	x[order(x,decreasing=T)[1:100]])
termimportance.longform <- melt(termimportance,
								value.name="probability",
								varnames=c("termnumber","topic"))

ggplot(data=termimportance.longform,
	   aes(
		x=termnumber,
		y=probability,
		color=factor(topic),
		group=topic)) + 
	geom_line()


###2: Individual comics' distribution over topics ###
comics.topics <- posterior(lda.model,dtm[trainpoints,])$topics
df.comics.topics <- as.data.frame(comics.topics)
df.comics.topics <- cbind(comic=as.character(rownames(df.comics.topics)),
						  df.comics.topics, stringsAsFactors=F)

df.comics.topics.longform <- melt(df.comics.topics,
								  id.vars="comic",variable.name="topic")
df.comics.topics.longform <- df.comics.topics.longform[order(as.numeric(df.comics.topics.longform$comic)),]

topic.names <- terms(lda.model,3)
topic.names <- apply(topic.names, 2, paste, collapse=",")
names(topic.names) <- NULL

n.comics.to.plot <- 30
start.comic <- 600

four.colour.palette <- brewer.pal(10,"Paired")[c(1,3,5,7)]

bplot <- ggplot(df.comics.topics.longform[(start.comic*k)+1:(n.comics.to.plot*k),],
				aes(x=comic,y=value)) + 
	geom_bar(stat="identity",position="stack",aes(fill=topic))

bplot + 
	scale_fill_manual(values=four.colour.palette,
					  name="Topic", breaks=1:k,labels=topic.names) +
	coord_flip()


###3: Look at a specific comic
look.for.comic <- "36" #say
rowids <- which(df.comics.topics.longform$comic==look.for.comic)
if(length(rowids) > 0){
	ggplot(df.comics.topics.longform[rowids,],
		   aes(x=topic, y=value)) + 
		geom_bar(stat="identity", aes(fill=topic)) +
		scale_fill_manual(values=four.colour.palette,
						  name="Topic",
						  breaks=1:k,
						  labels=topic.names)
	
} else {
	cat(paste("Comic",look.for.comic,"did not have enough text to be assigned topics"))
}


	




