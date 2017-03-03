library(syuzhet)
sentiment<-sapply(listings[[5]], get_sentiment, method="afinn")
interest<-factor(unlist(listings$interest_level), levels=c("low", "medium", "high"))
levels(interest)<-c(1,2,3)
interest<-as.numeric(as.character(interest))
qd.sent<-lm(interest ~ sentiment)
summary(qd.sent)
#bing R2: .002, nrc: .0007, syuzhet:.001, afinn: .004
#Straight-up univariate standard sentiment won't work

desc<-unlist(listings$description)
desc2<-tolower(unlist(strsplit(desc, split=" ")))
freq<-data.frame(table(desc2))
freq2<-freq[order(freq, decreasing=T),]
colnames(freq2)[1]<-"word"

setwd("C:/Users/David/Desktop/Corpora/brown_corpus_untagged")
brown<-lapply(dir(), scan, what="character")
brown<-unlist(brown)
brown.table<-data.frame(table(brown))
brown.freq<-brown.table[order(brown.table[,2], decreasing=T),]
colnames(brown.freq)[1]<-"word"

#Keyness function
keyness<-function(test,reference){
  test.total<-sum(test)
  reference.total<-sum(reference)
  expected1<-test.total * (test + reference)/(test.total+reference.total)
  expected2<-reference.total*(test + reference)/(test.total+reference.total)
  
  return(2*((test*log(test/expected1)) + (reference * log(reference/expected2))))
}

#Construct keyness matrix and inspect for high-keyness sentiment words
library(dplyr)
desc.brown<-inner_join(freq2, brown.freq, by="word", copy=T)
desc.brown[,2:3]<-sapply(desc.brown[,2:3], as.numeric)
desc.brown2<-desc.brown %>% mutate(Keyness = keyness(desc.brown$Freq.x, desc.brown$Freq.y))
desc.brown3<-filter(desc.brown2, Freq.x>Freq.y)

#Sentiment: brand? great, beautiful*, amazing, gorgeous, stunning,  perfect, enjoy, best, bright, sun*ny, !, 
#fantastic, spectacular, unique, charming, nice, elegant, lovely, incredible, excellent, fabulous, classic, better, vibrant, awesome, wonderful
#finest, sleek, happy, desirable
#Size: spacious/space, full/fully, lots, tons, huge, plenty, ample, oversized, massive, grand, abundan*t, generous, soaring
#Location: real? central/center?  heart? convenient*?, easily? overlooking? historic? helpful, rare, nearby, luxur*y, exclusive, prime,

pos.sent<-c("great", "beautiful", "beautifully", "amazing", "gorgeous", "stunning", "perfect", "enjoy", "best", "bright", "sun", "sunlight", "sunny", 
            "fantastic", "spectacular", "unique", "charming", "nice", "elegant", "lovely", "incredible", "excellent", "fabulous", "classic", "better", "vibrant", "awesome", "wonderful",
            "finest", "sleek", "happy", "desirable")
space.sent<-c("spacious", "space", "full", "fully", "lots","tons", "huge", "plenty", "ample", "oversized", "massive", "grand", "abundant", "abundance", "generous", "soaring")

mat.sent<-c("luxury", "luxury", "luxurious", "center", "central", "heart", "convenient", "conveniently", "easily", "overlooking", "historic", "helpful", "rare", "nearby", "exclusive", "prime")

description<-lapply(lapply(listings$description, tolower), strsplit, split=" ")
desc.vec<-lapply(description, unlist)
space.loc<-sapply(desc.vec, match, space.sent)
space.count<-rep(0, length(listings))
for (i in 1:length(space.loc)){
  space.count[i]<-sum(!is.na(space.loc[[i]]))
}
pos.loc<-sapply(desc.vec, match, pos.sent)
pos.count<-rep(0, length(listings))
for (i in 1:length(pos.loc)){
  pos.count[i]<-sum(!is.na(pos.loc[[i]]))
}
mat.loc<-sapply(desc.vec, match, mat.sent)
mat.count<-rep(0, length(listings))
for (i in 1:length(mat.loc)){
  mat.count[i]<-sum(!is.na(mat.loc[[i]]))
}
#rm(desc2, description, space.loc)
#rm(mat.loc, pos.loc)
characters<-sapply(listings$description, nchar)

sentiment<-data.frame(unlist(listings$listing_id), unlist(listings$interest_level), 500*pos.count/characters, 500*space.count/characters, 500*mat.count/characters)
colnames(sentiment)<-c("listing_id", "interest_level", "Positive.Affect", "Space.Affect", "About.Town.Affect")

sentiment.raw<-data.frame(unlist(listings$listing_id), unlist(listings$interest_level), pos.count, space.count, mat.count)
colnames(sentiment.raw)<-c("listing_id", "interest_level", "Positive.Affect", "Space.Affect", "About.Town.Affect")

raw.lm<-lm(interest ~ Space.Affect, data=sentiment.raw)
#.7% on saturated, .5% on space-only
sent.lm<-lm(interest ~ Space.Affect, data=sentiment)
#Again, only .6% R2 on saturated, .3% on space-only (which is best)
