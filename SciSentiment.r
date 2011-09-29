library("twitteR")
library("ggplot2")
library("stringr")
##implement Jefrey's scoring algorithm as a function
#' 
#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param pos.words vector of words of postive sentiment
#' @param neg.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores
#' @author Jefrey Breen <jbreen@cambridge.aero>
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
	require(stringr)
	
	# we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
	# we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
	scores = laply(sentences, function(sentence, pos.words, neg.words) {
		
		# clean up sentences with R's regex-driven global substitute, gsub():
		sentence = gsub('[[:punct:]]', '', sentence)
		sentence = gsub('[[:cntrl:]]', '', sentence)
		sentence = gsub('\\d+', '', sentence)
		# and convert to lower case:
    sentence = iconv(sentence, 'UTF-8', 'ASCII')
		sentence = tolower(sentence)

		# split into words. str_split is in the stringr package
		word.list = str_split(sentence, '\\s+')
		# sometimes a list() is one level of hierarchy too much
		words = unlist(word.list)

		# compare our words to the dictionaries of positive & negative terms
		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)
	
		# match() returns the position of the matched term or NA
		# we just want a TRUE/FALSE:
		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)

		# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		score = sum(pos.matches) - sum(neg.matches)

		return(score)
	}, pos.words, neg.words, .progress=.progress )

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}

##Grab the past week's tweets, or 1500, whichever is less
## using "mendeley" gives fewer results, but I haven't examined why, "#mendeley" gives much fewer
tweets = searchTwitter("mendeley", n=1500) 
tweets_df = ldply(tweets, function(t) t$toDataFrame() )

##get previously scored tweets TODO
##attreport_old<-read.csv("C:\\Users\\William Gunn\\Desktop\\My Dropbox\\Scripting\\Data\\webinars\\attreport.csv", header = TRUE, sep=",", stringsAsFactors = FALSE, blank.lines.skip = TRUE)
##t<-try(rbind(attreport_old, attreport_new))
##if(inherits(t, "try-error")) stop("Make sure the number of columns are equal. Add NAs to a column if you need.") else attreport<-rbind(attreport_old, attreport_new)
##rm(t)
## remove previously scored tweets from fetched tweets TODO
## come code that finds old tweets in tweets_df and deletes them

## load the word lists from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
poswords = scan('/home/williamgunn/Dropbox/Scripting/Data/positive-words.txt', what = 'character', comment.char = ';')
negwords = scan('/home/williamgunn/Dropbox/Scripting/Data/negative-words.txt', what = 'character', comment.char = ';')

## add a few specific to software
negwords = c(negwords, 'wtf', 'fail', 'crash', 'uninstall', 'uninstalling')

## remove paper tweets
source_filtered_df<-subset(tweets_df, tweets_df$statusSource !="&lt;a href=&quot;http://www.mendeley.com&quot; rel=&quot;nofollow&quot;&gt;Mendeley&lt;/a&gt;")
##score tweets and add score column to dataframe

sentiment_score = score.sentiment(source_filtered_df$text, poswords, negwords, .progress = 'text')
tweet_sentiment<-merge(source_filtered_df, sentiment_score, by = "text")

## add previously scored tweets to newly fetched ones TODO
## rbind (old_tweets, tweet_sentiment)

## filter noise
pos_sentiment<-subset(tweet_sentiment, tweet_sentiment$score >=2)
neg_sentiment<-subset(tweet_sentiment, tweet_sentiment$score <=-2)
strong_sentiment<-rbind(droplevels(pos_sentiment), droplevels(neg_sentiment))

## CHARTS & PLOTS
## code sentiment values, sort by date & plot
strong_sentiment$category<-cut(strong_sentiment$score, breaks = c(-20, 0, 20), labels = c("negative","positive"))
strong_sentiment<-strong_sentiment[order(strong_sentiment$created),]
sentiment_timeline<-qplot(strong_sentiment$created, strong_sentiment$score, geom = "area", main = "Researcher Sentiment", xlab = NULL, ylab = "Sentiment Score")
print(sentiment_timeline)
## trying to color above and below the axis with something like: (..., fill = strong_sentiment$category) + scale_color_manual(values = c("2" = "green", "-2" = "red")) but it's not working as desired

##sentiment distribution
sentiment_distribution<-qplot(strong_sentiment$score, fill = strong_sentiment$category) + scale_fill_manual(values = c("positive" = "green", "negative" = "red"))
print(sentiment_distribution)

## pie
slice_df<-data.frame(table(strong_sentiment$category, exclude = "", useNA="no"))
# remove null, na, or empty values and values too small to display
bigslices_df<-subset(slice_df, slice_df$Freq>=1)
# make slice annotations
pct<-round(bigslices_df$Freq/sum(bigslices_df$Freq)*100)
lbls<-paste(bigslices_df$Var1, pct)
lbls<-paste(lbls, "%",sep="")
## mmmmm....pie
print(pie(bigslices_df$Freq, labels = lbls, main="Sentiment", col=c("negative" = "red", "positive" = "green")))

## write scored tweets out to file for loading and de-duping next run
## write.csv(attreport, file = "C:\\Users\\William Gunn\\Desktop\\My Dropbox\\Scripting\\Data\\webinars\\attreport.csv")