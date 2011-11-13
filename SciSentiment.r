library('ggplot2')
library('stringr')
require('rplos')
##implement Jefrey's scoring algorithm as a function
#' 
#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param sure.words vector of words of postive sentiment
#' @param unsure.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores
#' @author Jefrey Breen <jbreen@cambridge.aero>
score.sentiment = function(sentences, surewords, unsurewords, .progress='none')
{
  require(plyr)
	require(stringr)
	
	# we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
	# we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
	scores = laply(sentences, function(sentence, sure.words, unsure.words) {
		
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
		sure.matches = match(words, sure.words)
		unsure.matches = match(words, unsure.words)
	
		# match() returns the position of the matched term or NA
		# we just want a TRUE/FALSE:
		sure.matches = !is.na(sure.matches)
		unsure.matches = !is.na(unsure.matches)

		# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		score = sum(sure.matches) - sum(unsure.matches)

		return(score)
	}, sure.words, unsure.words, .progress=.progress )

	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}


##Get number of papers with our terms
## Legacy stuff
## using "mendeley" gives fewer results, but I haven't examined why, "#mendeley" gives much fewer
## tweets = searchTwitter("mendeley", n=1500) 
## tweets_df = ldply(tweets, function(t) t$toDataFrame() )

# load the word lists
surewords = scan('/home/william/SciSentiment/sure-words.txt', what = 'character', comment.char = ';')
unsurewords = scan('/home/william/SciSentiment/unsure-words.txt', what = 'character', comment.char = ';')

## use this to interactively add words
# surewords = c(surewords, '[add words here')
# get word occurrence summary
surecount<-plosword(surewords, vis = 'TRUE')
unsurecount<-plosword(unsurewords, vis = 'TRUE')

surecount_df<-surecount$table
unsurecount_df<-unsurecount$table

# get everything for a given term (can only pass terms one at a time)

geteverything<-function(surewords){
  out<-searchplos(surewords, 'id, title, subject, pagecount, publication_date, abstract, author, article_type, figure_table_caption, introduction, results_and_discussion, materials_and_methods, supporting_information, body,', 5)
}

makedf_everything<-function(sureword_everything_list){
  out<-data.frame(sureword_everything_list)
}

#countterms<-function(sureword_everything_df){
#  out<-length(grep(surewords, sureword_everything_df$body))
#}

sureword_everything_list<-llply(surewords, geteverything, .progress = 'text')
sureword_everything_df<-ldply(sureword_everything_list, makedf, .progress = 'text')

## remove stuff
# source_filtered_df<-subset(tweets_df, tweets_df$statusSource !="&lt;a href=&quot;http://www.mendeley.com&quot; rel=&quot;nofollow&quot;&gt;Mendeley&lt;/a&gt;")
sureword_everything_df[,1]<-NULL
sureword_everything_df$pagecount<-strtrim(sureword_everything_df$pagecount, 3)
sureword_everything_df$id<-strtrim(sureword_everything_df$id, 28)
sureword_everything_df$publication_date<-strtrim(sureword_everything_df$publication_date, 10)

# convert numbers to numbers and dates to date
sureword_everything_df$publication_date<-strptime(sureword_everything_df$publication_date, format = "%Y-%m-%d")
sureword_everything_df$pagecount<-as.numeric(sureword_everything_df$pagecount)
sureword_everything_df$body<-gsub('[[:cntrl:]]', '', sureword_everything_df$body)
sureword_everything_df$figure_table_caption<-gsub('[[:cntrl:]]', '', sureword_everything_df$figure_table_caption)
sureword_everything_df$materials_and_methods<-gsub('[[:cntrl:]]', '', sureword_everything_df$materials_and_methods)
sureword_everything_df$results_and_discussion<-gsub('[[:cntrl:]]', '', sureword_everything_df$results_and_discussion)
sureword_everything_df$introduction<-gsub('[[:cntrl:]]', '', sureword_everything_df$introduction)
sureword_everything_df$body<-tolower(sureword_everything_df$body)
sureword_everything_df$figure_table_caption<-tolower(sureword_everything_df$figure_table_caption)
sureword_everything_df$materials_and_methods<-tolower(sureword_everything_df$materials_and_methods)
sureword_everything_df$results_and_discussion<-tolower(sureword_everything_df$results_and_discussion)
sureword_everything_df$introduction<-tolower(sureword_everything_df$introduction)

## score papers and add score column to dataframe

wordcount<-data.frame(1)
for(j in 1:length(sureword_everything_df$body)){
  for(i in 1:length(surewords)){
    wordcount[j,i]<-length(grep(surewords[i], sureword_everything_df$body[j]))
    }
}


sentiment_score = score.sentiment(source_filtered_df$text, poswords, negwords, .progress = 'text')
paper_sentiment<-merge(source_filtered_df, sentiment_score, by = "text")

## add previously scored papers to newly fetched ones TODO

## filter noise
pos_sentiment<-subset(paper_sentiment, paper_sentiment$score >=2)
neg_sentiment<-subset(paper_sentiment, paper_sentiment$score <=-2)
strong_sentiment<-rbind(droplevels(pos_sentiment), droplevels(neg_sentiment))

## CHARTS & PLOTS

# Plot distribution of phrases
surebarplot<-ggplot(surecount_df, aes(x = reorder(Term, No_Articles), y = No_Articles)) + geom_bar() + coord_flip()
print(surebarplot)
unsurebarplot<-ggplot(unsurecount_df, aes(x = reorder(Term, No_Articles), y = No_Articles)) + geom_bar() + coord_flip()
print(unsurebarplot)

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

## write scored paper out to file for loading and de-duping next run
## write.csv(attreport, file = "C:\\Users\\William Gunn\\Desktop\\My Dropbox\\Scripting\\Data\\webinars\\attreport.csv")