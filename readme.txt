Required Libraries

library(dygraphs) # interactive js plots library(xts) library(knitr) library(dplyr) library(ggplot2) library(wordcloud) library(RColorBrewer) library™ # for document term matrix library(e1071) # for naive bayes library(stringr)
=========================
#read the csv containing df. df<-read.csv(“nbasubredditfull.csv”)

#read the sentiment lexico, a list of >6000 sentiment lexicon gathered from Liu #https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html sent_lexicon <- read.csv(“sent_lexicon.csv”)

#convert the times to UTC standards df$created_timestamp <- as.POSIXct(df$created_utc, tz = “UTC”, origin = “1970-01-01”) df$created_date <- as.Date(format(df$created_timestamp, “%Y-%m-%d”))

#read the author_flair_text to get names of teams, #players, and based on that, get how many users has them in their flair #this starts from the intuition that those would be very popular flares under these subreddits supporter_counts <- df %>% filter(!is.na(author_flair_text)) %>% group_by(author_flair_text) %>% summarize(comments = n(), users = n_distinct(author))

#Simple table generator kable(head(supporter_counts %>% select(author_flair_text, comments, users) %>% arrange(desc(comments)), n = 10), align = “c”, col.names = c(“Team”, “Comments”, “Users”))
fit a lm coeficient to get redditors that are more active in the subreddits

#and draw a linear trendline for all subreddditors fit_coef <- coef(lm(comments~users, supporter_counts)) ggplot(supporter_counts, aes(x = users, y = comments)) + geom_point() + geom_text(aes(label = ifelse(users > 400, author_flair_text, “”)), size = 4, hjust = -.05) + theme_light(base_size=16) + labs(xlab = “# of Unique Users”, ylab = “# of Comments”) + xlim(c(0, 6000)) + ggtitle(“Active Fans”) + geom_abline(intercept = fit_coef[1], slope = fit_coef[2])

#Reduce the author flair to only those equal to the names of the teams in the #finals in their respective conferences c_finals <- df %>% filter(author_flair_text %in% c(“Atlanta”,“Hawks”,“Atlanta Hawks”,“Cleveland ”,“Cavaliers”,“Cleveland Cavaliers”,“Golden”,“State”,“Warriors”, “Golden State Warriors”,“Houston”,“Rockets”,“Houston Rockets”))%>% group_by(created_date, author_flair_text) %>% dplyr::summarize(comments = n()) #further subset the dataframe into individual team flairs and prepare to create a
a Time Series plot only for the four teams

hw <- filter(c_finals, author_flair_text == “Hawks”) cav <- filter(c_finals, author_flair_text == “Cavaliers”) war <- filter(c_finals, author_flair_text == “Warriors”) roc <- filter(c_finals, author_flair_text == “Rockets”) y <- cbind(“Atlanta Hawks” = xts(hw$comments, hw$created_date), “Cleveland Cavaliers” = xts(cav$comments, cav$created_date), “Golden State Warriors” = xts(war$comments, war$created_date), “Houston Rockets” = xts(roc$comments, roc$created_date))

plot_ts <- function(y, ylabel, yrange){ #using dygraph to make a time series and add the event descript to- #ease the analysis dygraph(y, main = “Reddit Comments over NBA Conference Finals ”) %>% dyAxis(“x”, drawGrid = FALSE) %>% dySeries(“Atlanta.Hawks”) %>% dySeries(“Cleveland.Cavaliers”) %>% dySeries(“Golden.State.Warriors”) %>% dySeries(“Houston.Rockets”) %>% dyEvent(“2015-05-19”, “Rockets 106 - Warrior 110”, labelLoc = “top”) %>% dyEvent(“2015-05-20”, “Cavaliers 97 - 89 Hawks ”, labelLoc = “top”) %>% dyEvent(“2015-05-21”, “Rockets 98 - 99 Warriors”, labelLoc = “top”) %>% dyEvent(“2015-05-22”, “Cavaliers 94 - 82 Hawks”, labelLoc = “top”) %>% dyEvent(“2015-05-23”, “Warriors 115 - 80 Rockets”, labelLoc = “top”) %>% dyEvent(“2015-05-24”, “Hawks 111 - 113 Cavaliers ”, labelLoc = “top”) %>% dyEvent(“2015-05-25”, “Warriors 115 -128 Houston”, labelLoc = “top”) %>% dyEvent(“2015-05-26”, “Hawks 88 - Cavaliers 118”, labelLoc = “top”) %>% dyEvent(“2015-05-27”, “Rockets 90- 104 Warriors”, labelLoc = “top”) %>% dyAxis(“y”, label = ylabel, valueRange = yrange) %>% dyRangeSelector() } #call the function responsible to prepare and plot the TS graph plot_ts(y, “Comments”, c(0, 5000))

#get the deviation from the mean to make better sense of the values #this comes after seeing the discrepancies in th unnormalized values mean_dev <- function(d){ d$comments/mean(d$comments) - 1 }

#plot again the same graph using the normalized values hw$dev <- mean_dev(hw) cav$dev <- mean_dev(cav) war$dev <- mean_dev(war) roc$dev <- mean_dev(roc) y <- cbind(“Atlanta Hawks” = xts(hw$dev, hw$created_date), “Cleveland Cavaliers” = xts(cav$dev, cav$created_date), “Golden State Warriors” = xts(war$dev, war$created_date), “Houston Rockets” = xts(roc$dev, roc$created_date))

plot_ts(y, “NBA Conference Finals Comments as Proportion of Mean”, c(-1, 6))

#create a time interval slot for capturing the activities during the game day interval = c(as.POSIXct(“2015-05-24 12:00:00”, “UTC”), as.POSIXct(“2015-05-25 12:00:00”, “UTC”)) # (roughly) 24 hours post match

#fetch all comments and flair_texts between the interval post_match = df %>% filter(created_timestamp > interval[1], created_timestamp < interval[2], author_flair_text %in% c(“Rockets”, “Warriors”)) %>% select(author_flair_text, body) #write.csv(post_match,“D:/DATA SCIENCE DATA/comments_game_day.csv”)

library(dplyr) #function slitly from Jeffrey Breen's seminar on twitter sentiment analysis with R #http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ get_sentiment = function(sentences, poses, negs, .progress='none') { # create simple array of scores with laply scores = laply(sentences, function(sentence, poses, negs) { # remove punctuation sentence = gsub(“[[:punct:]]”, “”, sentence) # remove control characters sentence = gsub(“[[:cntrl:]]”, “”, sentence) # remove digits? sentence = gsub('\d+', '', sentence)

               # define error handling function when trying tolower
               tryTolower = function(x)
               {
                 # create missing value
                 y = NA
                 # tryCatch error
                 try_error = tryCatch(tolower(x), error=function(e) e)
                 # if not an error
                 if (!inherits(try_error, "error"))
                   y = tolower(x)
                 # result
                 return(y)
               }
               # use tryTolower with sapply 
               sentence = sapply(sentence, tryTolower)

               # split sentence into words with str_split (stringr package)
               word.list = str_split(sentence, "\\s+")
               words = unlist(word.list)

               # compare words to the dictionaries of positive & negative terms
               pos.matches = match(words, poses)
               neg.matches = match(words, negs)

               # get the position of the matched term or NA
               # we just want a TRUE/FALSE
               pos.matches = !is.na(pos.matches)
               neg.matches = !is.na(neg.matches)

               # final score
               score = sum(pos.matches) - sum(neg.matches)
               return(score)
             }, poses, negs, .progress=.progress )

# data frame with scores for each sentence scores.df = data.frame(text=sentences, score=scores) return(scores.df) }

#get the sentiment intensity on all the comments sentiment = get_sentiment(post_match$body, sent_lexicon$positive, sent_lexicon$negative, .progress='text')

#add to a new column in the post_match d.f post_match$sentiment<- sentiment$score junk$nm[junk$nm == “B”] <- “b” post_match$sentVal[post_match$sentiment<=-2]<-“Very Negative” post_match$sentVal[post_match$sentiment==-1]<-“Negative” post_match$sentVal[post_match$sentiment==0]<-“Neutral” post_match$sentVal[post_match$sentiment>=2]<-“Very Positive” post_match$sentVal[post_match$sentiment==1]<-“Positive”
boxplot

qplot(author_flair_text, sentiment,data=post_match,fill = author_flair_text,geom=c(“boxplot”, “jitter”), main=“Sentiment by Team”, xlab=“Teams”, ylab=“Sentiment intensity”)

#Create a list of emotion polarity and intensities emotion_list = levels(factor(post_match$sentVal)) emot_numb = length(emotion_list) emotion.coments = rep(“”, emot_numb)

for (i in 1:emot_numb) { #tmp = post_match$body[post_match$sentVal == emotion_list[i]] tmp= subset( post_match$body, post_match$sentVal == emotion_list[i]) emotion.coments[i] = paste(tmp, collapse=“ ”) }
remove stopwords

emotion.coments = removeWords(emotion.coments, stopwords(“english”))
create corpus

corpus = Corpus(VectorSource(emotion.coments)) tdm = TermDocumentMatrix(corpus) tdm = as.matrix(tdm) colnames(tdm) = emotion_list
make a word cloud to give a sense of what is actually going on

comparison.cloud(tdm, max.words = 1000, colors = brewer.pal(emot_numb, “Dark2”), scale = c(3,.5), random.order = FALSE, title.size = 1.5)
