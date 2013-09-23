# A script to scrape the top comments from a specified posts on Reddit.
# It also can plot a word cloud from the most common words on the page. 

# Notes:
#
# This won't work on NSFW subreddits that require you to click that you're 18 
# or older. I haven't looked at how to code that yet.

# Overview of the function:  
# 1. Get the post and scrape the comments
# 2. Clean it up
# 3. Get the word frequency 
# 4. Plot a word cloud
# 5. Return a word frequency table


redditScrape <- function(postLink, plotCloud = TRUE, saveText = FALSE, myDirectory = "/choose/a/directory") {


	#######################################################
	# 0. Load the required packages.  And check a few items
	
	require(XML)
	require(RCurl)
	require(RColorBrewer) 
	require(wordcloud)
	

	#######################################################
	#  1. Scrape the page
	#  This will scrape a page and put it in to 
	#  an R list object 
	thePost <- getURL(postLink)
	textList <- htmlParse(thePost, asText = TRUE)
	textList <- xpathSApply(textList, "//p", xmlValue)
	
		
	#######################################################
	#  2. Clean up the text.

	# Remove the submitted lines and lines at the end of each page
	submitLine <- grep("submitted [0-9]", textList) 
	textList <- textList[{(submitLine[1] + 1):(length(textList)-10)}]
	
	# Removing lines capturing user and points, etc.
	# Yes, there could be fewer grep calls, but this made it 
	# easier to keep track of what was going on.
	grep('points 1 minute ago', textList) -> nameLines1
	grep('points [0-9] minutes ago', textList) -> nameLines2
	grep('points [0-9][0-9] minutes ago', textList) -> nameLines3
	grep("points 1 hour ago", textList) -> nameLines4
	grep("points [0-9] hours ago", textList) -> nameLines5
	grep("points [0-9][0-9] hours ago", textList) -> nameLines6
	grep('points 1 day ago', textList) -> nameLines7
	grep('points [0-9] days ago', textList) -> nameLines8
	grep('points [0-9][0-9] days ago', textList) -> nameLines9
	grep('points 1 month ago', textList) -> nameLines10
	grep('points [0-9] months ago', textList) -> nameLines11
	grep('points [0-9][0-9] months ago', textList) -> nameLines12
	allLines <- c(nameLines1, nameLines2, nameLines3, nameLines4, 
			nameLines5, nameLines6, nameLines7, nameLines8, nameLines9, 
			nameLines10, nameLines11, nameLines12)
	textList <- textList[-allLines]
	textList <- textList[textList!=""]
	textList <- tolower(textList)

  
	# Let's simplify our list. Could have been done earlier, but so it goes. 
	# This actually isn't necessary, but I didn't want to refactor.  Got a bit lazy.
	# Sorry. 
	allText <- unlist(textList)

	# Remove the punctuation, links, etc.
	allText <- gsub("https?://[[:alnum:][:punct:]]+", "", allText)
	allText <- gsub("[,.!?\"]", "", allText)
	allText <- strsplit(allText, "\\W+", perl=TRUE)
	allText <- unlist(allText)

	# Remove frequent words and orphans of contractions (that sounds 
	# sadder than it is).
	frequentWords <- c("the", "be", "been", "to", "of", "and", "a", "in", 
	"that", "have", "i", "it", "for", "not", "on", "with", "he", "as", "you", 
	"do", "at", "this", "but", "his", "by", "from", "they", "we", "say", "her", 
	"she", "or", "an", "will", "my", "one", "all", "would", "there", "their", 
	"what", "so", "up", "out", "if", "about", "who", "get", "which", "go", 
	"me", "when", "make", "can", "like", "time", "no", "just", "him", "know", 
	"take", "people", "into", "year", "your", "good", "some", "could", "them", 
	"see", "other", "than", "then", "now", "look", "only", "come", "its", 
	"over", "think", "also", "back", "after", "use", "two", "how", "our", 
	"work", "first", "well", "way", "even", "new", "want", "because", "any", 
	"these", "give", "day", "most", "us", 'is', 'are', 'was', 'were', 'i', 's', 
	'was', 'don', 'aren', 'points1', 'point', 't', 'm', 'points0', '10', '1', 
	're', 'll', 'd', '2', '3', '4', '5', '6', '7', '8', '9', 'doesn','d', 've', 
	'r', 'has', 'had', 'been', 'being', '0', 'more', 'really', 'isn', 'very', 
	'am', 'didn', 'wouldn', '', 'points', 'point', 'months', 'ago', 'deleted', 
	'much')

	for (i in 1:length(frequentWords)) { 
		allText <- allText[allText!=frequentWords[i]]
	}

	# Save the file to your drive. This way you can drop it into
	# Wordle.net or use it other places. 
	if (saveText == TRUE) {
		curWD <- getwd() 
		setwd(myDirectory)
		filename <- paste("Reddit Comments Postscrub ", subred, " ", time, " ", 
			Sys.time(),".txt", sep = "") 
		write.table(allText, file = filename, row.names=F, col.names=F, append=T)
		# save(allText, file = filename)
		textListBackup <- textList
		setwd(curWD)
	}

	#######################################################
	#  3. Word frequency table

	textTable <- table(allText)
	textTable <- sort(textTable, decreasing = TRUE)

	#######################################################
	#  4. Plot word cloud

	if (plotCloud == TRUE) {
		# This is a nice option.  Just use a portion of the 0-1 for color
		rainbow(30,s=.8,v=.6,start=.5,end=1,alpha=1) -> pal
		wordcloud(names(textTable[1:200]), textTable[1:200], scale = c(4,.5), colors = pal)
	}

	#######################################################
	#  5. Return the text table

	textTable
}


