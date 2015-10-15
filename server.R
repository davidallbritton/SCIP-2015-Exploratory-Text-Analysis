## shiny app for exploratory text mining in R
## David Allbritton
## Society for Computer in Psychology, 2015 Annual Meeting
#
#  server.R

if (!require(shiny)) {install.packages("shiny"); library(shiny)}

################################
# Define shiny server logic;  reactive stuff starts here ####
shinyServer(function(input, output) {  
  
  ## set default values:
  colNum <- 1  #default is column 1
  stopWordsFile <- stopWordsFile.default <- "stopWords.xlsx"
  min.freq.default <- 3  
  maxPhraseLength <- 4  #note that this is hard coded in server.R & ui.R separately; have to change it in both places
  
  ## select the input file:
  inFile <-  reactive(input$inFile)
  
  # load or install the required R packages for reading Excel files and data manipulation:
  if (!require(xlsx)) {install.packages("xlsx"); library(xlsx)}
  # load or install R packages required specifically for doing a word cloud:
  if (!require(wordcloud)) {install.packages("wordcloud"); library(wordcloud)}
  if (!require(tm)) {install.packages("tm"); library(tm)}
  #  detach ggplot2 before using NLP because it masks nlp::annotate
  if (sum(search() == "package:ggplot2")) detach(package:ggplot2)
  if (!require(NLP)) {install.packages("NLP"); library(NLP)}
  if (!require(SnowballC)) {install.packages("SnowballC"); library(SnowballC)}   # for stemming
  
  ### define functions
  # function to clean up the documents vector:
  cleanUpDocs <- function (docs, myStopWords = c("a", "the"))  {
    toSpace <- content_transformer(function(x,pattern) gsub(pattern," ", x))
    toApost <- content_transformer(function(x,pattern) gsub(pattern,"'", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, toApost, "[^a-zA-Z ]")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # remove own custom stop words
    docs <- tm_map(docs, removeWords, myStopWords) 
    docs <- tm_map(docs, stripWhitespace)
    docs
  }
  
  # read input file
  dfInfile <- reactive(read.csv(file=inFile()))  #reading from sheet 1
  
  # read stop words file
  myStopWordsFromFile <- c("a", "the")
  # read stop words file
  if(file.exists(stopWordsFile)) {                        #check existence of stop words file
    dfstopWords <- read.xlsx(file=stopWordsFile, 1)       #reading from sheet 1
    myStopWordsFromFile <- tolower(as.character(dfstopWords[,1]))
  }
  
  # get the selected column of texts and clean them up:
  myTexts1 <- reactive (Corpus(VectorSource(dfInfile()[,colNum])))    #default is column 1
  myTexts <- reactive (cleanUpDocs(myTexts1(), myStopWordsFromFile))  # removing stop words, etc.
  myTexts.stemmed <- reactive(tm_map(myTexts(), stemDocument))      # stemmed version
  
  ### create all the frequency lists that will be used for creating output; 
  ### store as one reactive list of lists
  AllFreqListsReactive <- reactive({          ## start creating collection of frequency lists
    
    # initialize some lists to put the results in:
    dtm <- list()                #  document-term matrix
    dtm.stemmed <- list()        #  
    list.freq <- list()          #  ngram frequencies from dtm; will be added to allFreqList
    list.freq.stemmed <- list()  #  will be added to allFreqList
    allFreqList <- list()        #  will be returned and stored as a reactive object; refreshed 
    #  whenever input file changes
    
    #################################
    ####  iterate over N=1..4 for Ngram lengths 
    for(n in 1:maxPhraseLength){          # for each Ngram size from 1 to max length
      # n is the phraselength
      # create document-term matrix of word or ngram counts:  
      dtm[[n]] <- ({
        NgramTokenizer <-
          # define a function for using bigrams or ngrams rather than single words
          function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)  
        DocumentTermMatrix(myTexts(), control = list(tokenize = NgramTokenizer)) 
      })
      dtm.stemmed[[n]] <-  ({
        # define a function for using bigrams or ngrams rather than single words (same as above)
        NgramTokenizer2 <-
          function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)  
        DocumentTermMatrix(myTexts.stemmed(), control = list(tokenize = NgramTokenizer2))
      })
      # create vector of word frequencies:
      freq <- sort(colSums(as.matrix(dtm[[n]])), decreasing=TRUE)    
      freq.stemmed <- sort(colSums(as.matrix(dtm.stemmed[[n]])), decreasing=TRUE)  
      
      # add to the lists of frequencies, one for each ngram size:
      list.freq[[n]] <- freq
      list.freq.stemmed[[n]] <-  freq.stemmed
      
    }  # end for loop over Ngram sizes
    #################################
    
    allFreqList[[1]] <- list.freq           # 1 = unstemmed
    allFreqList[[2]] <- list.freq.stemmed   # 2 = stemmed
    allFreqList       ## return the list of lists (2) of dataframes (4) to assign to reactive object
  })                                        ## end creating collection of frequency lists
  
  
  ## The next assignment statements reactively choose the frequency data to display based on UI selections
  freq.Reactive <- reactive({  # pick the stemmed vs unstemmed version for the selected ngram size:
    if (input$useStemming) stemmed <- 2 else stemmed <- 1
    AllFreqListsReactive()[[stemmed]][[input$phraseLength]]
  })
  maxFreqReactive <- reactive(max(freq.Reactive()))
  # duplicate the word frequencies as a data frame for displaying in a table:
  fullFreqList.Reactive <- reactive (data.frame(Ngram=names(freq.Reactive()), Frequency=freq.Reactive()))   
  
  #  Only load this after doing NLP stuff, because it masks nlp::annotate
  if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
  
  # start writing outputs for the UI to display:
  
  # create the UI element that selects the minimum frequency to plot:
  output$setMinFreq <- renderUI({                      # creating a UI element for choosing min frequency
    sliderInput('minFreq', 'Min frequency to plot',
                min = 1, max = maxFreqReactive(),  min(min.freq.default, maxFreqReactive()), step = 1, round = TRUE)
  })
  output$maxObservedFreq <- renderText (paste0 ("Max Ngram Frequency: ", (max(freq.Reactive()))))
  output$totalObservedNgrams <- renderText (paste0 ("Total unique Ngrams: ", (length(freq.Reactive()))))
  output$freqTable <- renderTable({
    table(freq.Reactive(), dnn = c("Frequency of Ngram (word/phrase) Frequencies"))
  })
  output$plot1wordCloud <- renderPlot({
    cat("updating word cloud.....\n")
    wordcloud(names(freq.Reactive()), freq.Reactive(), min.freq=min(input$minFreq, maxFreqReactive()), max.words=input$maxWords, colors=brewer.pal(6,"Dark2"),random.order=FALSE, scale=c(input$maxScale,input$minScale))
  })
  output$plot2topNgrams <- renderPlot({
    wf <- transform(fullFreqList.Reactive(), Ngram = reorder(Ngram, -Frequency))
    wf <- subset(wf, Frequency>=min(input$minFreq, maxFreqReactive()))
    wf <- wf[1:(min(input$maxWords,length(wf[,1]))),]
    p <- ggplot(wf, aes(Ngram, Frequency))
    p <-p+ geom_bar(stat="identity", width = .7, fill= "#99CCFF")
    p <-p+ theme(axis.text.x=element_text(angle=45, hjust=1, color="blue"))
    p
  })
  output$table2allNgrams <- renderDataTable({
    fullFreqList.Reactive()
  })
})


