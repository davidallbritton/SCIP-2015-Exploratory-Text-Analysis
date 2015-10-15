## shiny app for exploratory text mining in R
## David Allbritton
## Society for Computer in Psychology, 2015 Annual Meeting
#
# ui.R

# set default values:
columnNum.default <- 1
max.words.default  <- 50
# min.freq.default <- 3   goes in server.R instead
minScale.default <- 0.1
maxScale.default <- 2
phraseLength.default <- 1   # set to 1 for single words; 2 for 2-word phrases, etc.  Ngram size.
maxPhraseLength <- 4  #note that this is hard coded in server.R & ui.R separately; have to change it in both places
myStopWords.default <- c("however", "also", "actually", "something", "rather")

# select an Excel file in the current working directory for input:
filenames<-list.files(pattern="\\.csv$")

# Define UI 
fluidPage(
  headerPanel('Exploratory Text Analysis in R with NLP and Wordcloud'),
  sidebarPanel(
    #selectInput('colNumCode', 'Column in Input File', 1:4),
    selectInput(inputId = "inFile",
                label = "Choose Input File",
                filenames
    ),
    checkboxInput('useStemming', 'Stemming', value = FALSE),   
    #    conditionalPanel (                  ##### stem completion not yet implemented
    #      condition = "input.useStemming == true",
    #      checkboxInput ('stemCompletion', 'Stem Completion', value = FALSE)
    #    ),
    sliderInput ('phraseLength', 'Words per Phrase (Ngram size)', min =1, max = 4, value = phraseLength.default, round = TRUE, ticks = FALSE),
    textOutput ("totalObservedNgrams"),
    sliderInput ('maxWords', 'Max Ngrams to plot',
                 min = 2, max = 200,  max.words.default, step = 1, round = TRUE),
    textOutput ("maxObservedFreq"),
    uiOutput ("setMinFreq"),
    sliderInput ('maxScale', 'Max font size for Cloud',
                 min = 0.2, max = 10, value = maxScale.default),
    sliderInput ('minScale', 'Min font size for Cloud',
                 min = 0.1, max = 2, value = minScale.default),
    tableOutput("freqTable")
  ),
  mainPanel(
    plotOutput("plot1wordCloud"),
    plotOutput("plot2topNgrams"), HTML("<h2>All Ngram Frequencies</h2>"),
    dataTableOutput("table2allNgrams")
  )
)