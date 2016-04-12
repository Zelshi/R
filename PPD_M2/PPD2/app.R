library(shiny)
library("twitteR")
library("wordcloud")
library("tm")
#interface
ui <- fluidPage(
  titlePanel("Tweak tweets with keywords"),
  #input side bar
  sidebarLayout(
    sidebarPanel(
      
      textInput("handle", "Search Tweets:"),
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=1500, value = 200),
      downloadButton("download","Download File")
    
       ),
    #Output() du nuage de mots
    mainPanel(
      plotOutput("plot")
    )
  ),
  dataTableOutput("table")
 
)

server <- function(input, output) {
  consumer_key <- 'PWoJnyBWRqNFOaylJq6usQxq9'
  consumer_secret <- 'kxGUKNeuBtvvWE0GrMInJIgQBaS08HecItLfRAfa6EaMZqUXIf'
  access_token <- '709350050737098752-kIo4mA9qi5xOJmuSxMC5Tf8PkLvLs4p'
  access_secret <- 'fL0manfNLuiX4s7qz4HzwOmin1JyuUEHt7e0frAHyfZKQ'
  my_oauth <- setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                                  access_token = access_token, access_secret = access_secret)
  TweetFrame<-function(searchTerm, maxTweets)
  {
    text1 <- searchTwitter(searchTerm, maxTweets)
    # Get text data from the result of Twitter search
    text1 <- sapply(text1, function(x) x$getText())
    # Remove retweets
    text1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text1)
    # Remove at people
    text1 = gsub("@\\w+", "", text1)
    # Remove punctuation
    text1 = gsub("[[:punct:]]", "", text1)
    # Remove numbers
    text1 = gsub("[[:digit:]]", "", text1)
    # Remove html links
    text1 = gsub("http\\w+", "", text1)
    # remove unnecessary spaces
    text1 = gsub("[ \t]{2,}", "", text1)
    text1 = gsub("^\\s+|\\s+$", "", text1)
    # define "tolower error handling" function 
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
    #
    # lower case using tryTolower with sapply 
    text1 = sapply(text1, tryTolower)
    #
      text1_corpus <- Corpus(VectorSource(text1))
      motClef <- tm_map(text1_corpus,
      content_transformer(function(x) iconv(x, 'UTF-8','ASCII')),
      mc.cores=1
    )
    motClef <- tm_map(motClef, content_transformer(tolower), mc.cores=1)
    motClef <- tm_map(motClef, removePunctuation, mc.cores=1)
    motClef <- tm_map(motClef, function(x)removeWords(x,stopwords()), mc.cores=1)
    return(wordcloud(motClef, random.color=TRUE))
  }
  
  #Output nuage de mots
  output$plot <- renderPlot({TweetFrame(input$handle, input$maxTweets)})
  #Tweet sous forme de tableau
  output$table <- renderDataTable({
    TweetFrame<-function(searchTerm, maxTweets)
    {
      twtList<-searchTwitter(searchTerm,n=maxTweets)
      twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
      twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') *#WILL, THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/…/twitter-use…/2013-May/000335.html
      return(twtList1)
    }
    entity1<-reactive({entity1<-TweetFrame(input$handle, input$maxTweets)})
    output$table <- renderDataTable({tab<-entity1()[1]})
    #telecharger sous forme csv les tweets
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                       content = function(file){
                                         write.csv(entity1(), file)
                                       }
    )
  })
}


shinyApp(ui = ui, server = server)