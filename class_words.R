# SENTIMENT ANALYSIS CLASS

## THE DATA STRUCTURE DECLARATION:

words<-setClass(
        #### Set name for the class.
        "words",
        
        #### Define slots: text = the text we want to analyse.
        slots = c(text="character"),
        
        #### Set default values (optional).
        prototype=list(text="default text"),
        
        ### Validity (optional, in this case it will do nothing since the class methid is checking whether its a character in the first place).
        validity=function(object)
        {
                if(!is.character(object@text)) {
                        return("Object give is NOT a text")
                }
                return(TRUE)
        }
)

## METHODS:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method getMatrix() returns the strsplit() of the corpus processed text as data.frame.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="getMatrix",
           def=function(theObject)
           {
                   standardGeneric("getMatrix")
           }
)


setMethod(f="getMatrix",
          signature="words",
          definition=function(theObject)
          {
                cor_matr<-function(x){
                          y<-VectorSource(paste(x,collapse=" "))
                          corpus<-Corpus(y)
                          corpus<-tm_map(corpus,content_transformer(tolower))
                          corpus<-tm_map(corpus, removePunctuation)
                          corpus<-tm_map(corpus, stripWhitespace)
                          corpus<-tm_map(corpus, removeWords, stopwords(kind="en"))
                          df<-corpus[[1]]$content
                          df<-unlist(strsplit(df,' '))
                          df<-df[df!=""]
                        return(as.data.frame(df))
                  }
                     return(cor_matr(theObject@text))
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method getPreparsed() gives the corpus content, using the library "koRpus".
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="getPreparsed",
           def=function(theObject)
           {
                   standardGeneric("getPreparsed")
           }
)


setMethod(f="getPreparsed",
          signature="words",
          definition=function(theObject)
          {
                  cor_matr<-function(x){
                          y<-VectorSource(paste(x,collapse=" "))
                          corpus<-Corpus(y)
                          corpus<-tm_map(corpus,content_transformer(tolower))
                          corpus<-tm_map(corpus, removePunctuation)
                          corpus<-tm_map(corpus, stripWhitespace)
                          corpus<-tm_map(corpus, removeWords, stopwords(kind="en"))
                          df<-corpus[[1]]$content
                          #dtm <- DocumentTermMatrix(corpus)
                          #dtm2<-as.matrix(dtm)
                          return(df)
                  }
                  return(cor_matr(theObject@text))
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The sentI() method returns negative sentiment words.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="sentI",
           def=function(theObject)
           {
                   standardGeneric("sentI")
           }
)

setMethod(f="sentI",
          signature="words",
          definition=function(theObject)
          {  
                  jj<-getMatrix(theObject)
                  sentiment<-sapply(as.character(jj$df),function(x){get_sentiment(x, method="bing")})
                  rm(jj)
                  df<-cbind(as.data.frame(sentiment),names(sentiment),c(1:length(sentiment)))
                  names(df)<-c("sentimet","text","id")
                  df<-subset(df,sentiment<0)
                  return(df)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The sentIfull() returns full sentiment analysis withtext and the word positions (as id).
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="sentIfull",
           def=function(theObject)
           {
                   standardGeneric("sentIfull")
           }
)

setMethod(f="sentIfull",
          signature="words",
          definition=function(theObject)
          {  
                  jj<-getMatrix(theObject)
                  sentiment<-sapply(as.character(jj$df),function(x){get_sentiment(x, method="bing")})
                  rm(jj)
                  df<-cbind(as.data.frame(sentiment),names(sentiment),c(1:length(sentiment)))
                  names(df)<-c("sentimet","text","id")
                  return(df)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The tagPOSr() is a generic function used for the tagging, note: the tagPOS() function has to be defined also GLOBALLY in the main code.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="tagPOSr",
           def=function(theObject)
           {
                   standardGeneric("tagPOSr")
           }
)

setMethod(f="tagPOSr",
          signature="words",
          definition=function(theObject)
          {  
           
                  tagPOS <-  function(x, ...) {
                             s <- as.String(x)
                             word_token_annotator <- Maxent_Word_Token_Annotator()
                             a2 <- Annotation(1L, "sentence", 1L, nchar(s))
                             a2 <- annotate(s, word_token_annotator, a2)
                             a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
                             a3w <- a3[a3$type == "word"]
                             POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
                             POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
                             list(POStagged = POStagged, POStags = POStags)
                           }
                  return(tagPOS(theObject@text))
          }
)


### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The findJJsentI() uses the globally defined tagPOS. This method returnes the sentiment of text together with the word index (position).
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="findJJsentI",
           def=function(theObject)
           {
                   standardGeneric("findJJsentI")
           }
)

setMethod(f="findJJsentI",
          signature="words",
          definition=function(theObject)
          {  
                  temp<-words(text=getPreparsed(theObject))
                  tem<-subset(sentIfull(theObject),tagPOSr(temp)$POStags=="JJ")
                  rm(temp)
                  tem<-data.frame(tem[,1],tem[,2],tem[,3])
                  names(tem)<-c("sentiment","text","index")
                  return(subset(tem,sentiment<0))
          }
)


### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The findNearNouns() looks for nearby nouns based on findJJsentI() method, returns unlisted set of nouns.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setMethod(f="findNearNouns",
                     signature="words",
                     definition=function(theObject)
                             {  
                                             temp<-words(text=getPreparsed(theObject))
                                             tun<-cbind(as.data.frame(strsplit(tagPOSr(temp)$POStagged,' ')),as.data.frame(tagPOSr(temp)$POStags))
                                             names(tun)<-c("word","type")
                                             tun$index<-c(1:nrow(tun))
                                             JJset<-paste(findJJsentI(temp)$text,"/JJ",sep="")
                                             z<-as.character(tun$word)%in%JJset
                                             tunN<-tun[z,]
                                             rm(z,JJset)
                                             j<-sapply(tunN$index,function(x){
                                                                      kusok<-subset(tun,(x-3)<=index&index<=(x+3))
                                                                      kusok<-kusok[grepl("NN",kusok$type),]        
                                                                      if(nrow(kusok)!=0){kusok<-subset(kusok,abs(index-x)==min(abs(index-x)))
                                                                      return(paste(kusok$word))}
                                                                      else{return(paste(NA))}
                                                              }
                                                              )
                                             j<-subset(j,as.character(j)!="NA")
                                             return(unlist(j))
                                     }
           )

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method AllNearNouns() returns all nouns arround the words returned by method sentI().
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="AllNearNouns",
           def=function(theObject)
           {
                   standardGeneric("AllNearNouns")
           }
)


setMethod(f="AllNearNouns",
          signature="words",
          definition=function(theObject)
          {  
                  raw<-getMatrix(theObject)
                  raw$ind<-rownames(raw)
                  names(raw)<-c("raw","ind")
                  raw<-subset(raw,raw%in%sentI(theObject)$text)
                  
                  return(raw)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method Epsilon() retunrs just the nouns that are in the epsilon neighborhood (in this case plus minus three words) of the negative sentiment words. 
### The unlisted list is returned.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="Epsilon",
           def=function(theObject)
           {
                   standardGeneric("Epsilon")
           }
)



setMethod(f="Epsilon",
          signature="words",
          definition=function(theObject)
          {  
                  temp<-getPreparsed(theObject)
                  temp<-strsplit(temp,' ')
                  temp<-unlist(temp)
                  temp<-temp[temp!=""]
                  temp<-as.data.frame(temp)
                  temp$index<-c(1:nrow(temp))
                  names(temp)<-c("text","index")
                  bad<-sentI(theObject)$id
                  j<-sapply(bad,function(x){
                          kusok<-subset(temp,(x-3)<=index&index<=(x+3));
                          return(paste(kusok$text))
                          }
                  )
                 return(unlist(j))        
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method freqM() returns the word frequencies. It uses the corpus and DocumentTermMatrix() from the library "kuRpus".
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="freqM",
           def=function(theObject)
           {
                   standardGeneric("freqM")
           }
)


setMethod(f="freqM",
          signature="words",
          definition=function(theObject)
          {
                  cor_matr<-function(x){
                          y<-VectorSource(paste(x,collapse=" "))
                          corpus<-Corpus(y)
                          corpus<-tm_map(corpus,content_transformer(tolower))
                          corpus<-tm_map(corpus, removePunctuation)
                          corpus<-tm_map(corpus, stripWhitespace)
                          corpus<-tm_map(corpus, removeWords, stopwords(kind="en"))
                          dtm <- DocumentTermMatrix(corpus)
                          dtm2<-as.matrix(dtm)
                          return(dtm2)
                  }
                  return(cor_matr(theObject@text))
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------

## GLOBAL:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The GLOBAL implementation of the tagPOS() function.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

tagPOS <-  function(x, ...) {
        s <- as.String(x)
        word_token_annotator <- Maxent_Word_Token_Annotator()
        a2 <- Annotation(1L, "sentence", 1L, nchar(s))
        a2 <- annotate(s, word_token_annotator, a2)
        a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
        a3w <- a3[a3$type == "word"]
        POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
        POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
        list(POStagged = POStagged, POStags = POStags)
}

### --------------------------------------------------------------------------------------------------------------------------------------------------------
