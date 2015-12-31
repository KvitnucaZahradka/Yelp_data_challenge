# EVALUATION CLASS

# The methods of this class use the java garbage collection gc() together with the fault tolerant implementation of some java enabled functions.
# More specifically: the tagPOS and tagPOSr embeded in Epsilon() method of the class words use the java implemented tagging methods of tagging libraries openNLP. 
# The issue is that we need to call the java garbage collection while running those methods. Moreover since we want to loop through a large portion of data we needed 
# to implement those tagging functions in a fault tolerant way. The method also saves result on a local drive.

## THE DATA STRUCTURE DECLARATION:  

evaluate<-setClass(
        #### Set name for the class.
        "evaluate",
        
        #### Define slots: text = the text we want to tag
        ####               data = generic filename.
        slots = c(text="character",filename="character"),
        
        #### Set default values (optional).
        prototype=list(text="default text"),
        
        #### Validity (optional, in this case it will do nothing since the class methid is checking whether its a character in the first place).
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
### The method eval_summary() saves the tagged text on a local drive as the .rds file and returns the tagged text.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="eval_summary",
           def=function(theObject)
           {
                   standardGeneric("eval_summary")
           }
)



setMethod(f="eval_summary",
          signature="evaluate",
          definition=function(theObject)
          {
                  #### Required fault tolerant function.
                  extra<-function(x){temp<-words(text=x);return(as.vector(Epsilon(temp)));gc()}
                  FaultTolerantTestFunction <- function(x) {
                          tryCatch({ret <- extra(x);gc()}, error = function(e) {ret <<- NA});
                          ret
                  }
                  
                  #### Find near nouns function.
                  find<-function(x){return(as.vector(findNearNouns(x)));gc()}
                  nouns <- function(x) {
                          tryCatch({ret <- find(x);gc()}, error = function(e) {ret <<- NA});
                          ret
                  }
                  
                  #### Find and save.
                  
                  GURU<-sapply(theObject@text,FaultTolerantTestFunction,USE.NAMES=F)
                  saveRDS(GURU,paste(theObject@filename,"_GURU_file.rds"))
                  GURU<-unlist(GURU)
                  GURU<-paste(GURU, sep=" ", collapse=" ")
                  GURU<-words(text=GURU)
                  GURU<-nouns(GURU)
                  GURU<-gsub("\\/NN+$|\\/NNS+$", "", GURU)
                  GURU<-paste(GURU, sep=" ", collapse=" ")
                  saveRDS(GURU,paste(theObject@filename,"_summary_text.rds"))
                  GURU<-words(text=GURU)
                  GURU<-freqM(GURU)
                  return(GURU)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------

