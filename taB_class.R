# CLASS FOR TABLE GENERATION

# This class eats up the list of the bad features (with their occurence frequencies) together with the low_cutof and years. The low_cutof slot is used 
# to determine where to low cut the frequencies, i.e. what is the lowest number of frequencies we want to see. User can also tell what years should be 
# included in the resulting table inserting numeric years vector into slot years.

## THE DATA STRUCTURE DECLARATION: 

taB<-setClass(
        #### Set name for the class.
        "taB",
        
        #### Define slots: theSF = the pre-generated list of bad features together with their frequencies.
        ####               low_cutof = the lowest bad feature occurence frequency user wants to see
        ####               years = which years user want to subset.
        slots = c(theSF="list",low_cutof="numeric",years="numeric"),
        
        
        # Validity (optional, in this case it will do nothing since the class methid is checking whether its a character in the first place).
        validity=function(object)
        {
                if(!is.list(object@theSF)) {
                        return("Object give is NOT a list")
                }
                return(TRUE)
        }
)

## METHODS:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The finalTable() method returns the table that has just the bad features in a given set of years and with occurence frequencies bigger that a low_cutof.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="finalTable",
           def=function(theObject)
           {
                   standardGeneric("finalTable")
           }
)

setMethod(f="finalTable",
          signature="taB",
          definition=function(theObject)
          {  
                  #### Functions to remove null observations.
                  is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
                  rmNullObs <- function(x) {
                          x<- Filter(Negate(is.NullOb), x)
                          lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
                  }
                  
                  #### Code.
                  temp<-theObject@theSF
                  v<-sapply(as.character(theObject@years),function(x){paste0("temp$","\"",x,"\"",sep="")})
                  v<-as.data.frame(v)
                  v<-as.vector(v$v)
                  temp<-sapply(v,function(x){eval(parse(text=x))})
                  names(temp)<-theObject@years
                  
                  #### Initial condition.
                  zz<-merge(eval(parse(text=paste0("temp$","\"",as.character(theObject@years[1]),"\"",sep=""))),eval(parse(text=paste0("temp$","\"",as.character(theObject@years[2]),"\"",sep=""))),all=TRUE,sort=FALSE)
                  zz[is.na(zz)] <- 0
                  #### If longer than initial condition.
                  if(length(names(temp))>2){
                  set<-names(temp)[3:length(names(temp))]
                  for(x in set){zz<-merge(zz,eval(parse(text=paste0("temp$","\"",as.character(x),"\"",sep=""))),all=TRUE,sort=FALSE)}
                  zz[is.na(zz)] <- 0
                  }
                  temp<-t(zz)
                  rm(zz)
                  temp<-as.data.frame.matrix(temp)
                  names(temp)<-theObject@years
                  temp<-t(temp)
                  names<-rownames(temp)
                  sub<-sapply(1:ncol(temp),function(x){if(as.logical(max(temp[,x])>theObject@low_cutof)){df<-as.data.frame(temp[,x]);rownames(df)<-rownames(temp);colnames(df)<-colnames(temp)[x];return(df)}},USE.NAMES=T)
                  temp<-rmNullObs(sub)
                  temp<-as.data.frame(temp)
                  rownames(temp)<-names
                  rm(names)
                  return(temp)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The list_for_no_pick() method returnes the list of possibly same meaning words. The user will use the list to decide which words are of the same meaning.
### After picking the words with the same meaning the user have to create the no_pick numeric vector where the indices of the the words with DIFFERENT meaning
### are held. 
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="list_for_no_pick",
           def=function(theObject)
           {
                   standardGeneric("list_for_no_pick")
           }
)

setMethod(f="list_for_no_pick",
          signature="taB",
          definition=function(theObject)
          {  
                  #!!!!!!!!!!!this is used for the no_pick list !!!!!!!!
                  
                  
                  #functions needed in this method
                  where<-function(x,y){
                          temp1<-any(grepl(x,y))
                          temp2<-any(grepl(y,x))
                          if(temp1==TRUE){res<-c(x,y);return(res)}
                          if(temp2==TRUE){res<-c(y,x);return(res)}
                          else{return(NA)}
                  }
                  
                  #code
                  obj<-theObject@theSF
                  yer<-theObject@years
                  nove<-taB(theSF=obj,low_cutof=0,years=yer)
                  tab_tips_0<-finalTable(nove)
                  rm(nove)
                  sorted<-sort(names(tab_tips_0))
                  ww<-sapply(c(1:(length(sorted)-1)),function(x){where(sorted[x],sorted[x+1])})
                  list<-ww[!is.na(ww)]
                  
                  return(list)
          }
          
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The merge_same() method the table of the same structure as the finalTable(), but with merged words of the same meaning. Therefore one needs to create the 
### no_pick vector.
### --------------------------------------------------------------------------------------------------------------------------------------------------------


          
setGeneric(name="merge_same",
                     def=function(theObject)
                     {
                             standardGeneric("merge_same")
                     }
          )

setMethod(f="merge_same",
          signature="taB",
          definition=function(theObject)
          {  
                  #### !!!!!!!!!!! In order to use this method you have to have the "no_pick" list in your main script. !!!!!!!!
                  
                  
                  #### Functions needed in this method.
                  where<-function(x,y){
                          temp1<-any(grepl(x,y))
                          temp2<-any(grepl(y,x))
                          if(temp1==TRUE){res<-c(x,y);return(res)}
                          if(temp2==TRUE){res<-c(y,x);return(res)}
                          else{return(NA)}
                  }
                  
                  #### Code.
                  if(is.double(no_pick)>0&&length(no_pick)>0){
                  obj<-theObject@theSF
                  yer<-theObject@years
                  nove<-taB(theSF=obj,low_cutof=0,years=yer)
                  tab_tips_0<-finalTable(nove)
                  rm(nove)
                  sorted<-sort(names(tab_tips_0))
                  ww<-sapply(c(1:(length(sorted)-1)),function(x){where(sorted[x],sorted[x+1])})
                  list<-ww[!is.na(ww)]
                  orig_list<-names(tab_tips_0)
                  delta<-orig_list[is.na(pmatch(orig_list,unlist(list)))]
                  
                  for(i in no_pick){list[i]<-NA}
                  list<-list[!is.na(list)]
                  
                  if(length(list)>0){
                          toto<-c(list[[length(list)]][1][1],list[[length(list)]][2][1])
                          df<-select(tab_tips_0,one_of(toto))
                          data<-df[1]+df[2]
                  }
                  
                  
                  if(length(list)>1){
                          for(i in c(1:(length(list)-1))){toto<-c(list[[i]][1][1],list[[i]][2][1]);df<-select(tab_tips_0,one_of(toto));data<-cbind(data,df[1]+df[2])}
                  }
                  
                  if(length(delta)>1){data<-cbind(data,select(tab_tips_0,one_of(delta)))}
                  
                  
                  
                  return(data)
                  }
                  else{return("missing or wrong no_pick list")}
          }
          
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------

## GLOBAL:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### For the very final table that is a table produced by merge_same() but further cut by some larger lower bound on occurence frequency. We implemented the GLOBAL
### function callled very_final_table_cut_2(). It has a low cutoff equal to 2. It could be adjusted by the user wish.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

very_final_table_cut_2<-function(temp){sub<-sapply(1:ncol(temp),function(x){if(as.logical(max(temp[,x])>2)){df<-as.data.frame(temp[,x]);rownames(df)<-rownames(temp);colnames(df)<-colnames(temp)[x];return(df)}},USE.NAMES=T);temp<-rmNullObs(sub);temp<-as.data.frame(temp);temp}

### --------------------------------------------------------------------------------------------------------------------------------------------------------





