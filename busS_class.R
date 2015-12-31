# BUSINESS SUBSETTING CLASS 

## THE DATA STRUCTURE DECLARATION:

busS<-setClass(
        #### Set name for the class.
        "busS",
        
        #### Define slots: data_rev = the review or tips data.frame from the Yelp data set
        ####               data = the business data.frame from the Yelp data set
        ####               city = which city we want to subset
        ####               business = what business we want to subset
        ####               year =  for what year we want to do the subsetting.
        
        slots = c(data_rev="data.frame",data="data.frame",city="factor",business="character",year="numeric"),
        
        
        #### Validity (optional, in this case it will do nothing since the class methid is checking whether its a right data type in the first place).
        validity=function(object)
        {
                if(!is.data.frame(object@data)) {
                        return("Object give is NOT a data.frame()")
                }
                return(TRUE)
        }
)

## METHODS:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The getBusinessId() method returnes the Yelp data set id for a subset business.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="getBusinessId",
           def=function(theObject)
           {
                   standardGeneric("getBusinessId")
           }
)

setMethod(f="getBusinessId",
          signature="busS",
          definition=function(theObject)
          {  
                  df<-subset(theObject@data,theObject@data$city %in% theObject@city)
                  bool<-sapply(1:nrow(df),function(x){theObject@business%in%unlist(df$categories[x])})
                  df<-df[bool,]
                  rm(bool)
                  return(df$business_id)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The method getWholeBusiness() returns the business reviews in a given city for a given business type in a given year.
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="getWholeBusiness",
           def=function(theObject)
           {
                   standardGeneric("getWholeBusiness")
           }
)

setMethod(f="getWholeBusiness",
          signature="busS",
          definition=function(theObject)
          {  
                  id<-getBusinessId(theObject)
                  df<-subset(theObject@data_rev,theObject@data_rev$business_id%in%id)
                  parsed_dates<-parse_date_time(df$date,"%y%m%d")
                  df$year<-year(parsed_dates)
                  df<-df[df$year==theObject@year,]
                  rm(id)
                  return(df)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------