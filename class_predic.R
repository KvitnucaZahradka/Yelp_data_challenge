# PREDICTION CLASS:

# This class takes the table produced by the class taB method merge_same() and implements various time series model predictions. There are three main time series prediction 
# models: nnetar(), Arima() and auto.arima() from the library forecast. The user can set on and off any of them to be calculated. Moreover user can define from what year
# the calculations should be done. The method pred() of this class returns the list coonsisting of two main parts: 
# $data_frame = the time series prediction for each model for each column (i.e. bad feature) of the initial table together with accuracies of wished models.
# $fits = list of fits for each desired model and each column of the initial table. This part is handy when one wants to plot the time series later on. 

## THE DATA STRUCTURE DECLARATION: 

predic<-setClass(
        ### Set name for the class.
        "predic",
        
        #### Define slots: data = the pre-generated table by the method merge_same() of the class taB.
        ####               start_year = the year from which we want to do predictions.
        ####               nnet = if TRUE the nnetar() model is calculated.
        ####               arima = if TRUE the Arima() model is calculated.
        ####               autoarima = if TRUE the auto.arima() model is calculated.
        #### Note: the time series models parameters were adjusted in the class in a way they are suited for Las Vegas, business Cafe Yelp data. For different
        ####       situations the parameters HAVE to be changed or the class has to be extended. 
        slots = c(data="data.frame",start_year='numeric',nnet='logical',arima='logical',autoarima='logical'),
        
        
        #### Validity (optional, in this case it will do nothing since the class methid is checking whether its a character in the first place).
        validity=function(object)
        {
                if(!is.data.frame(object@data)) {
                        return("Object give is NOT a data.frame")
                }
                return(TRUE)
        }
)


### METHODS:

### --------------------------------------------------------------------------------------------------------------------------------------------------------
### The pred() method returns the list with structure described in this class introduction text (the text at the beginning).
### --------------------------------------------------------------------------------------------------------------------------------------------------------

setGeneric(name="pred",
           def=function(theObject,plot)
           {
                   standardGeneric("pred")
           }
)

setMethod(f="pred",
          signature="predic",
          definition=function(theObject)
          {  
                  #### Define the prediction methods you want: nnetar, Arima, auto.arima.
                  df<-theObject@data
                  rr<-theObject@data
                  #### Code.
                  if(theObject@nnet==TRUE){
                  #### Initial condition.
                  fits_nnetar<-lapply(c(1:(length(rr)-1)),function(x){temp<-as.vector(rr[,x]);fit<-nnetar(ts(temp[c(1:(length(temp)-1))],frequency=1,start=c(theObject@start_year)),size=18,repeats=50);fit})
                  for(i in c(1)){temp<-as.vector(rr[,i])
                                 pred<-forecast(fits_nnetar[[i]],h=1)
                                 pred<-as.data.frame(as.numeric(pred$mean[]))
                                 names(pred)<-names(rr)[i]
                                 acc<-accuracy(forecast(nnetar(temp[c(1:floor(0.9*length(temp)))],size=18,repeats=50),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                 a<-as.data.frame(rbind(acc[11],acc[12]))
                                 names(a)<-names(rr[i])
                                 ac<-rbind(pred,a)
                                 rownames(ac)<-c('nnetar_pred_2015','nnetar_acu_train','nnetar_acu_test')
                                 }
                  #### Accuracy calculations.
                  for(i in c(2:(length(rr)-1))){temp<-as.vector(rr[,i])
                                #fit<-nnetar(ts(temp[c(1:(length(temp)-1))],frequency=1,start=c(theObject@start_year)),size=18,repeats=50)
                                pred<-forecast(fits_nnetar[[i]],h=1)
                                pred<-as.data.frame(as.numeric(pred$mean[]))
                                names(pred)<-names(rr)[i]
                                acc<-accuracy(forecast(nnetar(temp[c(1:floor(0.9*length(temp)))],size=18,repeats=50),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                a<-as.data.frame(rbind(acc[11],acc[12]))
                                names(a)<-names(rr[i])
                                ac<-cbind(ac,rbind(pred,a))
                          }
                  year<-as.data.frame(c(2015,NA,NA))
                  names(year)<-c('year')
                  ac<-cbind(ac,year)
                  df<-rbind(df,ac)
                  fits<-list(fits_nnetar=fits_nnetar)
                  }
                  
                if(theObject@arima==TRUE){
                          #### Initial condition.
                        fits_arima<-lapply(c(1:(length(rr)-1)),function(x){temp<-as.vector(rr[,x]);fit<-Arima(ts(temp[c(1:(length(temp)-1))],frequency=1,start=c(theObject@start_year)),order=c(0,1,4));fit})
                          for(i in c(1)){temp<-ts(rr[,i],frequency=1,start=c(theObject@start_year))
                                         pred<-forecast(fits_arima[[i]],h=1)
                                         pred<-as.data.frame(as.numeric(pred$mean[]))
                                         names(pred)<-names(rr)[i]
                                         acc<-accuracy(forecast(Arima(temp[c(1:floor(0.9*length(temp)))],order=c(0,1,4)),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                         a<-as.data.frame(rbind(acc[11],acc[12]))
                                         names(a)<-names(rr[i])
                                         ac<-cbind(rbind(pred,a))
                                         rownames(ac)<-c('Arima_pred_2015','Arima_acu_train','Arima_acu_test')
                                         }
                          #### Accuracy calculations.
                          for(i in c(2:(length(rr)-1))){temp<-as.vector(rr[,i])
                                                        pred<-forecast(fits_arima[[i]],h=1)
                                                        pred<-as.data.frame(as.numeric(pred$mean[]))
                                                        names(pred)<-names(rr)[i]
                                                        acc<-accuracy(forecast(Arima(temp[c(1:floor(0.9*length(temp)))],order=c(0,1,4)),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                                        a<-as.data.frame(rbind(acc[11],acc[12]))
                                                        names(a)<-names(rr[i])
                                                        ac<-cbind(ac,rbind(pred,a))
                                                        
                                                        
                          }
                          #names(fits_arima)<-paste('arima_fits_',names(rr)[1:(length(rr)-1)],sep="")
                          fits<-list(fits,fits_arima=fits_arima)
                          year<-as.data.frame(c(2015,NA,NA))
                          names(year)<-c('year')
                          ac<-cbind(ac,year)
                          df<-rbind(df,ac)
                          }
                  
                
                if(theObject@autoarima==TRUE){
                        #### Initial condition.
                        fits_autoarima<-lapply(c(1:(length(rr)-1)),function(x){temp<-as.vector(rr[,x]);fit<-auto.arima(ts(temp[c(1:(length(temp)-1))],frequency=1,start=c(theObject@start_year)));fit})
                        for(i in c(1)){temp<-as.vector(rr[,i])
                                       
                                       pred<-forecast(fits_autoarima[[i]],h=1)
                                       pred<-as.data.frame(as.numeric(pred$mean[]))
                                       names(pred)<-names(rr)[i]
                                       acc<-accuracy(forecast(auto.arima(temp[c(1:floor(0.9*length(temp)))]),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                       a<-as.data.frame(rbind(acc[11],acc[12]))
                                       names(a)<-names(rr[i])
                                       ac<-cbind(rbind(pred,a))
                                       rownames(ac)<-c('auto.arima_pred_2015','auto.arima_acu_train','auto.arima_acu_test')}
                        #### Accuracy calculations.
                        for(i in c(2:(length(rr)-1))){temp<-as.vector(rr[,i])
                                                      fit<-auto.arima(temp[c(1:(length(temp)-1))])
                                                      pred<-forecast(fit,h=1)
                                                      pred<-as.data.frame(as.numeric(pred$mean[]))
                                                      names(pred)<-names(rr)[i]
                                                      acc<-accuracy(forecast(auto.arima(temp[c(1:floor(0.9*length(temp)))]),h=1),temp[c((floor(0.9*length(temp))+1):length(temp))])
                                                      a<-as.data.frame(rbind(acc[11],acc[12]))
                                                      names(a)<-names(rr[i])
                                                      ac<-cbind(ac,rbind(pred,a))
                                                      
                        }
                        year<-as.data.frame(c(2015,NA,NA))
                        names(year)<-c('year')
                        ac<-cbind(ac,year)
                        df<-rbind(df,ac)
                        fits<-list(fits,fits_autoarima=fits_autoarima)
                       }
                muro<-list(data_frame=df,fit=fits)
                return(muro)
          }
)

### --------------------------------------------------------------------------------------------------------------------------------------------------------


