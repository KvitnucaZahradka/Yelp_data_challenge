#THE MAIN SCRIPT

### First loading the libraries.

library("doParallel")
library("stringr")
library("dplyr")
library("plyr")
library("jsonlite")
library("tm")
library("wordcloud")
library("openNLP")
library("coreNLP")
library("syuzhet")
library("rJava")
library("koRpus")
library("lubridate")
library("forecast")
library("ggplot2")
library("grid")
library('hts')
library('caret')
library('zoo')
library('devtools')
library('ggfortify')
library('rlist')



## ONE SHOULD FIRST LOAD THE CLASSES: words, busS, evaluate, taB, predic from the same folder where the main script is located 

### Register two cores.
registerDoParallel(cores=2)

### Reading the Yelp data sets. And save them on local drive.

fnames<-c("business","checkin","review","tip","user")
jfile<-paste0(getwd(),"/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_",fnames,".json")
dat<-llply(as.list(jfile),function(x) stream_in(file(x),pagesize=1000))
saveRDS(dat,"data.rds")

fnames<-c("business")
jfile<-paste0(getwd(),"/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_",fnames,".json")
dat<-llply(as.list(jfile),function(x) stream_in(file(x),pagesize=1000))
saveRDS(as.data.frame(dat),"business_table.rds")
rm(dat)

### Reading the files from the local drive.

bus<-readRDS("business_table.rds")
us<-readRDS("user_table.rds")
rev<-readRDS("review_table.rds")
tip<-readRDS("tip_table.rds")
check<-readRDS("checkin_table.rds")

### The following gives almost all generalised cities, we want the city of Las Vegas.

t2<-as.data.frame(table(bus$city))
t3<-subset(t2,Freq>20)
nam<-as.character(t3$Var1)
x<-lapply(nam,function(x){grep(x,t2$Var1,ignore.case=T)})
y<-lapply(x,function(x){t2[x,]})
g<-rbind(as.data.frame(y[37]),as.data.frame(y[50]),as.data.frame(y[48]),as.data.frame(y[49]))
y[c(37,50,48,49)]<-NULL
for(i in c(1:length(y))){g<-rbind(g,as.data.frame(y[i]))}

### The point was that we noticed that Dollard-Des-Ormeaux was more than once the same data set.
### We removed it.

### We had a nice table of a raw generalised cities

### We just sub-set the Las Vegas city, since we wanted to examine closely the Las Vegas City.

las<-g$Var1[1:16]

### Using the busS class we can easily subset the desired business together with desired year and 
### obtain the reviews or by analogy tips for a given city in a given year.

df<-busS(data_rev=rev,data=bus,city=las_vegas,business="Cafes",year=2006)
df<-getWholeBusiness(df)

### We want to loop through years and looking for bad business features. For that we are using the custom made library busS and method evaluate()

set<-c(2006:2014)
SF<-sapply(set,function(x){cafe<-busS(data_rev=rev,data=bus,city=las,business="Cafes",year=as.numeric(x));
                           cafe<-getWholeBusiness(cafe);cafe<-subset(cafe,stars<=3);
                           cafe<-evaluate(text=cafe$text,filename=paste("cafe_rev_",x,collapse="",sep=""));cafe<-eval_summary(cafe)})

### We want find what are the words with the same meaning, we used custom library taB and function list_for_no_pick()

SF_rev<-taB(theSF=SF,low_cutof=0,years=c(2006:2014))
no_pick_list<-list_for_no_pick(SF_rev)

### we craeated the no_pick numeric vector by picking the words that did not have the same meaning:

no_pick<-c(1,7,13,14,20,21,22,32,39,40,49,59,75,83,87)

### We used the merge_same() method of the class taB to merge same word in the final table

final_table_rev<-merge_same(SF_rev)

### We create the the table with higher low frequency cutoff, now cutoff will be bigger or equal to 2.

final_table_rev<-very_final_table_cut_2(final_table_rev)

### ------------------------
### We did the same as above for the tip data set

df<-busS(data_rev=tip,data=bus,city=las_vegas,business="Cafes",year=2006)
df<-getWholeBusiness(df)

### We want to loop through years and looking for bad business features. For that we are using the custom made library busS and method evaluate()

set<-c(2006:2014)
SF<-sapply(set,function(x){cafe<-busS(data_rev=tip,data=bus,city=las,business="Cafes",year=as.numeric(x));
                           cafe<-getWholeBusiness(cafe);cafe<-subset(cafe,stars<=3);
                           cafe<-evaluate(text=cafe$text,filename=paste("cafe_tip_",x,collapse="",sep=""));cafe<-eval_summary(cafe)})

### We want find what are the words with the same meaning, we used custom library taB and function list_for_no_pick()

SF_tip<-taB(theSF=SF,low_cutof=0,years=c(2006:2014))
no_pick_list<-list_for_no_pick(SF_tip)

### we craeated the no_pick numeric vector (now for tips) by picking the words that did not have the same meaning:

no_pick<-c(2,3,6,7,19,24,27,34,38,40)

### We used the merge_same() method of the class taB to merge same word in the final table

final_table_tip<-merge_same(SF_tip)

### We create the the table with higher low frequency cutoff, now cutoff will be bigger or equal to 2.

final_table_tip<-very_final_table_cut_2(final_table_tip)

### We merge add the tables final_table_tip and final_table_rev together, because the cumulative table correctly represents the bad features

final_table_rev[which(rownames(final_table_rev) %in% rownames(final_table_tip)),which(colnames(final_table_rev) %in% colnames(final_table_tip))]<-final_table_rev[which(rownames(final_table_rev) %in% rownames(final_table_tip)),which(colnames(final_table_rev) %in% colnames(final_table_tip))]+final_table_tip[which(rownames(final_table_tip) %in% rownames(final_table_rev)),which(colnames(final_table_tip) %in% colnames(final_table_rev))]

table_final<-final_table_rev

## Forecasting and plotting

### For the forecasting we created the class predic, we want to turn on the nnetar() model together with Arima with order=c(0,1,4) and auto.arima().

pred_final<-predic(data=table_final,start_year=2006,nnet=T,arima=T,autoarima=T)

### The final predictions

pred_final_list<-pred(pred_final)
pred_final_list$data_frame

### The plotting can be made easy since we are using the ggplot2 extension by the ggfortify.


#### The auto.arima(), nnetar() and Arima() predictions for "experience". We are saving the resulting plots. 

p1<-autoplot(forecast(muro$fit$fits_autoarima[[10]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p1<-p1+xlab('years')+ylab(names(rr)[10])+ggtitle(paste('auto.arima prediction for the \"',names(rr)[10],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#arima prediction
p2<-autoplot(forecast(muro$fit[[1]]$fits_arima[[10]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p2<-p2+xlab('years')+ylab(names(rr)[10])+ggtitle(paste('Arima prediction for the \"',names(rr)[10],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#nnetar prediction
p3<-autoplot(forecast(muro$fit[[1]][[1]]$fits_nnetar[[10]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = F)
p3<-p3+xlab('years')+ylab(names(rr)[10])+ggtitle(paste('nnetar prediction for the \"',names(rr)[10],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))

png(paste(names(rr)[10],"predictions.pdf",sep=""),pointsize=15,width=1080,height=880) # Open a new png file
grid.arrange(p1, p2, p3, ncol=2) # Write the grid.arrange in the file
dev.off()

### The auto.arima(), nnetar() and Arima() predictions for "service". We are saving the resulting plots. 

p1<-autoplot(forecast(muro$fit$fits_autoarima[[27]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p1<-p1+xlab('years')+ylab(names(rr)[27])+ggtitle(paste('auto.arima prediction for the \"',names(rr)[27],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#arima prediction
p2<-autoplot(forecast(muro$fit[[1]]$fits_arima[[27]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p2<-p2+xlab('years')+ylab(names(rr)[27])+ggtitle(paste('Arima prediction for the \"',names(rr)[27],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#nnetar prediction
p3<-autoplot(forecast(muro$fit[[1]][[1]]$fits_nnetar[[27]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = F)
p3<-p3+xlab('years')+ylab(names(rr)[27])+ggtitle(paste('nnetar prediction for the \"',names(rr)[27],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))

png(paste(names(rr)[27],"predictions.pdf",sep=""),pointsize=15,width=1080,height=880) # Open a new png file
grid.arrange(p1, p2, p3, ncol=2) # Write the grid.arrange in the file
dev.off()

### The auto.arima(), nnetar() and Arima() predictions for "price". We are saving the resulting plots. 

p1<-autoplot(forecast(muro$fit$fits_autoarima[[21]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p1<-p1+xlab('years')+ylab(names(rr)[21])+ggtitle(paste('auto.arima prediction for the \"',names(rr)[21],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#arima prediction
p2<-autoplot(forecast(muro$fit[[1]]$fits_arima[[21]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p2<-p2+xlab('years')+ylab(names(rr)[21])+ggtitle(paste('Arima prediction for the \"',names(rr)[21],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#nnetar prediction
p3<-autoplot(forecast(muro$fit[[1]][[1]]$fits_nnetar[[21]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = F)
p3<-p3+xlab('years')+ylab(names(rr)[21])+ggtitle(paste('nnetar prediction for the \"',names(rr)[21],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
png(paste(names(rr)[21],"_predictions.png",sep=""),pointsize=15,width=1080,height=880) # Open a new png file
grid.arrange(p1, p2, p3, ncol=2) # Write the grid.arrange in the file
dev.off()

### The auto.arima(), nnetar() and Arima() predictions for "breakfast". We are saving the resulting plots. 

p1<-autoplot(forecast(muro$fit$fits_autoarima[[2]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p1<-p1+xlab('years')+ylab(names(rr)[2])+ggtitle(paste('auto.arima prediction for the \"',names(rr)[2],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#arima prediction
p2<-autoplot(forecast(muro$fit[[1]]$fits_arima[[2]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = T)
p2<-p2+xlab('years')+ylab(names(rr)[2])+ggtitle(paste('Arima prediction for the \"',names(rr)[2],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
#nnetar prediction
p3<-autoplot(forecast(muro$fit[[1]][[1]]$fits_nnetar[[2]],h=2),ts.colour = 'firebrick1', predict.colour = 'red',predict.linetype = 'dashed', conf.int = F)
p3<-p3+xlab('years')+ylab(names(rr)[2])+ggtitle(paste('nnetar prediction for the \"',names(rr)[2],"\"",sep=""))+theme( plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 1))
png(paste(names(rr)[2],"_predictions.png",sep=""),pointsize=15,width=1080,height=880) # Open a new png file
grid.arrange(p1, p2, p3, ncol=2) # Write the grid.arrange in the file
dev.off()

