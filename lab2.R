

rm(list=ls())

library(dplyr)
library(gtools)
library(tidyr)
library(car)

path<-setwd("~/Documents/LAB2")
filelist <- list.files(path=path, pattern="*.txt")
filelist

for (filename in filelist){
  if (!exists("dataset")){dataset <- read.table(file = filename, header=TRUE, fill = TRUE, sep="\t")
  }
  if (exists("dataset")){
    dataset1 <-read.table(file = filename, header=TRUE, fill = TRUE, sep="\t")
    dataset<-smartbind(dataset, dataset1, fill = NA)
    #dataset<-rbind.data.frame(dataset, dataset1)
    rm(dataset1)
  }
}

dataset$Inlanad.Utlanad <- NULL
dataset$Kvartalsslut[dataset$Kvartalsslut==""] <- NA
save(dataset, file = "dataset.rdata")
load("dataset.rdata")

dataset1<- dataset %>% fill(Kvartalsslut)
save(dataset1, file = "dataset1.rdata")
load("dataset1.rdata")

sweden_market <- c()
sweden_market <- subset.data.frame(dataset1, grepl("Nordea", dataset1$Instrumentnamn) & dataset1$Land=="SE", select = c("Kvartalsslut", "Marknadsvarde"))
summary(sweden_market$Kvartalsslut)
n <- length(levels(sweden_market$Kvartalsslut))-1
market_value <- c()

for (i in 1:n){
  arg1 <- sweden_market$Kvartalsslut == levels(sweden_market$Kvartalsslut)[i+1]
  values <- as.numeric(sub(',','.', sweden_market$Marknadsvarde[arg1]))
  total_value <- sum(values)
  market_value <- c(market_value, total_value)
}
sweden_percent <- subset.data.frame(dataset1, Posttyp =="info" | Land == "SE", select = c("Kvartalsslut", "Marknadsvarde_tot", "Marknadsvarde"))
sweden_percent[sweden_percent==""] <- NA
sweden_percent[is.na(sweden_percent)] = 0
n <- length(levels(sweden_percent$Kvartalsslut))-1
swe_percent <- c()

for (i in 1:n){
  arg2 <- sweden_percent$Kvartalsslut == levels(sweden_market$Kvartalsslut)[i+1]
  total_marketvalue <- as.numeric(sub(',','.', sweden_percent$Marknadsvarde_tot[arg2]))
  sc_marketvalue <- as.numeric(sub(',','.', sweden_percent$Marknadsvarde[arg2]))
  percent <- sum(sc_marketvalue)/sum(total_marketvalue)
  swe_percent <- c(swe_percent, percent)
}

##Graph Plot 
image_name <- paste0("Nordea_Nordenfond.jpeg")
jpeg(filename = image_name)
plot(market_value ~ as.Date(levels(sweden_market$Kvartalsslut)[2:41]), type="l", lwd=3, col="purple", 
     xlab="Years",ylab="Market Value", main= "Nordea Nordenfond")
par(new = TRUE)
plot(swe_percent ~ as.Date(levels(sweden_percent$Kvartalsslut)[2:41]), type="l", lwd=3, col="green", 
     axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext("Swedish Companies %", side = 4)
legend("topleft", col = c("purple","green"), lty = 1, lwd=3, legend = c("Value","Swedish %"))
dev.off()
