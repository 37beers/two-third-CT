#!/usr/bin/env Rscript

#TODO: Please always use a header. To make a function executable, add the Rscript above. 


#TODO: This is a good package for making it commandline. See what happens when you chmod +x twothirds.R
# then run... /path/twothirds.R -f TCR.txt

suppressPackageStartupMessages(library("argparse"))
parser <- ArgumentParser()
parser$add_argument("-f", "--file", action="store", dest="file",
    help="file containing raw qPCR data to be read")
args <- parser$parse_args()

#TODO: avoid setwd. Always just spell out the full input path
#setwd("/Users/abeerobaid/desktop")
#data <- read.csv('/Users/abeerobaid/desktop/TCR.txt', header= FALSE)

#TODO: but using the commandline parsing might be easier. Also read.csv is not going to work bc .txt file
data <- read.csv(args$file, header=FALSE)

#TODO: I'm not sure what this meant to do...
#data

#TODO: You're not reading in the right part of the file here. Below is not a good strategy. Try combining this code with a little bit of standard bash. 

#TODO: grep on a data frame is a strange combination. Also you want this for all wells of interest right?
x <- grep("A1\t", data[,1])

#the first thing it finds is useless, so remove it 
x <- x[-c(1,2)]

CT <- function(a) {
  cycle = c()
  blue = c()
  for (i in a) {   
  well <- data[i,]
  well <- unlist(strsplit(as.character(well), "\t"))
  cycle <- c(cycle, well[2])
  blue <- c( blue, as.numeric(well[3]) )
  }
  return(data.frame(as.numeric(cycle),as.numeric(blue)))
}

#Store the dataframe into a variable, and find the 2/3 fluorescent value
frame <- CT(x)
two.thirds <- (2*frame[which.max(frame[,2]),2]-frame[which.min(frame[,2]),2])/3

#TODO: plotting is not necessary

#ploting and finding the CT value 
plot(frame)
fit4 <- lm(frame[,2]~poly(frame[,1],12,raw=TRUE))
lines(frame[,1], predict(fit4, data.frame(x=frame[,2]), col="purple"))
approx(frame[,2],frame[,1],xout=two.thirds)
