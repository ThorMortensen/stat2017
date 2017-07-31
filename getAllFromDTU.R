## WARNING: THIS SCRIPTS OVERWRITES the files, therefore if you did notes in the pdfs etc. this will be lost!

## First set the working directory (run "?getwd" in the console to learn about it):
## In RStudio: open the script, take "Session->Set Working Directory->To Source File Location"
## and then replace the line below with the line from the console (this is a good trick for all scripts)
#setwd(getwd)

## The file names of the chapters
chapternames <- c("chapter1-DataVisualization",
                  "chapter2-ProbabilitySimulation",
                  "chapter3-StatisticsNormalAssumption",
                  "chapter4-StatisticsSimulationBased",
                  "chapter5-SimpleLinearRegression",
                  "chapter6-MultipleLinearRegression",
                  "chapter7-ProportionsFrequencyTables",
                  "chapter8-StatisticsMultigroupANOVA",
                  "chapterA-formulas")

## Function for downloading
download <- function(directory, files){
  ## First create the directory
  dir.create(paste0("introstat/",directory), showWarnings=FALSE, recursive=TRUE)
  ## Download the files to the directory
  for(i in 1:length(files)){
    print(paste("Now downloading file",i,"of",length(files)))
    tryCatch(download.file(url=paste0("http://www2.compute.dtu.dk/courses/introstat/",directory,"/",files[i]),
                           destfile=paste0("introstat/",directory,"/",files[i]),
                           quiet=TRUE,
                           mode='wb'),
             error=function(e){NA})
  }
}


## Download the book and chapters
download("book", c("book-IntroStatistics.pdf", paste0(chapternames,".pdf")))

## Download the chapter R scripts
download("book-scripts", paste0(chapternames,".R"))

## Download the chapter exercise solutions
file.remove(dir("introstat/book-solutions/","^chapter*",full.names=TRUE))
download("book-solutions", paste0("solutions-chapter",1:8,".pdf"))

## Download the 02323 slides
download("slides02323", paste0("week",1:13,".pdf"))
download("slides02323", paste0("week",1:13,"HA.pdf"))
download("slides02323", paste0("week",1:13,".R"))

## Download the 02402 slides
download("slides02402", paste0("week",1:13,".pdf"))
download("slides02402", paste0("week",1:13,"HA.pdf"))
download("slides02402", paste0("week",1:13,".R"))

## Download the 02402 Danish slides
download("slides02402da", paste0("uge",1:13,".pdf"))
download("slides02402da", paste0("uge",1:12,".R"))
download("slides02402da", paste0("uge",1:13,"HA.pdf"))


## Upload this script
## source("functions/introstatUploadScript.R")
## introstatUploadScript("downloadMaterial.R","")