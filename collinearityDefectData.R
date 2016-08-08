### Detect collinearity in defect dataset

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import packages
# These two lines are used to download DefectData library
#library(devtools)
#install_github("klainfo/DefectData")
library(DefectData)
library(Hmisc)
library(compiler)

enableJIT(3)

## Global variables
failVarclus <- c()
failRMSVIF <- c()
datasetInfo <- c()

## Define functions
.writeLine <- function(arg1,arg2) {
  write(paste0(arg2,collapse = ","),file = arg1,append = TRUE)
}
writeLine <- cmpfun(.writeLine)

.exportDataDist <- function(dataset,dataHeader) {
  # plot dataset distribution
  print("Plot and export dataDist")
  
  png(
    height = 6000, width = 6000, pointsize = 15, file = paste0("./output/datasetDist/",dataHeader,"_dataDist.png")
  )
  pairs(dataset, upper.panel = panel.cor)
  dev.off()
}
exportDataDist <- cmpfun(.exportDataDist)

.corrGraph <- function(dataset,refOrder,dataHeader) {
  vc <- varclus( ~ .,
                 data = dataset[refOrder],
                 similarity = "spearman",
                 trans = "abs")
  png(
    height = 1080, width = 1080, pointsize = 15, file = paste0("./output/varclus/",dataHeader,"_varclus.png")
  )
  
  # TRUE if there are variables that have more than 0.7 similarity
  datasetInfo <<-
    c(datasetInfo,Reduce("|",(1 - vc$hclust$height) >= 0.7))
  
  plot(vc)
  abline(h = 0.3) # threashold 0.7 on graph
  dev.off()
  
}
corrGraph <- cmpfun(.corrGraph)

.getVIF <- function(dataset,refOrder,depVar,dataHeader,dataID) {
  model <- glm(as.formula(paste(
    depVar, "~", paste(refOrder, collapse = '+')
  )),
  data = dataset,
  family = binomial())
  
  tryCatch({
    # Get vif score for each metric
    model.vif <- rms::vif(model)[refOrder]
    
    # Export in long format
    for (index in 1:length(refOrder)) {
      writeLine("./output/VIF.csv",
                paste0(
                  c(dataID,
                    dataHeader,
                    refOrder[index],
                    model.vif[index],
                    collapse = ",")
                ))
    }
    
    # Check if there are any variables that have vif value more than 5 and 10 (Collinearity)
    datasetInfo5 <- Reduce("|",model.vif >= 5)
    datasetInfo10 <<- Reduce("|",model.vif >= 10)
    writeLine("./output/VIFSummarized.csv",
              paste0(
                c(dataID,
                  dataHeader,
                  datasetInfo5,
                  datasetInfo10,
                  collapse = ",")
              ))
  },
  error = function(e) {
    print(paste0("Fail to export rms::VIF on ",dataHeader))
    failRMSVIF <<- c(failRMSVIF,dataID)
    writeLine("./output/VIF.csv",
              paste0(
                c(dataID,
                  dataHeader,
                  '-',
                  'FAILED',
                  collapse = ",")
              ))
    writeLine("./output/VIFSummarized.csv",
              paste0(
                c(dataID,
                  dataHeader,
                  'FAILED',
                  'FAILED',
                  collapse = ",")
              ))
  })
}
getVIF <- cmpfun(.getVIF)

# Take from http://www.r-bloggers.com/scatter-plot-matrices-in-r/
.panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if (p < 0.01)
    txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}
panel.cor <- cmpfun(.panel.cor)


## Main

# Create output folder
dir.create(file.path(paste0(getwd(), '/output/')), showWarnings = FALSE)
dir.create(file.path(paste0(getwd(), '/output/varclus/')), showWarnings = FALSE)
dir.create(file.path(paste0(getwd(), '/output/datasetDist/')), showWarnings = FALSE)
writeLine("./defectData_summarize.csv",paste0(
  c("DataId","Name","Varclus-0.7","VIF(rms)-5","VIF(rms)-10"),collapse = ","
))

# dataId, dataHeader, metric, vif score
writeLine("./output/VIF.csv",
          paste0(
            c("datasetID",
              "datasetName",
              "Metric",
              "VIF"),
            collapse =","))
writeLine("./output/VIFSummarized.csv",
          paste0(
            c("datasetID",
              "datasetName",
              "VIF-threshold5",
              "VIF-threshold10"),
            collapse =","))
failDD <- c()
for (targetProjectId in 1:nrow(listData)) {
  print(paste('ProjectID:', targetProjectId,"START"))
  datasetInfo <<- c(targetProjectId)
  
  targetData <- as.character(listData[targetProjectId, 1])
  datasetInfo <<- c(datasetInfo,targetData)
  
  # Load data
  Data <- loadData(targetData)
  
  # retrieve data, name of dependent and independent variables
  dataset <- Data$data
  dep <- Data$dep
  indep <- Data$indep
  
  # Export dataDist of dataset
  #   tryCatch({
  #     exportDataDist(dataset[indep],targetData)
  #   },error = function(e){
  #     failDD <<- c(failDD,targetProjectId)
  #   })
  
  # convert fron logical -> factor variable type
  dataset <- dataset[c(indep, dep)]
  dataset[dep] <-
    lapply(dataset[dep], function(x)
      factor(ifelse(x, "true", "false")))
  #
  #   print("Plot and export varclus")
  #   tryCatch(
  #     {
  #       corrGraph(dataset,indep,targetData)
  #     },
  #     error = function(e){
  #       print(paste0("Fail to export correlation graph(varclus) on ",targetData))
  #       failVarclus <<- c(failVarclus,targetProjectId)
  #       datasetInfo <<- c(datasetInfo,"FAIL")
  #       })
  
  print("Build model and compute VIF")
  getVIF(dataset,indep,dep,targetData,targetProjectId)
  
  writeLine("./defectData_summarize.csv",datasetInfo)
  
  print(paste('ProjectID:', targetProjectId,"DONE"))
}

print(paste0("Fail to varclus(",length(failVarclus),") projectID"))
print(failVarclus)
print(paste0("Fail to car::VIF(",length(failCarVIF),") projectID"))
print(failCarVIF)
print(paste0("Fail to rms::VIF(",length(failRMSVIF),") projectID"))
print(failRMSVIF)
print(paste0("Fail to DD",length(failDD),") projectID"))
print(failDD)
