### Detect collinearity in defect dataset

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import packages 
# These two lines are used to download DefectData library
#library(devtools)
#install_github("klainfo/DefectData")
library(DefectData)

## Define functions
writeLine <- function(arg1,arg2){
  write(paste0(arg2,collapse=","),file=arg1,append=TRUE)  
}

corrGraph <- function(dataset,refOrder,dataHeader){
  
  vc <- varclus(~.,
                data=dataset[refOrder],
                similarity = "spearman",
                trans="abs")
  png(height=1080, width=1080, pointsize=15, file=paste0("./output/varclus/",dataHeader,"_varclus.png"))
  plot(vc)
  abline(h=0.3) # threashold 0.7 on graph
  dev.off()
}

getVIF <- function(dataset,refOrder,depVar,dataHeader){
  model <- glm(as.formula(paste(
    depVar, "~", paste(refOrder, collapse = '+')
  )),
  data = dataset,
  family = binomial())
  model.vif <- vif(model)[refOrder]
  # Header, n(HighlyCorrelatedVar) Text,Indep
  # Header, n(HighlyCorrelatedVar),VIF
  writeLine("./output/VIF.csv",paste0(c(dataHeader,"n(highCol)",names(model.vif)),collapse=","))
  writeLine("./output/VIF.csv",paste0(c(dataHeader,length(model.vif[model.vif>5]),model.vif),collapse=","))
}

## Main

# Create output folder 
dir.create(file.path(paste0(getwd(), '/output/')), showWarnings = FALSE)
dir.create(file.path(paste0(getwd(), '/output/varclus/')), showWarnings = FALSE)


for(targetProjectId in 1:nrow(listData)) {

  print(paste('Start ProjectID:', targetProjectId))
  
  targetData <- as.character(listData[targetProjectId, 1])
  
  ## Load data
  Data <- loadData(targetData)
  # retrieve data, name of dependent and independent variables
  dataset <- Data$data
  dep <- Data$dep
  indep <- Data$indep
  # convert fron logical -> factor variable type
  dataset <- dataset[c(indep, dep)]
  dataset[dep] <-
    lapply(dataset[dep], function(x)
      factor(ifelse(x, "true", "false")))
  
  corrGraph(dataset,indep,targetData)
  
  getVIF(dataset,indep,dep,targetData)
}
