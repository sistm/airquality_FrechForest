# Dependencies : stringr, randomForest, FrechForest, ggplot2 (this last one is
# optional and only used to get graphical illustrations of the results)

# Data load

air <- read.csv2("AirQualityUCI.csv", header = TRUE)
air <- subset(air, select = c(
  Date, Time, CO.GT., C6H6.GT., NOx.GT., NO2.GT., T, RH, AH))

# Data management

## Recode "-200" by NA
Date <- air$Date
heure <- air$Time

Mat <- as.matrix(air[,-c(1,2)])
Mat[which(Mat==-200)] <- NA
air[,-c(1,2)] <- Mat

## Find and remove curve with more than 10 missing values

new_date <- rep(NA,nrow(air))
retire <- NULL
for (i in 1:length(unique(air$Date))){
  w <- which(air$Date==unique(air$Date)[i])
  new_date[w] <- i
  vide <- FALSE 
  for (k in 3:ncol(air)){
    if (sum(is.na(air[w,k]))>10){
      vide <- TRUE
    }
  }
  if (vide==TRUE) retire <- c(retire,i)
}

W = NULL 
for (i in retire){
  W <- c(W, which(new_date==i))
}

air$Date <- new_date
air <- air[-W,]

air$Time <- as.numeric(stringr::str_sub(as.character(air$Time), 1, 2))

## Remove 6 first observations because day 1 is incomplete

X <- air[-(1:6), ]

## Split data: in x first 12 hours for inputs
## and in y last 12 hours for the output

x <- NULL 
y <- NULL

id_y <- NULL 
time_y <- NULL 
for (i in unique(X$Date)){
  w_x <- which(X$Date==i & X$Time<=11)
  w_y <- which(X$Date==i & X$Time>11)
  
  if (length(w_x)==length(w_y)){
    x <- rbind(x,X[w_x,])
    w_y <- which(X$Date==i & X$Time>11)
    y <- c(y,X$CO.GT.[w_y])
    id_y <- c(id_y, X$Date[w_y])
    time_y <- c(time_y, X$Time[w_y])
  }
}

## Build Curve object and Y object for FrechForest use

airCurves <- list(type = "curve", X = x[, -c(1, 2, 3)], time = x$Time, id = x$Date)
airY <- list(type = "curve", Y = y, time = time_y, id = id_y)


# RF

## Build data frame that ignore the curve structure (all observations are seen
## as independent)
airDF <- airCurves$X
airDF$Y <- airY$Y
airDF <- na.omit(airDF)

## RF tuning

library(randomForest)
mtryValues <- 1:ncol(airCurves$X)

# ncores <- 32
# set.seed(749611, kind = "L'Ecuyer-CMRG")
# oobErrorsMtryTuneCO <- simplify2array(lapply(mtryValues, function(mval) {
#   repErr <- simplify2array(parallel::mclapply(1:25, function(i) {
#     rf <- randomForest(Y ~ ., data = airDF, mtry = mval)
#     err <- rf$mse[500]
#     return(err)
#   }, mc.cores = ncores))
#   return(repErr)
# }))
# save(oobErrorsMtryTuneCO, file = "oobErrorsMtryTuneCO.Rdata")

load("oobErrorsMtryTuneCO.Rdata")
boxplot(oobErrorsMtryTuneCO, xlab = "mtry", ylab = "OOB error")
mtryOpt <- mtryValues[which.min(colMeans(oobErrorsMtryTuneCO))]

## Optimal RF run

set.seed(421223, kind = "L'Ecuyer-CMRG")
rfOpt <- randomForest(Y ~ ., data = airDF, mtry = mtryOpt, importance = TRUE)
rfOpt
# varImpPlot(rfOpt, type = 1, scale = FALSE)

## RF partial plots for interpretation

# par(mfrow = c(2, 2))
# partialPlot(rfOpt, pred.data = airDF, x.var = "NOx.GT.")
# partialPlot(rfOpt, pred.data = airDF, x.var = "NO2.GT.")
# partialPlot(rfOpt, pred.data = airDF, x.var = "AH")
# partialPlot(rfOpt, pred.data = airDF, x.var = "RH")
# par(mfrow = c(1, 1))


# FRF
library(FrechForest)

## FRF tuning

mtryValues <- 1:ncol(airCurves$X)

# ncores <- 32
# set.seed(941949, kind = "L'Ecuyer-CMRG")
# CurveOOBErrorsMtryTuneCO <- simplify2array(lapply(mtryValues, function(mval) {
#   FRF <- FrechForest(Curve = airCurves, Y = airY, ERT = FALSE, ntree = 250,
#                      mtry = mval, ncores = ncores, importance = FALSE)
#   CurveOOBErr <- mean(FRF$oob.err)
#   return(CurveOOBErr)
# }))
# save(CurveOOBErrorsMtryTuneCO, file = "CurveOOBErrorsMtryTuneCO.Rdata")

load("CurveOOBErrorsMtryTuneCO.Rdata")
plot(mtryValues, CurveOOBErrorsMtryTuneCO, type = "b")
CurveMtryOpt <- mtryValues[which.min(CurveOOBErrorsMtryTuneCO)]

## Optimal FRF run

# ncores <- 32
# set.seed(503579, kind = "L'Ecuyer-CMRG")
# FRFairqualCO <- FrechForest(Curve = airCurves, Y = airY, ERT = FALSE,
#   ntree = 500, mtry = 2, ncores = ncores, importance = TRUE)
# save(FRFairqualCO, file = "FRFairqualCO.Rdata")

load("FRFairqualCO.Rdata")
varImp <- sort(FRFairqualCO$varImp$Curve, index.return = TRUE)
# barplot(varImp$x, names.arg = colnames(airCurves$X)[varImp$ix], horiz = TRUE)

## FRF point-by-point OOB error computation

dfY <- data.frame(time = airY$time, Y = airY$Y, id = airY$id)
dfY <- transform(dfY, id = factor(id))
dfYwPred <- cbind(dfY, pred = do.call(rbind, FRFairqualCO$oob.pred)$y)
errP2P <- (dfYwPred$Y - dfYwPred$pred)^2
summary(errP2P)

## FRF predictions visualisation

# library(ggplot2)
# predPlot <- function(df) {
#   ggplot(df, aes(x = time, y = Y)) +
#     geom_line() +
#     geom_line(aes(x = time, y = pred), color = "red") +
#     labs(x = "Time") +
#     labs(y = "Gaz") +
#     theme_bw() +
#     facet_wrap(.~id)
# }
# subInd <- sample(unique(Y$id), 25)
# subdfYwPred <- subset(dfYwPred, id %in% subInd)
# predPlot(subdfYwPred)


# Comparison RF vs FRF on several runs

# ncores <- 32
# set.seed(468902, kind = "L'Ecuyer-CMRG")
# allErrP2P_CO <- sapply(1:20, function(i) {
#   rf <- randomForest(Y ~ ., data = airDF, mtry = mtryOpt)
#   errRF <- rf$mse[500]
#   FRF <- FrechForest(Curve = airCurves, Y = airY, ERT = FALSE, ntree = 250,
#     mtry = CurveMtryOpt, ncores = ncores, importance = FALSE)
#   dfYwPred <- cbind(dfY, pred = do.call(rbind, FRF$oob.pred)$y)
#   errFRF <- mean((dfYwPred$Y - dfYwPred$pred)^2, na.rm = TRUE)
#   print(paste0("iteration", i, "done"))
#   return(c(errFRF, errRF))
# })
# save(allErrP2P_CO, file = "allErrP2P_CO.Rdata")

load("allErrP2P_CO.Rdata")
allErrP2P_CO <- data.frame(t(allErrP2P_CO))
names(allErrP2P_CO) <- c("FRF", "RF")
summary(allErrP2P_CO)
boxplot(allErrP2P_CO)


## Comparing variable importance scores

dfImp <- data.frame(imp = c(rfOpt$importance[, "%IncMSE"], FRFairqualCO$varImp$Curve),
                    type = c(rep("RF", 6), rep("FRF", 6)))
dfImp$var <- rep(c("C6H6", "Nox", "NO2", "Temp", "RH", "AH"), 2)
str(dfImp)
# library(ggplot2)
# plotImp <- ggplot(data = dfImp, aes(x = var, y = imp)) +
#   geom_col() +  theme_bw() + facet_wrap(~type) +
#   xlab("Variables") + ylab("Variable importance")
#   # scale_fill_manual(values=viridis::viridis(3)[c(1,2)])
# plotImp
# dev.print(device = pdf, file = "varImpComp.pdf", width = 5, height = 3)
