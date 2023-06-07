#' Coefficient of Dispersion
#'
#' Measurement of horizontal equity. Where horizontal equity is the deviation
#' of like properties from one another.
#' @param x Appraisal value
#' @param y Sales price
#' @return COD
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' codvalue <- COD(x,y);
#' @export
COD <- function(x,y){ #Cod
  ratio <- x / y
  med <- median(ratio, na.rm=TRUE)
  dev<- sum(abs(ratio-med))
  avgdev <- dev / NROW(ratio)
  return(100*(avgdev / med))
}

#' Price Related Differential
#'
#' Measurement of vertical equity. Where vertical equity is the mean divided mby the
#' weighted mean, which gives equity performance of appraisal to sales ratios of lower
#' to higher valued properties.
#' @param x Appraisal value
#' @param y Sales price
#' @return PRD
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' prdvalue <- PRD(x,y);
#' @export
PRD <- function(x,y){ #prd

  ratio <- x/ y

  mnratio <- mean(ratio , na.rm= TRUE)

  mnx <- mean(x , na.rm = TRUE)

  mny <- mean(y, na.rm = TRUE)

  prd <- mnratio /(mnx / mny)

  return(prd)



}

#' Weighted Mean
#'
#' Returns the weighted mean
#' @param x Appraisal value
#' @param y Sales price
#' @return weighted mean
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' wmvalue <- Weighted_Mean(x,y);
#' @export
Weighted_Mean <- function(x,y){ #Weighted_Mean
  return(mean(x,na.rm =TRUE) / mean(y, na.rm = TRUE))

}

#' Weighted Median
#'
#' Returns the weighted median, used by certain bodies for appraisal. The
#' mathematical utility of such an equation is suspect.
#' @param x Appraisal value
#' @param y Sales price
#' @return weighted median
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' wmedianvalue <- Weighted_Median(x,y);
#' @export
Weighted_Median <- function(x,y){ #Weighted_Median
  return(median(x,na.rm =TRUE) / median(y, na.rm = TRUE))

}

#' Price Related Bias
#'
#' Measurement of the linear relationship of the appraisal value to the sales price.
#' Developed by Bob.
#' @param x Appraisal value
#' @param y Sales price
#' @return PRB
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' prbvalue <- PRB(x,y);
#' @export
PRB <- function (x,y){ #prb
  if(NROW(unique(data.frame(x,y))) <= 2 ){
    return( NA)
  }
  else {
    asr <- x / y
    med <- median( asr ,na.rm= TRUE)
    avmed <- x / med
    value <- .5 * y + .5 * avmed
    ind <- log(value) / log(2)
    dep <- (asr -med) / med
    l<-lm(dep ~ ind)
    if(summary(l)$coefficients[2,4] < .1 ){
      return(summary(l)$coefficients[2,1])
    }
    else{
      return(0)
    }
  }
}


#' Price Related Bias
#'
#' Determines if the PRB is within an acceptable range.
#' @param x Appraisal value
#' @param y Sales price
#' @return PRB
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' prb_pf <- PRB_Conclusion(x,y);
#' @export
PRB_Conclusion <- function (x,y){ #PRB_Conclusion
  if(NROW(unique(data.frame(x,y))) <= 2 ){
    return( NA)
  }
  else {
    asr <- x / y
    med <- median( asr ,na.rm= TRUE)
    avmed <- x / med
    value <- .5 * y + .5 * avmed
    ind <- log(value) / log(2)
    dep <- (asr -med) / med
    l<-lm(dep ~ ind)
    if(summary(l)$coefficients[2,4] > .1  | (summary(l)$coefficients[2,4] < .1 & abs(summary(l)$coefficients[2,1]) < .05) ){
      return('PASS')
    }
    else{
      return('FAIL')
    }
  }
}

#' Price Related Bias
#'
#' calculates the bottom confidence interval.
#' @param x Appraisal value
#' @param y Sales price
#' @return PRB Lower
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' prbl <- PRB_Lower(x,y);
#' @export
PRB_Lower <- function (x,y){ #PRB_Lower
  if(NROW(unique(data.frame(x,y))) <= 2 ){
    return( NA)
  }
  else {
    asr <- x / y
    med <- median( asr ,na.rm= TRUE)
    avmed <- x / med
    value <- .5 * y + .5 * avmed
    ind <- log(value) / log(2)
    dep <- (asr -med) / med
    l<-lm(dep ~ ind)
    if(summary(l)$coefficients[2,4] < .1 ){
      return(confint(l)[2,1])
    }
    else{
      return(0)
    }
  }
}

#' Price Related Bias
#'
#' calculates the bottom confidence interval.
#' @param x Appraisal value
#' @param y Sales price
#' @return PRB Upper
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' prbu <- PRB_Upper(x,y);
#' @export
PRB_Upper <- function (x,y){ #PRB_Upper
  if(NROW(unique(data.frame(x,y))) <= 2 ){
    return( NA)
  }
  else {
    asr <- x / y
    med <- median( asr ,na.rm= TRUE)
    avmed <- x / med
    value <- .5 * y + .5 * avmed
    ind <- log(value) / log(2)
    dep <- (asr -med) / med
    l<-lm(dep ~ ind)
    if(summary(l)$coefficients[2,4] < .1 ){
      return(confint(l)[2,2])
    }
    else{
      return(0)
    }
  }
}

#' REAL Function Summary
#'
#' returns a console output of all the appraisal calculations
#' @param x Appraisal value
#' @param y Sales price
#' @param trim YES is default. Enables ratio trimming
#' @param ul Upper limit for ratios. 1.7 is the default value
#' @param ll Lower limit for ratios. .3 is the default value
#' @param year If years are included, runs summary analysis broken up by years
#' @param df Default is FALSE. Run data.frame
#' @return summary results
#' @examples
#' x <- c(25500,57000,39000,90000,51000,93000,49500);
#' y <- c(75000,150000,90000,180000,90000,150000,75000);
#' real_sum <- REALsummary(x,y);
#' @export
REALsummary <- function( a,s,trim = "YES",ul =1.7 ,ll =.3 ,year,df= FALSE){ #summarystats
  if(missing(year) ){
    col <- data.frame(a,s)
    col$ratio <- a/s
    if (trim == "YES") {
      col <- subset(col, ratio > ll & ratio < ul)
    } else if (trim == "NO") {
      col <- col
    }
    #col <- subset(col, ratio > ll & ratio < ul)
    a <- col$a
    s<- col$s
    print(paste(' Sample size: ', NROW(a),
                'Weighted Mean:',round(Weighted_Mean(a,s),3),
                'Median:',round(median(round(a / s, 4), na.rm = TRUE),3),
                'Weighted Median:',round(Weighted_Median(a,s),3),
                '   COD:', round(COD(a,s),3),
                '   PRD:', round(PRD(a,s),3),
                '   PRB:', round(PRB(a,s),3),
                '   PRB Result:', PRB_Conclusion(a,s),
                '   PRB Lower:', round(PRB_Lower(a,s),3),
                '   PRB Upper:', round(PRB_Upper(a,s),3)
    ))
  }
  else if( df == FALSE) {
    col <- data.frame(a,s,year)
    col$ratio <- a/s
    if (trim == "YES") {
      col <- subset(col, ratio > ll & ratio < ul)
    } else if (trim == "NO") {
      col <- col
    }
    #col <- subset(col, ratio > ll & ratio < ul)
    a <- col$a
    s<- col$s
    year <- col$year
    dat <- data.frame(a,s,year)
    for(i in unique(year)){
      dat1 <- subset(dat, year == i)
      print(paste('Group',i,
                  ' Sample size: ', NROW(a),
                  '  Weighted Mean:',round(Weighted_Mean(dat1$a,dat1$s),3),
                  '  Median:',round(median( round(dat1$a / dat1$s,4), na.rm = TRUE),3),
                  '  Weighted Median:',round(Weighted_Median(dat1$a,dat1$s),3),
                  '   COD:', round(COD(dat1$a,dat1$s),3),
                  '  PRD:', round(PRD(dat1$a,dat1$s),3),
                  '  PRB:', round(PRB(dat1$a,dat1$s),3),
                  '  PRB Result:', PRB_Conclusion(dat1$a,dat1$s),
                  '   PRB Lower:', round(PRB_Lower(dat1$a,dat1$s),3),
                  '   PRB Upper:', round(PRB_Upper(dat1$a,dat1$s),3)


      ))

    }
  }
  else{
    col <- data.frame(a,s,year)
    col$ratio <- a/s
    if (trim == "YES") {
      col <- subset(col, ratio > ll & ratio < ul)
    } else if (trim == "NO") {
      col <- col
    }
    #col <- subset(col, ratio > ll & ratio < ul)
    a <- col$a
    s<- col$s
    year <- col$year
    dat <- data.frame(a,s,year)
    resultStatistics <- data.frame("Group" = character() ,
                                   "Count" = integer(),
                                   "Weighted_Mean" = double(),
                                   "Median" = double(),
                                   "Weighted_Median" = double(),
                                   "COD" = double(),
                                   "PRD" = double() ,
                                   "PRB" = double() ,
                                   "PRBConc" = character(),
                                   "PRB_Lower" =double(),
                                   "PRB_Upper" = double(),
                                   stringsAsFactors =  FALSE)
    for(i in unique(year)){
      dat1 <- subset(dat, year == i)
      print(paste('Group',i,
                  '  Weighted Mean:',round(Weighted_Mean(dat1$a,dat1$s),3),
                  '  Median:',round(median(round(dat1$a / dat1$s ,4 ), na.rm = TRUE),3),
                  '  Weighted Median:',round(Weighted_Median(dat1$a,dat1$s),3),
                  '   Count:', NROW(dat1),
                  '   COD:', round(COD(dat1$a,dat1$s),3),
                  '  PRD:', round(PRD(dat1$a,dat1$s),3),
                  '  PRB:', round(PRB(dat1$a,dat1$s),3),
                  '  PRB Result:', PRB_Conclusion(dat1$a,dat1$s),
                  '   PRB Lower:', round(PRB_Lower(dat1$a,dat1$s),3),
                  '   PRB Upper:', round(PRB_Upper(dat1$a,dat1$s),3)
      ))
      resultStatistics[NROW(resultStatistics)+1,] <- list(i,
                                                          NROW(dat1),
                                                          round(Weighted_Mean(dat1$a,dat1$s),3),
                                                          round(median( round(dat1$a / dat1$s,4)  , na.rm = TRUE),3),
                                                          round(Weighted_Median(dat1$a,dat1$s),3),
                                                          round(COD(dat1$a,dat1$s),3),
                                                          round(PRD(dat1$a,dat1$s),3),
                                                          round(PRB(dat1$a,dat1$s),3),
                                                          PRB_Conclusion(dat1$a,dat1$s),
                                                          round(PRB_Lower(dat1$a,dat1$s),3),
                                                          round(PRB_Upper(dat1$a,dat1$s),3)
      )
    }
    cat("NOTE : The results have been saved to the data frame")
    return(resultStatistics)

  }
}


#' Polynomial Time Trend Fit
#'
#' Calculates a polynomial fit based on the regression results of the relationship
#' of the sales price to time. Input must be a data.frame with the first column being sales
#' and the second being time i,e, months. Time must have a origin at 0.
#' @param x Appraisal value
#' @param y Sales price
#' @param df data.frame containing sales price and time
#' @return rate table and a plot of the time trend fit
#' @examples
#' x <- c(10000,12000,11000,44000,35000,38000,89000,94000,102000,120000,125000,132000
#' ,135000,137000,142000,145000,147000,149000);
#' y <- c(5,5,5,4,4,4,3,3,3,2,2,2,1,1,1,0,0,0);
#' df <- data.frame(salesprice = x,months = y)
#' poly_fit <- poly.trend(df);
#' @export
poly.trend <- function(data1){ #function start

  modelData <- data.frame(
    'y' = data1[,1]
    # ,'const' = 1 #const not needed in R, R creates const by default
    ,'TimeToSale' = data1[,2]

    , stringsAsFactors = FALSE)

  m <- lm(y ~ ., data = modelData)
  bestTimeModel <- m
  for(dg in 2:length(unique(modelData$TimeToSale)) ){


    modelData[ , paste0("TimeToSale", dg)]   <- (modelData$TimeToSale)^dg
    m <- lm(y ~ ., data = modelData)

    if(BIC(m) < BIC(bestTimeModel)){
      bestTimeModel <- m
    }

    #.GlobalEnv$bestTimeModel <- bestTimeModel

  }
  #summary(m)
  #summary(bestTimeModel)


  modelData <- bestTimeModel$model

  modelData <- subset(modelData, select = -y )

  predData <- data.frame(
    'TimeToSale' = 0:max(modelData$TimeToSale) #0:(max(modelData$TimeToSale)+1 )

    , stringsAsFactors = FALSE)

  df_model <- length( bestTimeModel$coefficients ) -1
  if(df_model > 1 ){ #The model degrees of freedom. This is equal to p - 1, where p is the number of regressors.
    for(dg in 2:df_model ){


      predData[ , paste0("TimeToSale", dg)]   <- (predData$TimeToSale)^dg

    }

    #predict(bestTimeModel, newdata=predData)
    # Show both points and line
    plot(-predData$TimeToSale, predict(bestTimeModel, newdata=predData), type = "b", pch = 19,
         col = "red", xlab = "Months", ylab = "Trend")

    rateTable <- data.frame(
      'Months' = 0:max(modelData$TimeToSale) #0:(max(modelData$TimeToSale)+1 )
      ,'Rate' = predict(bestTimeModel, newdata=predData)[1]/predict(bestTimeModel, newdata=predData)

      , stringsAsFactors = FALSE)

    #.GlobalEnv$rateTable <- rateTable
    return(rateTable)

  } else {
    print("Time is linear")
  }

} #function end


