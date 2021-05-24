#' mims
#'
#' Gets the activity percentile based on the NHANES (2003-2006) survey.
#'
#' @param y The activity level measurement
#' @param age The age of the individual (must be at least 3)
#' @param sex The sex of the individual (M for male and F for female)
#'
#' @return Belcher, Wolff-Hughes, Dooley, Staudenmayer, Berrigan, Eberhardt, and Troiano. (2021) U.S. Population-Referenced Percentiles for Wrist-Worn Accelerometer Derived Activity.
#' @return The percentile(s) of the provided activity measurements
#' @export
#' @examples
#' mims(154754, 24, 'M')
mims <- function(y, age, sex){

  inputs <- cbind(y, age, sex)
  apply(inputs, 1, function(args){
    y <- as.numeric(args[1])
    age <- round(as.numeric(args[2]))
    sex <- as.character(args[3])

    if(age < 3){
      stop("Age must be bat least 3.")
    }

    if(sex != 'M' && sex != 'F'){
      stop("Sex must be either M or F for male and female respectively.")
    }

    if(age > 79){
      age = '80+' # Use 80+ data
    }

    row <- mims_data[ which(mims_data$Age == age & mims_data$Sex == sex),]
    L <- row$Lambda
    M <- row$Mu
    S <- row$Sigma
    Tau <- row$Tau

    round(gamlss.dist::pBCT(y, mu = M, sigma = S, nu = L, tau = exp(Tau)), 2)

  })

}
