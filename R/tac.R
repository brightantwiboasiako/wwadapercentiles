#' tac
#'
#' Gets the activity percentile based on the NHANES (2003-2006) survey.
#'
#' @param y The activity level measurement
#' @param age The age of the individual (must be between 6 and 85)
#' @param sex The sex of the individual (M for male and F for female)
#'
#' @return The percentile(s) of the provided activity measurements
#' @export
#' @examples
#' tac(154754, 24, 'M')
tac <- function(y, age, sex){

  inputs <- cbind(y, age, sex)
  apply(inputs, 1, function(args){
    y <- as.numeric(args[1])
    age <- round(as.numeric(args[2]))
    sex <- as.character(args[3])

    if(age < 6 || age > 85){
      stop("Age must be between 6 and 85 inclusive.")
    }

    if(sex != 'M' && sex != 'F'){
      stop("Sex must be either M or F for male and female respectively.")
    }

    row <- tac_data[ which(tac_data$Age == age & tac_data$Sex == sex),]
    L <- row$L
    M <- row$M
    S <- row$S

    round(gamlss.dist::pBCCG(y, mu = M, sigma = S, nu = L), 2)

  })

}
