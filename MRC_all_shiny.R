#'Compute power for Multiple Regression with Three Predictors
#'Requires correlations between all variables as sample size. Means, sds, and alpha are option. Also computes Power(All)
#'@param ry1 Correlation between DV (y) and first predictor (1)
#'@param ry2 Correlation between DV (y) and second predictor (2)
#'@param ry3 Correlation between DV (y) and third predictor (3)
#'@param r12 Correlation between first (1) and second predictor (2)
#'@param r13 Correlation between first (1) and third predictor (3)
#'@param r23 Correlation between second (2) and third predictor (3)
#'@param n Sample size
#'@param alpha Type I error (default is .05)
#'@param rep number of replications (default is 10000)
#'@param all default is OFF, ON returns Power(All)
#'@return Power for Multiple Regression (ALL)
#'@export
#'
#'

MRC_all <- function(ry1 = NULL, ry2 = NULL, ry3 = NULL, r12 = NULL, r13 = NULL, r23 = NULL, n = 100, alpha = .05, nruns = 10000,
                    my = 0, m1 = 0, m2 = 0, m3 = 0, sy = 1, s1 = 1, s2 = 1, s3 = 1, predictors = 2, ...){
  
  
  
  
  # calculate variances
  var_y <- sy^2
  var_1 <- s1^2
  var_2 <- s2^2
  if (predictors == 3) var_3 <- s3^2
  
  ###### -------------------------- BEGIN 1 PREDICTOR
  
  # begin power analysis for a simple regression
  if (predictors == 1){
    
    # check all necessary values
    if (is.null(ry1)){
      stop("Make sure there are no missing correlation values for a 1 predictor regression")
    }
    
    # simulate population from Multivariate Normal Distribution using specified covariance matrix
    sim_pop <- MASS::mvrnorm(n = 100000,
                             mu = c(my, m1),
                             Sigma = matrix(c(var_y, ry1,
                                              ry1, var_1), ncol = 2),
                             empirical = TRUE)
    
    sim_pop <- data.frame(sim_pop)
    colnames(sim_pop) <- c("y", "x1")
  
    # initialize vectors for slopes, R-squared, F statistic, and degrees of freedom
    b1 <- c()
    r2 <- c()
    f_stat <- c()
    df1 <- c()
    df2 <- c()
    
    # simulate data
    for (i in 1:nruns){
      # grab a random sample from sim_pop
      split <- sample(nrow(sim_pop), size = n)
      samp <- sim_pop[split, ]
      
      # run test regressions
      test <- lm(y ~ x1, data = samp)
      summ <- summary(test)
      
      # grab p-values from regressions for slopes
      b1[i] <- summ$coefficients[2, 4]
      
      # grab R-squareds, F-statistics, and degreees of freedom from regressions
      r2[i] <- summ$r.squared
      f_stat[i] <- summ$fstatistic[1]
      df1[i] <- summ$fstatistic[2]
      df2[i] <- summ$fstatistic[3]
    }
    
    # count totals for number of times we can reject the null for the parameters
    reject_b1 <- ifelse(b1 < alpha, 1, 0)
    
    # count totals for being able to reject the null for
    not_reject <- ifelse(reject_b1 == 0, 1, 0)
    reject <- ifelse(reject_b1 == 1, 1, 0)
    
    # count total for being able to reject R-squared
    probability_r2 <- 1 - pf(f_stat, df1, df2)
    reject_r2 <- ifelse(probability_r2 < alpha, 1, 0)
    
    # calculate power for slopes, r2, and rejecting null for one, two or neither of the parameters
    power_b1 <- mean(reject_b1)
    power_not_reject <- mean(not_reject)
    power_reject <- mean(reject)
    power_r2 <- mean(reject_r2)
    
    # create and store result messages
    n_print        <- paste0("Sample size is = ", n)
    power_r2_print <- paste0("Power R2 = ", power_r2)
    power_b1_print <- paste0("Power b1 = ", power_b1)
    power_none_print <- paste0("Proportion failing to reject the null = ", power_not_reject)
    power_one_print  <- paste0("Proportion rejecting the null ", power_reject)
    
    vals <- list(n_print = n_print, 
                 power_r2_print = power_r2_print,
                 power_b1_print = power_b1_print, 
                 power_none_print = power_none_print, 
                 power_one_print = power_one_print
                 )
    return(vals)
    }
    
    
    
    
    
  ###### -------------------------- END 1 PREDICTOR
  
  
  # begin power analysis for 2 predictor multiple regression
  if (predictors == 2){
    
    # check all necessary values
    if (is.null(ry1) | is.null(ry2)| is.null(r12)){
      stop("Make sure there are no missing correlation values for a 2 predictor regression")
    }
    
    # simulate population from Multivariate Normal Distribution using specified covariance matrix
    sim_pop <- MASS::mvrnorm(n = 100000,
                             mu = c(my, m1, m2),
                             Sigma = matrix(c(var_y, ry1, ry2,
                                              ry1, var_1, r12,
                                              ry2, r12, var_2), ncol = 3),
                             empirical = TRUE)
    sim_pop <- data.frame(sim_pop)
    colnames(sim_pop) <- c("y", "x1", "x2")
    
    # initialize vectors for slopes, R-squared, F statistic, and degrees of freedom
    b1 <- c()
    b2 <- c()
    r2 <- c()
    f_stat <- c()
    df1 <- c()
    df2 <- c()
    
    # simulate data
    for (i in 1:nruns){
      # grab a random sample from sim_pop
      split <- sample(nrow(sim_pop), size = n)
      samp <- sim_pop[split, ]
      
      # run test regressions
      test <- lm(y ~ x1 + x2, data = samp)
      summ <- summary(test)
      
      # grab p-values from regressions for slopes
      b1[i] <- summ$coefficients[2, 4]
      b2[i] <- summ$coefficients[3, 4]
      
      # grab R-squareds, F-statistics, and degreees of freedom from regressions
      r2[i] <- summ$r.squared
      f_stat[i] <- summ$fstatistic[1]
      df1[i] <- summ$fstatistic[2]
      df2[i] <- summ$fstatistic[3]
    }
    # count totals for number of times we can reject the null for the parameters
    reject_b1 <- ifelse(b1 < alpha, 1, 0)
    reject_b2 <- ifelse(b2 < alpha, 1, 0)
    
    # count totals for being able to reject the null for one, two, or neither of the parameters
    reject_count <- reject_b1 + reject_b2
    reject_none <- ifelse(reject_count == 0, 1, 0)
    reject_one <- ifelse(reject_count == 1, 1, 0)
    reject_all <- ifelse(reject_count == 2, 1, 0)
    
    # count total for being able to reject R-squared
    probability_r2 <- 1 - pf(f_stat, df1, df2)
    reject_r2 <- ifelse(probability_r2 < alpha, 1, 0)
    
    # calculate power for slopes, r2, and rejecting null for one, two or neither of the parameters
    power_b1 <- mean(reject_b1)
    power_b2 <- mean(reject_b2)
    power_none <- mean(reject_none)
    power_one <- mean(reject_one)
    power_all <- mean(reject_all)
    power_r2 <- mean(reject_r2)
    
    # print results
    n_print        <- paste0("Sample size is = ", n)
    power_r2_print <- paste0("Power R2 = ", power_r2)
    power_b1_print <- paste0("Power b1 = ", power_b1)
    power_b2_print <- paste0("Power b2 = ", power_b2)
    power_none_print <- paste0("Proportion Rejecting None = ", power_none)
    power_one_print  <- paste0("Proportion Rejecting One = ", power_one)
    power_all_print <- paste0("Proportion Rejecting All = ", power_all)
    
    vals <- list(n_print = n_print, power_r2_print = power_r2_print,
                 power_b1_print = power_b1_print, power_b2_print = power_b2_print, 
                 power_none_print = power_none_print, power_one_print = power_one_print, 
                 power_all_print = power_all_print)
    return(vals)
    
  }
  
  # begin power analysis for 3 predictor multiple regression
  if (predictors == 3){
    
    # check all necessary values
    if (is.null(ry1) | is.null(ry2)| is.null(ry3) | is.null(r12) | is.null(r13) | is.null(r23)){
      stop("Make sure there are no missing correlation values for a 3 predictor regression")
    }
    
    # simulate population from Multivariate Normal Distribution using specified covariance matrix
    sim_pop <- MASS::mvrnorm(n = 100000,
                             mu = c(my, m1, m2, m3),
                             Sigma = matrix(c(var_y, ry1, ry2, ry3,
                                              ry1, var_1, r12, r13,
                                              ry2, r12, var_2, r23,
                                              ry3, r13, r23, var_3), ncol = 4),
                             empirical = TRUE)
    sim_pop <- data.frame(sim_pop)
    colnames(sim_pop) <- c("y", "x1", "x2", "x3")
    
    # initialize vectors for slopes, R-squared, F statistic, and degrees of freedom
    b1 <- c()
    b2 <- c()
    b3 <- c()
    r2 <- c()
    f_stat <- c()
    df1 <- c()
    df2 <- c()
    
    # simulate data
    for (i in 1:nruns){
      # grab a random sample from sim_pop
      split <- sample(nrow(sim_pop), size = n)
      samp <- sim_pop[split, ]
      
      # run test regressions
      test <- lm(y ~ x1 + x2 + x3, data = samp)
      summ <- summary(test)
      
      # grab p-values from regressions for slopes
      b1[i] <- summ$coefficients[2, 4]
      b2[i] <- summ$coefficients[3, 4]
      b3[i] <- summ$coefficients[4, 4]
      
      # grab R-squareds, F-statistics, and degreees of freedom from regressions
      r2[i] <- summ$r.squared
      f_stat[i] <- summ$fstatistic[1]
      df1[i] <- summ$fstatistic[2]
      df2[i] <- summ$fstatistic[3]
    }
    # count totals for number of times we can reject the null for the parameters
    reject_b1 <- ifelse(b1 < alpha, 1, 0)
    reject_b2 <- ifelse(b2 < alpha, 1, 0)
    reject_b3 <- ifelse(b3 < alpha, 1, 0)
    
    # count totals for being able to reject the null for one, two, three, or none of the parameters
    reject_count <- reject_b1 + reject_b2 + reject_b3
    reject_none <- ifelse(reject_count == 0, 1, 0)
    reject_one <- ifelse(reject_count == 1, 1, 0)
    reject_two <- ifelse(reject_count == 2, 1, 0)
    reject_all <- ifelse(reject_count == 3, 1, 0)
    
    # count total for being able to reject R-squared
    probability_r2 <- 1 - pf(f_stat, df1, df2)
    reject_r2 <- ifelse(probability_r2 < alpha, 1, 0)
    
    # calculate power for slopes, r2, and rejecting null for one, two or neither of the parameters
    power_b1   <- mean(reject_b1)
    power_b2   <- mean(reject_b2)
    power_b3   <- mean(reject_b3)
    power_none <- mean(reject_none)
    power_one  <- mean(reject_one)
    power_two  <- mean(reject_two)
    power_all  <- mean(reject_all)
    power_r2   <- mean(reject_r2)
    
    # get results
    n_print        <- paste0("Sample size is = ", n)
    power_r2_print <- paste0("Power R2 = ", power_r2)
    power_b1_print <- paste0("Power b1 = ", power_b1)
    power_b2_print <- paste0("Power b2 = ", power_b2)
    power_b3_print <- paste0("Power b3 = ", power_b3)
    power_none_print <- paste0("Proportion Rejecting None = ", power_none)
    power_one_print  <- paste0("Proportion Rejecting One = ", power_one)
    power_two_print <- paste0("Proportion Rejecting Two = ", power_two)
    power_all_print <- paste0("Proportion Rejecting All = ", power_all)
    
    vals <- list(n_print = n_print, power_r2_print = power_r2_print,
                 power_b1_print = power_b1_print, power_b2_print = power_b2_print, 
                 power_b3_print = power_b3_print, power_none_print = power_none_print, 
                 power_one_print = power_one_print, power_two_print = power_two_print, 
                 power_all_print = power_all_print)
    return(vals)
  }
}




