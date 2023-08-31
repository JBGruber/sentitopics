#' Estimate Sentiment Labels with the Joint Sentiment Topic Model Several Times
#'
#' This function runs the Joint Sentiment Topic (JST) model multiple times and
#' provides an average sentiment label prediction for each run.
#'
#' @details Instead of running the JST model once, this function runs it 'n' times in
#' parallel. Instead of the full results, this function provides the average
#' sentiment label predictions with associated uncertainty estimates. For the
#' foundational model's details, please refer to the \code{\link{jst}}
#' function's documentation.
#' 
#' \strong{Advantages}:
#' \itemize{
#'   \item \strong{Improved Robustness}: Multiple runs can capture a more accurate representation of the underlying data structure by averaging out the noise.
#'   \item \strong{Uncertainty Estimation}: Provides a measure of variability in sentiment predictions, offering insights into the reliability of the results.
#'   \item \strong{Parallel Processing}: Utilizes parallel processing for faster computations, especially beneficial for large datasets or complex models.
#' }
#'
#' \strong{Disadvantages}:
#' \itemize{
#'   \item \strong{Computational Cost}: Requires more computational resources due to multiple runs.
#'   \item \strong{Memory Usage}: Might increase memory demand especially if the dataset is large.
#'   \item \strong{Results Interpretation}: Provides average values rather than full model details, which might be limiting if detailed model analysis is needed.
#' }
#' 
#' @inheritParams jst
#' @param n Number of model runs (defaults to 30)
#' @param confidence Confidence level for confidence intervals (defaults to 0.95)
#' @param ncores Number of (logical) CPU cores used for the estimation. (defaults to available number of cores - 3)
#' @param seed Seed for random number generator for reproducibility (defaults to \code{NA})
#' 
#' @return A data.frame with mean sentiment label predictions and uncertainty estimates
#'
#' @examples
#' \dontrun{
#' library(quanteda)
#' library(quanteda.textmodels)
#' data_irishbudget2010 <- data_corpus_irishbudget2010 %>% 
#'   tokens() %>% 
#'   dfm()
#' 
#' model <- jstManyRuns(data_irishbudget2010, 
#'                      sentiLexInput = paradigm(), 
#'                      numTopics = 5, 
#'                      numIters = 150,
#'                      n = 5,
#'                      confidence = 0.95,
#'                      ncores = 4,
#'                      seed = 123456)
#' }
#' 
#' @export
jstManyRuns <- function(dfm,
                        sentiLexInput = NULL,
                        numSentiLabs = 3,
                        numTopics = 10,
                        numIters = 3,
                        updateParaStep = -1,
                        alpha = -1,
                        beta = -1,
                        gamma = -1,
                        excludeNeutral = FALSE,
                        n = 30,
                        confidence = 0.95,
                        ncores = NA,
                        seed = NA){
  
  library(doRNG) # TO DO: Find other solution to use %dorng% from the doRNG-packge here
  
  if(is.na(ncores)){
    ncores <- parallel::detectCores() - 3
  }
  cl <- parallel::makeCluster(ncores) 
  doParallel::registerDoParallel(cl) 
  
  if(!is.na(seed)) set.seed(seed) # TO DO: use withr to make sure rng of user isn't changed 
  
  l <- foreach(i=1:n,
               .packages = c("sentitopics", "dplyr", "magrittr", "readr", "quanteda", "quanteda.textstats")) %dorng% 
    {
      
      jst_out <- sentitopics::jst(dfm = dfm, 
                                  sentiLexInput = sentiLexInput,
                                  numTopics = numTopics,
                                  numIters = numIters,
                                  excludeNeutral = excludeNeutral)  %>% 
        sentitopics::get_parameter("pi")
      
      # extract 3 labels if neutral is included
      if(excludeNeutral == FALSE){
        jst_out <- jst_out %>% 
          dplyr::select(sent1, sent2, sent3) 
      }else{ # otherwise just 2 labels (pos and neg)
        jst_out <- jst_out %>% 
          dplyr::select(sent1, sent2)
      }
      
      return(jst_out)
    }
  
  parallel::stopCluster(cl) 
  
  sent1 <- lapply(l, function(x) x %>% dplyr::select(sent1)) %>% data.frame()
  sent2 <- lapply(l, function(x) x %>% dplyr::select(sent2)) %>% data.frame()
  
  if(excludeNeutral == FALSE){
    sent3 <- lapply(l, function(x) x %>% dplyr::select(sent3)) %>% data.frame()
  }
  
  # calc stats rowwise (each row is one document in dfm, each column is pi from one model run)
  calcStats <- function(df){
    
    df <- df %>%  
      dplyr::mutate(mean = Matrix::rowMeans(.), 
                    sd = matrixStats::rowSds(as.matrix(.)),
                    se = sd/(sqrt(n)))
    
    if(n >= 30){
      df <- df %>%  
        dplyr::mutate(ci_high = mean + (qnorm((confidence + (1 - confidence) / 2)) * se),
                      ci_low = mean - (qnorm((confidence + (1 - confidence) / 2)) * se)) 
    }else{
      df <- df %>%  
        dplyr::mutate(ci_high = mean + (qt((confidence + (1 - confidence) / 2), df = n - 1) * se),
                      ci_low = mean - (qt((confidence + (1 - confidence) / 2), df = n - 1) * se)) 
    }
    
    df <- df %>% dplyr::select(mean, sd, se, ci_high, ci_low)
    return(df)
  }
  
  sent1 <- sent1 %>% calcStats()
  sent2 <- sent2 %>% calcStats()
  if(excludeNeutral == FALSE){
    sent3 <- sent3 %>% calcStats()
  }
  
  res <- data.frame(sent1_mean = sent1$mean,
                    sent1_sd = sent1$sd,
                    sent1_se = sent1$se,
                    sent1_ci_high = sent1$ci_high,
                    sent1_ci_low = sent1$ci_low,
                    sent2_mean = sent2$mean,
                    sent2_sd = sent2$sd,
                    sent2_se = sent2$se,
                    sent2_ci_high = sent2$ci_high,
                    sent2_ci_low = sent2$ci_low)
  
  if(excludeNeutral == FALSE){
    res <- res %>% 
      cbind(data.frame(sent3_mean = sent3$mean,
                       sent3_sd = sent3$sd,
                       sent3_se = sent3$se,
                       sent3_ci_high = sent3$ci_high,
                       sent3_ci_low = sent3$ci_low)) 
  }
  
  res <- cbind(dfm@docvars, res)
  
  return(res)
}