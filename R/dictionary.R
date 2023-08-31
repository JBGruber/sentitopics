#' Paradigm word list (Lin and He, 2009, p.379)
#'
#' This function returns a list of positive and negative words known as the
#' "paradigm word list," which was proposed by Lin and He (2009). This list is
#' considered to be domain-independent and is mostly used for sentiment analysis
#' in reviews. However, it is important to review the words in the list before
#' using it as a dictionary. See \link[quanteda]{dictionary} for more
#' information about the returned object.
#'
#' @return A quanteda dictionary object containing the paradigm word list
#'
#' @note Lin, C., & He, Y. (2009). Joint sentiment/topic model for sentiment
#'   analysis. In Proceedings of the 18th ACM conference on Information and
#'   knowledge management (pp. 375-384). ACM.
#'
#' @export
#'
#' @examples
#' # Load the paradigm word list
#' paradigm_dict <- paradigm()
#'
#' # add words to dictionary
#' paradigm_dict$positive <- c(paradigm_dict$positive, "best")
#'
#' # remove words from dictionary
#' paradigm_dict$positive <- setdiff(paradigm_dict$positive, "best")
paradigm <- function() {
  positive <- c(
    "dazzling", "brilliant", "phenomenal", "excellent", "fantastic", "gripping",
    "mesmerizing", "riveting", "spectacular", "cool", "awesome", "thrilling",
    "moving", "exciting", "love", "wonderful", "best", "great", "superb", "still",
    "beautiful"
  )
  negative <- c(
    "sucks", "terrible", "awful", "unwatchable", "hideous", "bad", "cliched",
    "boring", "stupid", "slow", "worst", "waste", "unexcited", "rubbish",
    "tedious", "unbearable", "pointless", "cheesy", "frustrated", "awkward",
    "disappointing"
  )
  return(quanteda::dictionary(list("positive" = positive, "negative" = negative)))
}

#' Wordstem a quanteda Dictionary object
#' 
#' Applies Porter stemming to the words in a \link[quanteda]{dictionary} and then removes 
#' duplicates, if the stemming created these.
#' 
#' @param dict A quanteda dictionary2 object
#' @return A quanteda dictionary2 object with all elements stemmed
#' 
#' @export
#' 
#' @examples
#' # Load the paradigm word list
#' paradigm_dict <- paradigm()
#'
#' # Stem the words in the paradigm word list dictionary
#' stemmed_paradigm_dict <- dictionary_wordstem(paradigm_dict)
#' 
#' # Create a simple dictionary
#' simple_dict <- quanteda::dictionary(list(positive = c("good", "great", "excellent"),
#' negative = c("bad", "terrible", "awful")))
#'
#' # Stem the words in the simple dictionary
#' stemmed_simple_dict <- dictionary_wordstem(simple_dict)
dictionary_wordstem <- function(dict, language = "porter") {
  sentiments <- names(dict)
  dictList <- as.list(dict)
  newdict <- list()
  for (name in sentiments) {
    newdict[[name]] <- unique(SnowballC::wordStem(dictList[[name]], language = language))
  }
  return(quanteda::dictionary(newdict))
}