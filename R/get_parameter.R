#' @include jst.R jst_reversed.R

#' @title Extract and Tidy Parameters from Estimated Joint Sentiment-Topic (JST)
#'   Models
#'
#' @description `get_parameter()` is a convenient function for extracting a
#'   specific parameter from the results of a Joint Sentiment-Topic model,
#'   computed using either `JST.result` or `JST_reversed.result` objects. The
#'   extracted parameter is returned as a tidy data.frame, easy to manipulate,
#'   analyze, and visualize.
#'
#' @param x An object of class `JST.result` or `JST_reversed.result`,
#'   representing the estimated JST model.
#' @param parameter A character string specifying the parameter to extract and
#'   tidy. There is no default value, you must specify one of these options:
#'   "phi" (Topic-word distribution), "theta" (Document-topic distribution),
#'    or "pi" (Document-sentiment distribution).
#'
#' @return A tidy data.frame containing the selected parameter.
#'
#' @examples
#' \dontrun{
#' # Load required libraries and data
#' library(quanteda)
#' library(quanteda.textmodels)
#' data <- data_corpus_irishbudget2010 %>%
#'   tokens() %>%
#'   dfm()
#'
#' # Estimate a JST model with 5 topics and 50 iterations
#' model <- jst(data, paradigm(), numTopics = 5, numIters = 50)
#'
#' # Extract and tidy different parameters from the estimated model
#' phi <- get_parameter(model, "phi")    # Topic-word distribution
#' theta <- get_parameter(model, "theta") # Document-topic distribution
#' pi <- get_parameter(model, "pi")      # Document-sentiment distribution
#'
#' # Explore the tidy parameter data.frames
#' head(phi)
#' head(theta)
#' head(pi)
#' }
#' @export
#' @md
get_parameter <- function(x, parameter = NULL) {
  if (is.JST_reversed.result(x)) {
    if (is.null(parameter)) {
      stop('Please specify which parameter from the object you would like to get')
    } else if (parameter == 'pi') {
      return (get_parameter.JST_reversed.result.pi(x))
    } else if (parameter == 'theta') {
      return (get_parameter.JST_reversed.result.theta(x))
    } else if (parameter == 'phi') {
      return (get_parameter.JST_reversed.result.phi(x))
    } else {
      stop(paste('\'', parameter, '\' is not a valid parameter of the JST_reversed.result model.', sep = ''))
    }
  } else if (is.JST.result(x)) {
    if (is.null(parameter)) {
      stop('Please specify which parameter from the object you would like to get')
    } else if (parameter == 'pi') {
      return (get_parameter.JST.result.pi(x))
    } else if (parameter == 'theta') {
      return (get_parameter.JST.result.theta(x))
    } else if (parameter == 'phi') {
      return (get_parameter.JST.result.phi(x))
    } else {
      stop(paste('\'', parameter, '\' is not a valid parameter of the JST.result model.', sep = ''))
    }
  } else {
    stop('The object to get a parameter from is not a valid (reversed) JST results object.')
  }

}

get_parameter.JST_reversed.result.pi <- function(x) {
  res <- x@pi

  if (length(x@docvars) > 0) {
    docvars <- x@docvars
    docvars$docID <- rownames(docvars)

    res <- merge(docvars, res, by = 'docID')
  }

  return (res)
}

get_parameter.JST_reversed.result.theta <- function(x) {
  if (length(x@docvars) > 0) {
    res <- cbind(x@docvars, x@theta)
  } else {
    res <- x@theta
  }

  return(res)
}

get_parameter.JST_reversed.result.phi <- function(x) {
  res <- x@phi

  res$word <- rownames(res)
  res$word <- as.factor(res$word)
  rownames(res) <- NULL

  res <- reshape2::melt(res, id='word')

  variable <- as.character(res$variable)
  variable <- gsub('topic', '', variable)
  topic <- as.numeric(substr(variable, start = 1, stop = regexpr('s', variable) - 1))
  sentiment <- as.numeric(substr(variable, start = regexpr('t', variable) + 1,
                                 stop = nchar(variable)))

  res <- cbind(res, topic, sentiment)
  res <- subset(res, select=c('word', 'sentiment', 'topic', 'value'))

  return(res)
}

get_parameter.JST.result.pi <- function(x) {
  if (length(x@docvars) > 0) {
    docvars <- x@docvars

    res <- cbind(docvars, x@pi)
  } else {
    res <- x@pi
  }
  return(res)
}

get_parameter.JST.result.theta <- function(x) {
  if (length(x@docvars) > 0) {
    docvars <- x@docvars
    docvars$docID <- rownames(docvars)

    res <- merge(docvars, x@theta, by = 'docID')
  } else {
    res <- x@theta
  }
  return(res)
}

get_parameter.JST.result.phi <- function(x) {
  res <- x@phi

  res$word <- rownames(res)
  res$word <- as.factor(res$word)
  rownames(res) <- NULL

  res <- reshape2::melt(res, id='word')

  variable <- as.character(res$variable)
  variable <- gsub('topic', '', variable)
  sentiment <- as.numeric(substr(variable, start=1, stop=regexpr('s', variable)-1))
  topic <- as.numeric(substr(variable, start = regexpr('t', variable) + 1, stop = nchar(variable)))

  res <- cbind(res, topic, sentiment)
  res <- subset(res, select=c('word', 'sentiment', 'topic', 'value'))

  return(res)
}
