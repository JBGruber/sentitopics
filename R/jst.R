#' @include topNwords.R
#' @importFrom methods new

#' @title JST results object
#'
#' @description Contains estimated Joint Sentiment Topic model
#'
#' @slot pi Document-level sentiment estimates
#' @slot theta Document-level sentitopic estimates
#' @slot phi Word-level sentitopic estimates
#' @slot phi.termScores Word-level term scores (suboptimal calculation, only useful for smaller models)
#' @slot numTopics Number of topics
#' @slot numSentiments Number of sentiment categories
#' @slot docvars Document-level metadata from the quanteda object used as input
JST.result <- setClass(
  'JST.result',
  representation(
    pi = "data.frame",
    theta = "data.frame",
    phi = "data.frame",
    phi.termScores = "data.frame",
    numTopics = "numeric",
    numSentiments = "numeric",
    docvars = "data.frame"
  )
)

#' Check if an object is a JST.result object
#'
#' @param x object
#'
#' @return Boolean. True if x is a JST.result object.
#'
#' @export
is.JST.result <- function(x) {
  return(inherits(x, 'JST.result'))
}

#' Run a Joint Sentiment Topic model
#'
#' This function implements the Joint Sentiment Topic model based on
#' the Gibbs sampling method. For the theoretical background, refer to the paper
#' by Lin and He (2009) for the basic model and Lin et al. (2012) for the supervised version.
#'
#' @param dfm A quanteda "dfm" (document-feature matrix) object containing the
#'   text data.
#' @param sentiLexInput Optional: A quanteda dictionary object for
#'   semi-supervised learning. If provided, the sentiment labels will be
#'   inferred from this dictionary, overriding \code{numSentiLabs}. By default,
#'   an additional neutral category is included. To exclude, set \code{excludeNeutral =
#'   TRUE}.
#' @param numSentiLabs Number of sentiment labels. Default is 3.
#' @param numTopics Number of topics to extract from the data. Default is 10.
#' @param numIters Number of iterations for the Gibbs sampler. Set to 3 for test
#'   runs. Adjust as needed for real data.
#' @param updateParaStep Number of iterations between updates of the
#'   hyperparameter alpha.
#' @param alpha Hyperparameter for topic distributions. If not set, it will be
#'   calculated using the formula provided.
#' @param beta Hyperparameter for sentiment-topic word distributions. Default is
#'   based on sentiment dictionary presence.
#' @param gamma Hyperparameter for document-topic distributions. If not set, it
#'   will be calculated using the formula provided.
#' @param excludeNeutral Boolean flag to determine if neutral sentiment category
#'   should be excluded. Default is \code{FALSE}.
#'
#' @return A JST_reversed.result. See \link{JST_reversed.result}
#'
#' @note For detailed model description, refer to: 
#' 
#'    Lin, C. and He, Y., 2009, November. Joint sentiment/topic model for sentiment analysis. In
#'    Proceedings of the 18th ACM conference on Information and knowledge management (pp. 375-384). ACM.
#' 
#'    Lin, C., He, Y., Everson, R.
#'    & Ruger, S. (2012). Weakly supervised joint sentiment-topic detection from
#'    text. IEEE Transactions on Knowledge and Data engineering, 24(6),
#'    pp.1134-1145.   
#'   
#'
#' @examples
#' \dontrun{
#' library(quanteda)
#' library(quanteda.textmodels)
#' data_irishbudget2010 <- data_corpus_irishbudget2010 %>%
#'   tokens() %>%
#'   dfm()
#'
#' model <- jst(data_irishbudget2010, sentiLexInput = paradigm(), numTopics = 5, numIters = 150)
#' model
#' }
#'
#' @export
jst <- function(dfm,
                sentiLexInput = NULL,
                numSentiLabs = 3,
                numTopics = 10,
                numIters = 3,
                updateParaStep = -1,
                alpha = -1,
                beta = -1,
                gamma = -1,
                excludeNeutral = FALSE) {
  if (!any(class(dfm) %in% c("dfm", "DocumentTermMatrix"))) {
    stop("Please input a quanteda dfm or tidytext tm object as data.")
  }
  if(any(class(dfm) == "DocumentTermMatrix")) {
    dfm <- dfm %>% quanteda::as.dfm()
  }

  sentiWords <- integer()
  sentimentCategory <- integer()

  if (!is.null(sentiLexInput)) {
    if (quanteda::is.dictionary(sentiLexInput)) {
      numSentiLabs_Lex <- length(sentiLexInput)
      numSentiLabs <- numSentiLabs_Lex + 1 - excludeNeutral

      size <- 1
      for (i in c(1:numSentiLabs_Lex)) {
        for (word in sentiLexInput[[i]]) {
          if (word %in% quanteda::featnames(dfm)) {
            sentiWords[size] <-
              as.integer(match(word, quanteda::featnames(dfm)) - 1) #-1 for C++ index
            sentimentCategory[size] <- as.integer(i - excludeNeutral)
            size <- size + 1
          }
        }
      }
    } else {
      stop('The input lexicon needs to be a quanteda dictionary object.')
    }
  }

  res <-
    jstcpp(
      dfm,
      sentiWords,
      sentimentCategory,
      numSentiLabs,
      numTopics,
      numIters,
      updateParaStep,
      alpha,
      beta,
      gamma
    )

  if (length(res) == 0) {
    return(".")
  }

  #prepare doc sentiment distribution data.frame
  docID <- quanteda::docnames(dfm)

  pi <- as.data.frame(res$pi)
  pi <- as.data.frame(t(pi))

  pi.names = character(numSentiLabs)
  for (i in c(1:numSentiLabs)) {
    pi.names[i] <- paste("sent", i, sep = "")
  }
  names(pi) <- pi.names
  rownames(pi) <- docID

  #prepare doc sentiment/topic distribution data.frame
  theta <- as.data.frame(res$theta)

  theta.names <- character(numTopics)

  theta.names = character(numSentiLabs * numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      theta.names[j + numTopics * (i - 1)] <-
        paste("topic", j, "sent", i, sep = "")
    }
  }

  names(theta) <- theta.names

  theta <- data.frame(docID, theta, row.names = NULL)

  #prepare word topic/sentiment distribtuion data.frame
  phi <- as.data.frame(res$phi)
  phi.termScores <- as.data.frame(res$phi.termScores)

  phi.names = character(numSentiLabs * numTopics)
  for (i in c(1:numSentiLabs)) {
    for (j in c(1:numTopics)) {
      phi.names[j + numTopics * (i - 1)] <-
        paste("topic", j, "sent", i, sep = "")
    }
  }
  names(phi) <- phi.names
  names(phi.termScores) <- phi.names
  rownames(phi) <- quanteda::featnames(dfm)
  rownames(phi.termScores) <- quanteda::featnames(dfm)

  return(
    JST.result(
      pi = pi,
      theta = theta,
      phi = phi,
      phi.termScores = phi.termScores,
      numTopics = numTopics,
      numSentiments = numSentiLabs,
      docvars = data.frame(quanteda::docvars(dfm), row.names = docID,
                           stringsAsFactors = FALSE)
    )
  )
}

#' @rdname topNwords-method
#' @aliases topNwords,JST.result,numeric,numeric,numeric-method
setMethod('topNwords', c('JST.result', 'numeric', 'numeric', 'numeric'),
          function(x, N, topic, sentiment) {
            colname <- paste('topic', topic, 'sent', sentiment, sep = '')

            column <- sapply(x@phi[colname], as.numeric)

            res <- rownames(x@phi)[topNwordSeeds(column, N)]

            res <- as.data.frame(res, stringsAsFactors = FALSE)

            names(res) <- colname

            return(res)
          })

#' @rdname topNwords-method
#' @aliases topNwords,JST.result,numeric,-method
setMethod('topNwords', c('JST.result', 'numeric'),
          function(x, N) {
            res <- as.data.frame(matrix(ncol = 0, nrow = N))

            for (topic in c(1:x@numTopics)) {
              for (sentiment in c(1:x@numSentiments)) {
                res <- cbind(res, topNwords(x, N, topic, sentiment))
              }
            }

            return(res)
          })

#' @rdname top20words-method
#' @aliases top20words,JST.result,numeric,numeric-method
setMethod('top20words', c('JST.result', 'numeric', 'numeric'),
          function(x, topic, sentiment) {
            return(topNwords(x, 20, topic, sentiment))
          })

#' @rdname top20words-method
#' @aliases top20words,JST.result-method
setMethod('top20words', c('JST.result'),
          function(x) {
            return(topNwords(x, 20))
          })
