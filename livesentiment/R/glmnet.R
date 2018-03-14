
createVocab = function(tokenized.tweets, method = "2gram") {
  if (method == "1gram") {
    vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 1L))
  } else if (method == "2gram") {
    vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 2L))
  } else if (method == "3gram") {
    vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 3L))
  } else if (method == "4gram") {
    vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 4L))
  }
}

tokenizerFunc = function(df, prep.fun = tolower, tok.fun = word_tokenizer) {

  # df has columns $text and $id
  itoken(df$text,
         preprocessor = prep.fun,
         tokenizer = tok.fun,
         ids = df$tweetnumber,
         progressbar = TRUE)
}

trainGlmNet = function(tweets.labeled, ngram) {
  # train.data = preProcTweets(tweets.labeled)

  tokenized = tokenizerFunc(tweets.labeled)
  vocab = createVocab(tokenized, method = ngram)

  # creating vocabulary and document-term matrix
  vectorizer = vocab_vectorizer(vocab)
  docterm = create_dtm(tokenized, vectorizer)
  # define tf-idf model
  tfidf = TfIdf$new()
  # fit the model to the train data and transform it with the fitted model
  docterm.tfidf = fit_transform(docterm, tfidf)
  # train the model

  glmnet.sentim.classif = cv.glmnet(x = docterm.tfidf,
                                    y = tweets.labeled[['sentiment']],
                                    family = 'binomial',
                                    # L1 penalty
                                    alpha = 1,
                                    # interested in the area under ROC curve
                                    type.measure = "auc",
                                    # 5-fold cross-validation
                                    nfolds = 5,
                                    # high value is less accurate, but has faster training
                                    thresh = 1e-3,
                                    # again lower number of iterations for faster training
                                    maxit = 1e3)
  plot(glmnet.sentim.classif)
  return(list("classif" = glmnet.sentim.classif, "vocab" = vocab))
}

getGlmnetSentiments <- function(classif, vocab, tweets_oneline_format,
                               lower.threshold, upper.threshold) {

  tokenized = tokenizerFunc(tweets_oneline_format)
  # vocab = createVocab(tokenized, method = ngram)

  # creating vocabulary and document-term matrix
  vectorizer = vocab_vectorizer(vocab)
  docterm = create_dtm(tokenized, vectorizer)
  # define tf-idf model
  tfidf = TfIdf$new()
  # fit the model to the train data and transform it with the fitted model
  docterm.tfidf = fit_transform(docterm, tfidf)
  # train the model
  preds = predict(classif, docterm.tfidf, type = 'response')[ , 1]
  # preds = sapply(preds, FUN = function(x) {
  #   if (x <= 0.45) {
  #     return("negative")
  #   } else if (x >= 0.55) {
  #     return("positive")
  #   } else {
  #     return("neutral")
  #   }
  # })
  sentims = predsToSentiments(preds, lower.threshold = lower.threshold,
                              upper.threshold = upper.threshold)
  out_df = data.table(tweets_oneline_format, sentims, preds)
  colnames(out_df) = c("text", "tweetnumber", "sentiment", "prob")
  out_df <- out_df[, c("tweetnumber", "text", "sentiment", "prob")]
  return(out_df)
}

predsToSentiments <- function(preds, lower.threshold, upper.threshold) {
  sentims = vapply(preds, FUN.VALUE = character(1), FUN = function(x) {
    if (x <= lower.threshold) {
      return("negative")
    } else if (x >= upper.threshold) {
      return("positive")
    } else {
      return("neutral")
    }
  })
  return(sentims)
}

# classifier = trainGlmNet(tweets.training, ngram = "3gram")
# classifier = trainSentimClassif("glmNet", tweets.training, ngram = "3gram")
# tweets = pullTweets(keyword = "trump", file.path = "tweets/tweets.json", interval = 5, geolocation = TRUE)
# tweets_org = organizeTweetsOneliner(tweets$text)
# tweets.df = tweets$text
  # tweets.tok = organizeTweetsTokenized(tweets_org, iter = 1)
# getBingSentiments(tweets_org, iter = 1)
#
# #
# pred = getGlmnetSentiments(classifier$classif, classifier$vocab, tweets_org,
                           # lower.threshold = 0.4, upper.threshold = 0.6)
# summary(pred)
# pred = countStatModelSentiments(pred, iter = 1)
# #
# #

countStatModelSentiments = function(sentiment_df, iter) {
  sentiment_df = sentiment_df %>%
    select(-text) %>%
    count(sentiment) %>%
    spread(sentiment, n) %>%
    mutate(tweetbatch = iter)

  if (!("positive" %in% colnames(sentiment_df))) {sentiment_df = mutate(sentiment_df, "positive" = 0)}
  if (!("negative" %in% colnames(sentiment_df))) {sentiment_df = mutate(sentiment_df, "negative" = 0)}

  sentiment_df = sentiment_df %>%
    mutate("diff" = positive - negative)

  return(sentiment_df)
}

trainSentimClassif = function(model, tweets.labeled, ngram) {
  if (model == "glmNet") {
    trainGlmNet(tweets.labeled, ngram)
  } else {}
}

predictionPlot = function(pred_df, lower.threshold, upper.threshold) {
  n = nrow(pred_df)
  ggplot(data = pred_df, aes(x = 1:n, y = prob)) +
    geom_point(aes(color = sentiment), size = 1.5) +
    geom_hline(yintercept = c(lower.threshold, upper.threshold), color = "black", size = 1) + 
    scale_colour_manual(values = c("negative" = "red", "neutral" = "orange", "positive" = "darkblue"))
  
}

# predictionPlot(pred, 0.4, 0.6)
