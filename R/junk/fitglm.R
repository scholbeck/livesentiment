# # # loading packages
# # library(twitteR)
# # library(ROAuth)
# # library(tidyverse)
# # library(text2vec)
# # library(caret)
# # library(glmnet)
# # library(ggrepel)
# # library(purrrlyr)
# #
# #
# #
# # ##### doc2vec #####
# # # define preprocessing function and tokenization function
# #
# # textPreProcess = tolower
# # tokenizerFunc = word_tokenizer
# #
# # tokenizerFunc = function(df) {
# #   # df has columns $text and $id
# #   itoken(df$text,
# #          preprocessor = prep_fun,
# #          tokenizer = tok_fun,
# #          ids = df$id,
# #          progressbar = TRUE)
# # }
# #
# # it_train = tokenizerFunc(data)
# # it_test <- tokenizerFunc(tweets_test)
# #
# # # creating vocabulary and document-term matrix
# # vocab = create_vocabulary(it_train)
# # save(vocab, file = "Data/vocab.rdata")
# # vectorizer = vocab_vectorizer(vocab)
# # docterm.train = create_dtm(it_train, vectorizer)
# # docterm.test = create_dtm(it_test, vectorizer)
# # # define tf-idf model
# # tfidf = TfIdf$new()
# # # fit the model to the train data and transform it with the fitted model
# # docterm.train.tfidf = fit_transform(docterm.train, tfidf)
# # docterm.test.tfidf = fit_transform(docterm.test, tfidf)
# #
# # # train the model
# # t1 <- Sys.time()
# # glmnet.sentim.classifier <- cv.glmnet(x = docterm.train.tfidf, y = tweets_train[['sentiment']],
# #                                family = 'binomial',
# #                                # L1 penalty
# #                                alpha = 1,
# #                                # interested in the area under ROC curve
# #                                type.measure = "auc",
# #                                # 5-fold cross-validation
# #                                nfolds = 5,
# #                                # high value is less accurate, but has faster training
# #                                thresh = 1e-3,
# #                                # again lower number of iterations for faster training
# #                                maxit = 1e3)
# # print(difftime(Sys.time(), t1, units = 'mins'))
# #
# # plot(glmnet.sentim.classifier)
# # print(paste("max AUC =", round(max(glmnet.sentim.classifier$cvm), 4)))
# #
# # preds = predict(glmnet.sentim.classifier, docterm.test.tfidf, type = 'response')[ ,1]
# # glmnet:::auc(as.numeric(tweets_test$sentiment), preds)
# #
# #
# # tweets = pullTweets("trump", "Tweets/tweets", interval = 5, geolocation = FALSE)
# # vocab = create_vocabulary(it_train)
# # raw.tweets = tweets
#
# library(text2vec)
#
# preProcTweets = function(raw.tweets) {
#
#   replace_reg = "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
#   unnest_reg = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
#
#   raw.tweets %>%
#     as.tibble() %>%
#     rename(text = value) %>%
#     filter(!str_detect(text, "^RT")) %>%
#     mutate(text = str_replace_all(text, replace_reg, "")) %>%
#     mutate(id = row_number())
#
# }
#
# createVocab = function(tokenized.tweets, method = "2gram") {
#   if (method == "1gram") {
#     vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 1L))
#   } else if (method == "2gram") {
#     vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 2L))
#   } else if (method == "3gram") {
#     vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 3L))
#   } else if (method == "4gram") {
#     vocab = create_vocabulary(tokenized.tweets, ngram = c(1L, 4L))
#   }
# }
#
# tokenizerFunc = function(df, prep.fun = tolower, tok.fun = word_tokenizer) {
#
#   # df has columns $text and $id
#   itoken(df$text,
#          preprocessor = prep.fun,
#          tokenizer = tok.fun,
#          ids = df$id,
#          progressbar = TRUE)
# }
#
#
# trainSentimClassif = function(tweets.labeled, ngram) {
#   # train.data = preProcTweets(tweets.labeled)
#   tokenized = tokenizerFunc(tweets.labeled)
#   vocab = createVocab(tokenized, method = ngram)
#
#   # creating vocabulary and document-term matrix
#   vectorizer = vocab_vectorizer(vocab)
#   docterm = create_dtm(tokenized, vectorizer)
#   # define tf-idf model
#   tfidf = TfIdf$new()
#   # fit the model to the train data and transform it with the fitted model
#   docterm.tfidf = fit_transform(docterm, tfidf)
#   # train the model
#
#   glmnet.sentim.classif = cv.glmnet(x = docterm.tfidf,
#                                  y = tweets_train[['sentiment']],
#                                  family = 'binomial',
#                                  # L1 penalty
#                                  alpha = 1,
#                                  # interested in the area under ROC curve
#                                  type.measure = "auc",
#                                  # 5-fold cross-validation
#                                  nfolds = 5,
#                                  # high value is less accurate, but has faster training
#                                  thresh = 1e-3,
#                                  # again lower number of iterations for faster training
#                                  maxit = 1e3)
#   print(difftime(Sys.time(), t1, units = 'mins'))
#   plot(glmnet.sentim.classif)
#   return(list("classif" = glmnet.sentim.classif, "vocab" = vocab))
# }
#
# classifier = trainSentimClassif(tweets_train, ngram = "2gram")
# tweets = pullTweets("trump", "Tweets/tweets", interval = 5, geolocation = FALSE)
# tweets.df = tweets$text
# pred = getGlmnetSentiments(classifier$classif, classifier$vocab, tweets$text)
# table(pred)
#
# getGlmnetSentiments = function(classif, vocab, tweets.df) {
#
#   tweets.df = preProcTweets(tweets.df)
#   tokenized = tokenizerFunc(tweets.df)
#   # vocab = createVocab(tokenized, method = ngram)
#
#   # creating vocabulary and document-term matrix
#   vectorizer = vocab_vectorizer(vocab)
#   docterm = create_dtm(tokenized, vectorizer)
#   # define tf-idf model
#   tfidf = TfIdf$new()
#   # fit the model to the train data and transform it with the fitted model
#   docterm.tfidf = fit_transform(docterm, tfidf)
#   # train the model
#   preds = predict(classif, docterm.tfidf, type = 'response')[ , 1]
#   preds = sapply(preds, FUN = function(x) {
#     if (x < 0.4) {
#       return("negative")
#     } else if (x > 0.6) {
#       return("positive")
#     } else {
#       return("neutral")
#     }
#     })
#   return(preds)
# }
#
# tweets = pullTweets("trump", "Tweets/tweets", interval = 5, geolocation = FALSE)
# tweets.df = tweets$text
# pred = getGlmnetSentiments(classifier, tweets$text, vocab = vocab, ngram = "3gram")
# # table(pred)
# #
# # plot(pred)
# # length(tweets$text)
# # length(pred)
# #
# # # save the model for future using
# # saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
# # #########################################
# #
# tweets.training = as.tibble(readLines("Data/twitter.training.txt")) %>%
#   separate(value, into = c("sentiment", "text"), sep = "\t") %>%
#   mutate(id = row_number())
# save(tweets.training, file = "Data/tweets.training.rdata")
#
# tweets_train = data
# tweets_train = tweets.test
# tweets
# # l1 = c("id" = 1L, "sentiment" = 1L, "text" = "this is a good tweet")
# # l2 = c("id" = 2L, "sentiment" = 0L, "text" = "this is a bad tweet")
# # tweets_train = data.frame(rbind(l1 ,l2), stringsAsFactors = FALSE)
#
#
# # tsk = makeClassifTask(data = data, target = "sentiment")
#
# prep_fun <- tolower
# tok_fun <- word_tokenizer
# tweets_test = tweets.test
#
# it_train <- itoken(tweets_train$text,
#                    preprocessor = prep_fun,
#                    tokenizer = tok_fun,
#                    ids = tweets_train$id,
#                    progressbar = TRUE)
# it_test <- itoken(tweets_test$text,
#                   preprocessor = prep_fun,
#                   tokenizer = tok_fun,
#                   ids = tweets_test$id,
#                   progressbar = TRUE)
#
# # creating vocabulary and document-term matrix
# vocab <- create_vocabulary(it_train, ngram = c(1L, 2L))
#
# vectorizer <- vocab_vectorizer(vocab)
# dtm_train <- create_dtm(it_train, vectorizer)
#
# # define tf-idf model
# tfidf <- TfIdf$new()
# # fit the model to the train data and transform it with the fitted model
# dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
# # apply pre-trained tf-idf transformation to test data
# dtm_test_tfidf  <- create_dtm(it_test, vectorizer) %>%
#   transform(tfidf)
#
# t1 <- Sys.time()
# glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf,
#                                y = tweets_train[['sentiment']],
#                                family = 'binomial',
#                                # L1 penalty
#                                alpha = 1,
#                                # interested in the area under ROC curve
#                                type.measure = "auc",
#                                # 5-fold cross-validation
#                                nfolds = 5,
#                                # high value is less accurate, but has faster training
#                                thresh = 1e-3,
#                                # again lower number of iterations for faster training
#                                maxit = 1e3)
# print(difftime(Sys.time(), t1, units = 'mins'))
#
# glmnet_classifier
#
# plot(glmnet_classifier)
# print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
#
#
# preds <- predict(glmnet_classifier, dtm_train_tfidf, type = 'response')[ , 1]
#
# l1 = c("id" = 1L, "sentiment" = 1L, "text" = "this is a good tweet")
# l2 = c("id" = 2L, "sentiment" = 0L, "text" = "this is a bad tweet")
# l3 = c("id" = 3L, "sentiment" = 0L, "text" = "trump is a terrible president")
# tweets.test = data.frame(rbind(l1 ,l2, l3), stringsAsFactors = FALSE)
#
#
# preds = predict(glmnet_classifier, dtm_test_tfidf, type = "response")[ , 1]
#
#
# auc(as.numeric(tweets_test$sentiment), preds)
#
# # save the model for future using
# saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
#
# dtm_train_tfidf
#
#
