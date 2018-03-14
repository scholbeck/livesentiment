# # loading packages
# library(twitteR)
# library(ROAuth)
# library(tidyverse)
# library(text2vec)
# library(caret)
# library(glmnet)
# library(ggrepel)
# library(purrrlyr)
#
#
#
# ### loading and preprocessing a training set of tweets
# # function for converting some symbols
# conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")
#
# ##### loading classified tweets ######
# # source: <a class="vglnk" href="http://help.sentiment140.com/for-students/" rel="nofollow"><span>http</span><span>://</span><span>help</span><span>.</span><span>sentiment140</span><span>.</span><span>com</span><span>/</span><span>for</span><span>-</span><span>students</span><span>/</span></a>
# # 0 - the polarity of the tweet (0 = negative, 4 = positive)
# # 1 - the id of the tweet
# # 2 - the date of the tweet
# # 3 - the query. If there is no query, then this value is NO_QUERY.
# # 4 - the user that tweeted
# # 5 - the text of the tweet
#
#
# all.tweets = read_csv('Data/training.1600000.processed.noemoticon.csv',
#                               col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
#   # converting some symbols
#   dmap_at('text', conv_fun) %>%
#   # replacing class values
#   mutate(sentiment = ifelse(sentiment == 0, 0, 1))
#
# # data splitting on train and test
#
# tweets_classified = all.tweets[sample(1600000, size = 1000), ]
#
# summary(tweets_classified$sentiment)
# unique(tweets_classified$sentiment)
#
# set.seed(2340)
# trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8,
#                                   list = FALSE,
#                                   times = 1)
# tweets_train <- tweets_classified[trainIndex, ]
# tweets_test <- tweets_classified[-trainIndex, ]
#
# ##### doc2vec #####
# # define preprocessing function and tokenization function
# prep_fun <- tolower
# tok_fun <- word_tokenizer
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
# vocab <- create_vocabulary(it_train)
# vectorizer <- vocab_vectorizer(vocab)
# dtm_train <- create_dtm(it_train, vectorizer)
# dtm_test <- create_dtm(it_test, vectorizer)
# # define tf-idf model
# tfidf <- TfIdf$new()
# # fit the model to the train data and transform it with the fitted model
# dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
# dtm_test_tfidf <- fit_transform(dtm_test, tfidf)
#
# # train the model
# t1 <- Sys.time()
# glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']],
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
# plot(glmnet_classifier)
# print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))
#
# preds <- predict(glmnet_classifier, dtm_test_tfidf, type = 'response')[ ,1]
# glmnet:::auc(as.numeric(tweets_test$sentiment), preds)
#
# # save the model for future using
# saveRDS(glmnet_classifier, 'glmnet_classifier.RDS')
# #########################################
#
# data = as.tibble(readLines("Data/twitter.training.txt")) %>%
#   separate(value, into = c("sentiment", "text"), sep = "\t") %>%
#   mutate(id = row_number())
# data
#
# tweets_train = data
# tweets_train = tweets.test
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
#                    preprocessor = prep_fun,
#                    tokenizer = tok_fun,
#                    ids = tweets_test$id,
#                    progressbar = TRUE)
#
# # creating vocabulary and document-term matrix
# vocab <- create_vocabulary(it_train)
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
