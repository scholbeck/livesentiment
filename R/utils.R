
organizeTweetsOneliner = function(raw.tweets) {

  replace_reg = "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  unnest_reg = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

  raw.tweets %>%
    as.tibble() %>%
    rename(text = value) %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    mutate(tweetnumber = row_number())
}


# cleanText = function(txt) {
#   txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
#   txt = gsub("@\\w+", "", txt)
#   txt = gsub("[[:punct:]]", "", txt)
#   txt = gsub("[[:digit:]]", "", txt)
#   txt = gsub("http\\w+", "", txt)
#   txt = gsub("[ \t]{2,}", "", txt)
#   txt = gsub("^\\s+|\\s+$", "", txt)
#   txt = gsub("amp", "", txt)
#   # define "tolower error handling" function
#   try.tolower = function(x)
#   {
#     y = NA
#     try_error = tryCatch(tolower(x), error = function(e) e)
#     if (!inherits(try_error, "error"))
#       y = tolower(x)
#     return(y)
#   }
#
#   txt = sapply(txt, try.tolower)
#   txt = txt[txt != ""]
#   names(txt) = NULL
#   return(txt)
# }

# clean.text = function(x)
# {
#   # tolower
#   x = tolower(x)
#   # remove rt
#   x = gsub("rt", "", x)
#   # remove at
#   x = gsub("@\\w+", "", x)
#   # remove punctuation
#   x = gsub("[[:punct:]]", "", x)
#   # remove numbers
#   x = gsub("[[:digit:]]", "", x)
#   # remove links http
#   x = gsub("http\\w+", "", x)
#   # remove tabs
#   x = gsub("[ |\t]{2,}", "", x)
#   # remove blank spaces at the beginning
#   x = gsub("^ ", "", x)
#   # remove blank spaces at the end
#   x = gsub(" $", "", x)
#   return(x)
# }

tweetMap = function(coords) {

  m = leaflet() %>%
    addTiles(
      group = "OSM",
      options = providerTileOptions(minZoom = 1, maxZoom = 5)
      ) %>%
    addCircleMarkers(data = coords, radius = 10) %>%
    setView(lng = -80, lat = 38, zoom = 3)
  return(m)
}

findRoots = function(d) {
  res = data.frame("tweetbatch" = c(), "cumsum" = c())
  for (i in 2:length(d$cumsum)) {
    if (d$cumsum[i] * d$cumsum[i-1] < 0) {
      a = as.numeric(d[i-1, ])
      b = as.numeric(d[i, ])
      r = data.frame("tweetbatch" = findLinearRoot(a, b), "cumsum" = 0)
      res = rbind(res, r)
    }
  }
  return(res)
}

findLinearRoot = function(a, b) {
  m = (b[2] - a[2]) / (b[1] - a[1])
  intercept = function(x) -1 * (m * x[1] - x[2])
  t = intercept(a)
  fn = function(x) {m * x + t}

  root = uniroot(fn, interval = c(a[1], b[1]))$root
  return(root)
}

getBingSentiments = function(tweets_oneliner, iter, filter = NULL,
                             lexicon = bing) {

  tweets_tokenized = organizeTweetsTokenized(tweets_oneliner, iter = iter)

  if (!is.null(filter) && (!filter == "NULL")) {
    filter.message = paste(
      sprintf("word == quote(%s)", filter), collapse = " | ")
    filter.message = paste("!(", filter.message, ")", sep = "")
    tweets_tokenized = filter(
      tweets_tokenized, eval(parse(text = filter.message))
      )
  } else {}

  sentiment <- tweets_tokenized %>%
    left_join(lexicon) %>%
    group_by(tweetnumber) %>%
    replace_na(list(sentiment = "neutral")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate("tweetbatch" = iter)
  if (!("positive" %in% colnames(sentiment))) {
    sentiment = mutate(sentiment, "positive" = 0)
    }
  if (!("neutral" %in% colnames(sentiment))) {
    sentiment = mutate(sentiment, "neutral" = 0)
    }
  if (!("negative" %in% colnames(sentiment))) {
    sentiment = mutate(sentiment, "negative" = 0)
    }
  sentiment = sentiment %>%
    mutate("diff" = positive - negative) %>%
    mutate(sentiment = {
      if (diff < 0) {return("negative")
      } else if (diff == 0) {return("neutral")
      } else if (diff > 0) {return("positive")}
    }) %>%
    ungroup()

  individual = left_join(tweets_oneliner, sentiment, by = "tweetnumber") %>%
    na.omit(sentiment)

  aggregated = sentiment %>%
    group_by(tweetbatch) %>%
    count(sentiment) %>%
    spread(sentiment, n)
  if (!("positive" %in% colnames(aggregated))) {
    aggregated = mutate(aggregated, positive = 0)
  }
  if (!("negative" %in% colnames(aggregated))) {
    aggregated = mutate(aggregated, negative = 0)
  }
  aggregated = aggregated %>%
    mutate(diff = positive - negative) %>%
    ungroup()

  return(list("individual" = individual, "aggregated" = aggregated))
}

getNrcSentiments = function(tweets, iter, filter = NULL, lexicon = nrc) {

  if (!is.null(filter) && (!filter == "NULL")) {
    filter.message = paste(sprintf("word == quote(%s)", filter),
                           collapse = " | ")
    filter.message = paste("!(", filter.message, ")", sep = "")
    tweets = filter(tweets, eval(parse(text = filter.message)))
  } else {}

  sentiment = tweets %>%
    inner_join(lexicon) %>%
    group_by(tweetnumber) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate("tweetbatch" = iter) %>%
    ungroup()

  aggregated = sentiment %>%
    group_by(tweetbatch) %>%
    select(-tweetnumber) %>%
    summarize_all(.funs = sum) %>%
    ungroup()

  return(list("individual" = sentiment, "aggregated" = aggregated))
}

getLoughranSentiments = function(tweets, iter, filter = NULL,
                                 lexicon = loughran) {

  if (!is.null(filter) && (!filter == "NULL")) {
    filter.message = paste(sprintf("word == quote(%s)", filter),
                           collapse = " | ")
    filter.message = paste("!(", filter.message, ")", sep = "")
    tweets = filter(tweets, eval(parse(text = filter.message)))
  } else {}

  sentiment = tweets %>%
    inner_join(lexicon) %>%
    group_by(tweetnumber) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate("tweetbatch" = iter) %>%
    ungroup()

  aggregated = sentiment %>%
    group_by(tweetbatch) %>%
    select(-tweetnumber) %>%
    summarize_all(.funs = sum) %>%
    ungroup()

  return(list("individual" = sentiment, "aggregated" = aggregated))
}

getAfinnSentiments = function(tweets, iter, filter = NULL, lexicon = afinn) {

  if (!is.null(filter) && (!filter == "NULL")) {
    filter.message = paste(sprintf("word == quote(%s)", filter),
                           collapse = " | ")
    filter.message = paste("!(", filter.message, ")", sep = "")
    tweets = filter(tweets, eval(parse(text = filter.message)))
  } else {}

  individual = tweets %>%
    inner_join(lexicon) %>%
    group_by(tweetnumber) %>%
    count(score) %>%
    mutate(weighted = score * n) %>%
    mutate(wordsum = sum(n)) %>%
    mutate(scoresum = sum(weighted)) %>%
    mutate(tweetbatch = iter) %>%
    select(tweetnumber, wordsum, scoresum, tweetbatch) %>%
    filter(!duplicated(tweetnumber)) %>%
    mutate(sentiment = round(scoresum/wordsum)) %>%
    ungroup()

  aggregated = individual %>%
    group_by(tweetbatch) %>%
    count(sentiment) %>%
    spread(sentiment, n) %>%
    ungroup() %>%
    mutate(sentiment = sum(individual$sentiment))

  return(list("individual" = individual, "aggregated" = aggregated))
}

organizeTweetsTokenized = function(tweets, iter, remove.stopwords = TRUE,
                                   filter = NULL) {

  replace_reg = "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
  unnest_reg = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

  organized.tweets = tweets %>%
    # rename(text = value) %>%
    # filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    mutate(text = str_replace_all(text, "@", "")) %>%
    mutate(tweetnumber = row_number()) %>%
    mutate(tweetbatch = iter) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg)
  if (remove.stopwords == TRUE) {
    # return(organized.tweets %>% filter(!word %in% stop_words$word, str_detect(word, "[a-z]")))
    return(organized.tweets %>% filter(!word %in% stopwords))
  } else {
    return(organized.tweets)
  }
}

### icon names

getIconNames = function() {
  file_text <- readr::read_file(
    paste0(
      .libPaths()[1],
      "/leaflet/htmlwidgets/plugins/Leaflet.awesome-markers/font-awesome.min.css")
  )
  icon_names <- stringr::str_extract_all(file_text, "(fa-)([^:]+)")[[1]]
  icon_names <- icon_names[-(1:36)] %>%
    stringr::str_sub(4, -1)
  icon_names

}
