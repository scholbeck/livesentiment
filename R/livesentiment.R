pullTweets = function(keyword, file.path, tweets_old = NULL, interval,
                      geolocation = FALSE) {

  filterStream(file = file.path, track = c(keyword), timeout = interval,
               oauth = my_oauth, language = "en")
  tweets.new = tryCatch(parseTweets(file.path), error = function(e) {})
  q = length(tweets.new$text) - length(tweets_old$text)
  if (q != 0) {
    last.observations = tail(tweets.new, q)
    geotag.id = which(
      !is.na(last.observations$place_lon) & !is.na(last.observations$place_lat)
    )

    if (geolocation == TRUE) {
      # with geotag
      txt.geoenabled =
        last.observations$text[!is.na(last.observations$place_lat)]
      coords =
        data.frame(
          "lat" = last.observations$place_lat[!is.na(last.observations$place_lat)],
          "lng" = last.observations$place_lon[!is.na(last.observations$place_lon)])
      return(list("text" = txt.geoenabled, "coords" = coords))
    } else {
      # no geotag
      return(list("text" = last.observations$text, "coords" = NULL))
    }
  } else {
    return(NULL)
  }
}

#' Interactive sentiment analysis for real time twitter feeds
#'
#' This function calls a shiny based interactive user interface for analyzing
#' real time twitter feeds.
#'
#' Setting up a twitter oath (with an existing twitter account) is necessary
#' beforehand.
liveSentiment = function() {

  dir.create(file.path(getwd(), "tweets"))
  file.path = "tweets/tweets.json"
  if (file.exists(file.path)) {
    file.remove(file.path)
  } else {}

  sentiment.colors = c("negative" = "#F8766D", "positive" = "#00BFC4")
  # first is red, second is blue

  # ________________________________________________________________________
  # UI
  app.ui = dashboardPage(
    shinyjs::useShinyjs(),
    header = dashboardHeader(
      title = "Live Sentiment Analysis with Twitter",
      titleWidth = "400px",
      tags$li(class = "dropdown",
              actionButton(
                "reload", "Reload application",
                width = "100%",
                icon("refresh"),
                style = "font-size: 16px; color: #fff;
                background-color: #337ab7; border-color: #2e6da4;
                padding: 13px"
              )
      )
    ),
    sidebar = dashboardSidebar(
      disable = FALSE,
      switchInput("active", "Pull tweets", value = FALSE),
      HTML('<hr style="color: purple;">'),
      sidebarMenu(
        menuItem("Authentification", tabName = "auth", icon = icon("key")),
        menuItem("Controls", tabName = "controls", icon = icon("laptop")),
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Tweets", tabName = "tweets", icon = icon("twitter")),
        menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
        menuItem("Plots", tabName = "plots", icon = icon("bar-chart")),
        menuItem("Map", tabName = "map", icon = icon("globe"))
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "auth",
                h2("Twitter Authentification"),
                box(width = 12,
                    fileInput("oauth", "Select oauth.rdata file")
                )
        ),
        tabItem(tabName = "controls",
                h2("Controls"),
                box(width = 12,
                    switchInput("geolocation", "Location", value = FALSE),
                    HTML('&nbsp;'),
                    textInput(
                      "hashtag",
                      "Tweet keyword without hashtag (separate multiple words with single whitespace)", value = "trump"),
                    textInput(
                      "filter",
                      "Remove words (separate multiple words with single whitespace)"),
                    sliderInput(
                      "interval", "Choose download interval",
                      min = 2, max = 20, value = 5
                    ),
                    selectInput(
                      "method", "Choose sentiment classification method",
                      choices = c(
                        # "Lexicon: Afinn (Scale -5 to 5)",
                        "Lexicon: Bing (Scale 'positive' & 'negative')",
                        "Statistical model"),
                      selected = "Lexicon: Bing (Scale 'positive' & 'negative')"),
                    conditionalPanel(
                      condition = "input.method == 'Statistical model'",
                      selectInput(
                        "model", "Choose statistical model",
                        choices = c("glmNet"),
                        selected = "glmNet")
                    ),
                    conditionalPanel(
                      condition = "input.method == 'Statistical model'",
                      selectInput(
                        "ngram", "Choose ngram for document term matrix",
                        choices = c("1gram", "2gram", "3gram", "4gram"),
                        selected = "1gram")
                    ),
                    conditionalPanel(
                      condition = "input.method == 'Statistical model'",
                      actionButton("train", "Train classifier")
                    ),
                    HTML('&nbsp;'),
                    verbatimTextOutput("classif.summary"),
                    conditionalPanel(
                      condition = "input.method == 'Statistical model'",
                      sliderInput("threshold", "Decision boundary",
                                  min = 0, max = 1,
                                  value = c(0.4, 0.6)
                      )
                    )
                )
        ),
        tabItem(tabName = "summary",
                h2("Tweet summary statistics"),
                # tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 70px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                box(width = 12,
                    fluidRow(
                      infoBoxOutput("iter"),
                      infoBoxOutput("amount"),
                      # infoBoxOutput("nonclassifrate")
                      infoBoxOutput("rating")
                    ),
                    fluidRow(
                      infoBoxOutput("positive"),
                      infoBoxOutput("negative"),
                      infoBoxOutput("neutral")
                    )
                ),
                fluidRow(
                  box(width = 12,
                      h2("Marginal sentiment"),
                      plotOutput("sentiment.count")
                  ),
                  box(width = 12,
                      h2("Cumulative sentiment"),
                      plotOutput("cumsum.plot")
                  )
                )
        ),
        tabItem(tabName = "wordcloud",
                # box(width = 12,
                # fluidRow(
                #   column(width = 6,
                #          sliderInput("wordcloud.maxwords", "Maximum number of words in wordcloud", min = 5,
                #                      max = 50, value = 20)
                #   ),
                #   column(width = 6,
                #          sliderInput("wordcloud.fontsize", "Font size", min = 1,
                #                      max = 5, value = 2)
                #   )
                # ),
                h2("Positive / Negative categorized via Bing lexicon"),
                plotOutput("wordcloud", width = "1000", height = "1400px"),
                h2("Emotions categorized via NRC lexicon"),
                plotOutput("emotioncloud", width = "1100px", height = "1400px")
                # )
        ),
        tabItem(tabName = "tweets",
                h2("Tweets"),
                fluidRow(
                  # verbatimTextOutput("txt")
                  box(
                    width = NULL,
                    status = "primary",
                    div(style = "overflow-x: scroll",
                        tableOutput("tweettable")
                    )
                  )
                )
        ),
        tabItem(tabName = "plots",
                h2("Plots"),
                box(
                  width = 12,
                  sliderInput(
                    "wordcount.height", "Plot height", min = 300, max = 1500,
                    value = 300),
                  plotOutput("wordcount", height = "auto"),
                  conditionalPanel(
                    condition = "input.method == 'Statistical model'",
                    plotOutput("probplot")
                  )
                )
        ),
        tabItem(tabName = "map",
                h2("Map"),
                conditionalPanel(
                  condition = "input.geolocation",
                  leafletOutput("map"))
        ),
        tabItem(tabName = "internalcalc",
                h2("Internal calculations")
                # tabBox("Data")
        )
      )
    )
  )
  # ________________________________________________________________________
  # Server
  app.server = function(input, output, session) {

    observeEvent(input$reload, {session$reload()})

    observeEvent(input$oauth, {
      in_file <- isolate({input$oauth})
      file <- in_file$datapath
      # load the file into new environment and get it from there
      e = new.env()
      name <- load(file, envir = e)
      my_oauth <<- e[[name]]
      df$oauth_provided <- TRUE
    })

    observeEvent({
      input$method
      input$model}
      , ignoreNULL = FALSE, ignoreInit = FALSE, {
      if (input$method == "Lexicon: Afinn (Scale -5 to 5)") {
        df$lexicon = afinn
        df$method = "afinn"
      } else if (
        input$method == "Lexicon: Bing (Scale 'positive' & 'negative')") {
        df$lexicon = bing
        df$method = "bing"
      } else if (input$method == "Statistical model") {
        df$method = "statmodel"
      }
    })

    output$classif.summary = renderText(classifierSummary())

    classifierSummary = eventReactive(ignoreNULL = FALSE, {
      df$method
      input$train}
      , {
      if (df$method == "afinn" | df$method == "bing") {
        paste0("You are currently using the ", df$method, " lexicon.")
      } else if (df$model.trained == TRUE) {
        paste0(
          "You are currently using a ",
          df$model,
          " with a document term matrix consisting of ",
          df$ngram,"s."
          )
      } else if (df$model.trained == FALSE) {
        paste0(
          "No model trained. Please train model before continuing or select lexicon based approach.")
      }
    })

    df = reactiveValues(iter = 0, amount = 0, positive = 0, negative = 0,
                        nonclassifrate = "0%", lexicon = bing,
                        model.trained = FALSE, tweet_storage = NULL)

    observeEvent({
      input$ngram
      input$model}
      , {
        df$ngram = input$ngram
        df$model = input$model
      })

    observeEvent(input$train, {
      df$model.trained = TRUE
      df$classif = withProgress(
        expr = trainSentimClassif("glmNet", tweets.training, ngram = df$ngram),
        min = 0, max = 100, value = 100,
        message = "Training classifier..",
        detail = "Please wait.")
    })

    observeEvent({
      input$active
      df$model.trained
      input$method
      df$oauth_provided}, ignoreNULL = FALSE
      , {
        shinyjs::toggle("active", condition = {
          (df$oauth_provided == TRUE) &&
            (
              (df$model.trained == TRUE) | (df$method == "bing") |
                (df$method == "afinn")
            )
        })
        shinyjs::toggle("geolocation", condition = {input$active == FALSE})
        shinyjs::toggleState("hashtag", condition = {input$active == FALSE})
        shinyjs::toggleState("filter", condition = {input$active == FALSE})
        shinyjs::toggleState("interval", condition = {input$active == FALSE})
        shinyjs::toggleState("method", condition = {input$active == FALSE})
        shinyjs::toggleState("model", condition = {input$active == FALSE})
        shinyjs::toggleState("ngram", condition = {input$active == FALSE})
        shinyjs::toggleState("train", condition = {input$active == FALSE})
      })

    observeEvent({
      input$active}
      # input$wordcloud.maxwords
      # input$wordcloud.fontsize}
      , {
        df$active = input$active
        # df$wordcloud.maxwords = input$wordcloud.maxwords
        # df$wordcloud.fontsize = input$wordcloud.fontsize
      })

    observeEvent({
      input$filter
      input$hashtag}
      , {
        df$hashtag = strsplit(input$hashtag, " ")[[1]]
        if (input$filter != "") {
          df$filter = strsplit(input$filter, " ")[[1]]
        } else {
          df$filter = NULL
        }
      })

    observeEvent(input$interval, {
      df$interval = input$interval
    })

    observe({input$active
      if (df$active == TRUE) {
        invalidateLater(0, session)
        isolate({
          pulled.tweets = pullTweets(df$hashtag, file.path,
                                     tweets_old = df$tweet_storage,
                                     interval = df$interval,
                                     geolocation = input$geolocation)
          df$iter = df$iter + 1

          if (!is.null(pulled.tweets)) {
            tweets.text = pulled.tweets[["text"]]
            # df$tweet_storage = c(df$tweet_storage, pulled.tweets)
            # geolocation == TRUE
            if (!is.null(pulled.tweets[["coords"]])) {

              organized <- organizeTweetsOneliner(tweets.text)
              df$tweets.oneliner <- organized
              df$tweets.tokenized <- organizeTweetsTokenized(
                tweets = organized,
                iter = df$iter,
                filter = df$filter)
              # df$tweets.tokenized = organizeTweetsTokenized(tweets = df$tweets.oneliner, iter = df$iter, filter = df$filter)
              df$tweet_storage = c(df$tweet_storage, df$tweets.oneliner)
              df$amount <- nrow(df$tweet_storage)
              df$coords <- pulled.tweets[["coords"]]
              # df$tweets.tokenized.total = c(df$tweets.tokenized.total, df$tweets.tokenized)

              # geolocation == FALSE
            } else {

              organized <- organizeTweetsOneliner(tweets.text)
              df$tweets.oneliner <- organized
              df$tweets.tokenized <- organizeTweetsTokenized(tweets = organized,
                                                             iter = df$iter,
                                                             filter = df$filter)

              df$amount <- df$amount + nrow(df$tweets.oneliner)
              df$coords <- NULL
              df$tweet_storage = c(df$tweet_storage, df$tweets.oneliner)
              # df$tweets.tokenized.total = c(df$tweets.tokenized.total, df$tweets.tokenized)
            }
          } else {}
        })
      } else {}
    })

    # observeEvent(df$tweets.tokenized, {
    #   req(df$tweets.tokenized)
    #   req(df$bing.sentiment)
    #
    #   sentims = df$bing.sentiment[["individual"]][["sentiment"]]
    #   df$table = data.table("Tweet" = df$text, "Sentiment" = sentims)
    # })

    observeEvent({
      df$tweets.oneliner}
      , {

      if (df$method == "bing") {
        bing.sentims = getBingSentiments(
          df$tweets.oneliner, iter = df$iter,
          filter = df$filter)
        # df$table = data.table("Tweet" = (df$tweets.oneliner %>% select(text)), "Sentiment" = bing.sentims[["individual"]][["sentiment"]])
        df$table = data.table("Number" = bing.sentims$individual$tweetnumber,
                              "Tweets" = bing.sentims$individual$text,
                              "Sentiment" = bing.sentims$individual$sentiment)
        recent.sentiment.count = bing.sentims[["aggregated"]]
      } else if (df$method == "statmodel") {
        glm_sentims = getGlmnetSentiments(
          df$classif[["classif"]],
          df$classif[["vocab"]],
          tweets_oneline_format = df$tweets.oneliner,
          lower.threshold = df$threshold[[1]],
          upper.threshold = df$threshold[[2]]
        )
        df$probs = glm_sentims
        glm.aggregated = countStatModelSentiments(glm_sentims %>% select(-prob),
                                                  df$iter)
        df$table = glm_sentims %>%
          select(-prob) %>%
          rename(Number = tweetnumber, Tweets = text, Sentiment = sentiment)
        recent.sentiment.count = glm.aggregated
      }
      # recent.sentiment.count = {
      #   if (df$method == "bing") {
      #     return(df$bing.sentiment[["aggregated"]])
      #   } else if (df$method == "statmodel") {
      #     sentim = getGlmnetSentiments(df$classif[["classif"]], df$classif[["vocab"]],
      #                                  tweets.df = df$text)
      #     sentim = countStatModelSentiments(sentim, df$iter)
      #     return(sentim)
      #   }
      # }

      # recent.sentiment.count = df$bing.sentiment[["aggregated"]]

      df$sentiment.count = bind_rows(df$sentiment.count, recent.sentiment.count) %>%
        mutate(cumsum = cumsum(diff))

      recent.emotions = df$tweets.tokenized %>%
        inner_join(nrc) %>%
        count(word, sentiment, sort = TRUE)
      df$emotions = rbind(df$emotions, recent.emotions)

      df$negative = sum(df$sentiment.count[["negative"]] %>% na.omit())
      df$positive = sum(df$sentiment.count[["positive"]] %>% na.omit())
      df$neutral = sum(df$sentiment.count[["neutral"]] %>% na.omit())
      df$nonclassifrate = paste0(
        round(digits = 2,
              1-((df$negative + df$neutral + df$positive)/df$amount)) * 100,
        "%")
      recent.sentiment.wordcount = df$tweets.tokenized %>%
        inner_join(df$lexicon) %>%
        count(word, sentiment, sort = TRUE)
      df$sentiment.wordcount = rbind(
        df$sentiment.wordcount, recent.sentiment.wordcount
        )

      df$sentiment = df$tweets.tokenized %>%
        inner_join(df$lexicon) %>%
        count(tweetnumber, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

    })

    output$iter = renderInfoBox({infoBox("Pulls:",
                                         value = df$iter,
                                         icon = shiny::icon("download")
    )})

    output$amount = renderInfoBox({infoBox("Tweets:",
                                           value = df$amount,
                                           icon = shiny::icon("twitter")
    )})

    output$positive = renderInfoBox({infoBox("Positive:",
                                             value = df$positive,
                                             icon = shiny::icon("smile-o"))})

    output$negative = renderInfoBox({infoBox("Negative:",
                                             value = df$negative,
                                             icon = shiny::icon("frown-o"))})
    output$rating = renderInfoBox({
      infoBox("Rating:",
              value = (
                if(df$positive > df$negative) {
                  print("positive")
                } else if (df$positive < df$negative) {
                  print("negative")
                } else if (df$positive == df$negative) {
                  print("neutral")}
              ),
              icon = shiny::icon("filter"))
    })

    observeEvent(input$threshold, {
      df$threshold <- c(input$threshold[1], input$threshold[2])
    })

    # output$nonclassifrate = renderInfoBox({infoBox("Unclassif.:",
    #                                                value = df$nonclassifrate,
    #                                                icon = shiny::icon("chain-broken"))})
    output$neutral = renderInfoBox({infoBox("Neutral:",
                                                   value = df$neutral,
                                                   icon = shiny::icon("meh-o"))})

    output$sentiment.count = renderPlot(sentimCount())

    sentimCount = eventReactive(df$sentiment.count , {
      req(nrow(df$sentiment.count) >= 1)

      d = df$sentiment.count %>%
        select(-c(diff, cumsum)) %>%
        melt("tweetbatch")

      cols = c("negative" = "#F8766D", "positive" = "#00BFC4",
               "neutral" = "#F1D6AF")

      ggplot(data = d, aes(x = tweetbatch, y = value, group = variable)) +
        geom_bar(stat = "identity", aes(fill = variable)) +
        scale_fill_manual(values = cols) +
        labs(x = "Tweet batch", y = "Marginal sentiment")
    })

    output$wordcount = renderPlot({
      wordCount()}, height = function(x) input$wordcount.height
    )

    wordCount = eventReactive(df$sentiment.wordcount, {

      df = df$sentiment.wordcount %>%
        filter(n >= 5) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(word = reorder(word, n))

      ggplot(data = df, aes(word, n, fill = sentiment)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme(
          text = element_text(size = 20),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
        ylab("Contribution to sentiment") +
        scale_fill_manual(values = sentiment.colors)
    })

    output$timeseries = renderPlot(sentimentTimeseries())

    output$probplot = renderPlot({predictionPlot(df$probs, df$threshold[[1]],
                                                 df$threshold[[2]]
                                                 )
    })

    sentimentTimeseries = eventReactive(df$sentiment, {
      ggplot(df$sentiment, aes(tweetnumber, sentiment, fill = sentiment > 0)) +
        geom_bar(stat = "identity", show.legend = FALSE, position = "dodge")
    })

    output$wordcloud = renderPlot(sentimentWordcloud())

    sentimentWordcloud = eventReactive(df$sentiment.wordcount, {

      termfreq = df$sentiment.wordcount %>%
        acast(word ~ sentiment, value.var = "n", fill = 0)

      comparison.cloud(term.matrix = termfreq,
                       colors = sentiment.colors,
                       random.order = FALSE,
                       max.words = 200)
    })

    output$emotioncloud = renderPlot(emotionWordcloud())

    emotionWordcloud = eventReactive(df$emotions, {

      termfreq = df$emotions %>%
        acast(word ~ sentiment, value.var = "n", fill = 0)

      comparison.cloud(term.matrix = termfreq,
                       colors = brewer.pal(10, "Paired"),
                       random.order = FALSE,
                       max.words = 200)
    })

    observeEvent(df$coords, {
      if (!is.null(df$coords)) {
        leafletProxy("map", deferUntilFlush = FALSE) %>%
          addCircleMarkers(data = df$coords, radius = 10)
      } else {}
    })

    # output$txt = renderPrint({
    #   capture.output(df$text)
    # })

    output$tweettable = renderTable(df$table)

    output$cumsum.plot = renderPlot(cumSumPlot())

    cumSumPlot = eventReactive(df$sentiment.count, {
      req(nrow(df$sentiment.count) >= 2)

      d = df$sentiment.count[, c("tweetbatch", "cumsum")]
      roots = findRoots(d)
      d2 = rbind(d, roots)

      ggplot(data = d2, aes(x = tweetbatch, y = cumsum)) +
        geom_area(data = subset(d2, cumsum <= 0), fill = "#F8766D") +
        geom_area(data = subset(d2, cumsum >= 0), fill = "#00BFC4") +
        labs(x = "Tweet batch", y = "Cumulative sentiment")

    })

    output$map = renderLeaflet({
      leaflet() %>%
        addTiles(group = "OSM",
                 options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>%
        setView(lng = -80, lat = 38, zoom = 3)
    })

    outs = outputOptions(output)
    lapply(names(outs), function(name) {
      outputOptions(output, name, suspendWhenHidden = FALSE)
    })

  }
  shinyApp(app.ui, app.server)
}
#
# load_all()
# liveSentiment()
