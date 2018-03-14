#
#
# liveSentiment = function(internal.calc = FALSE) {
#
#   dir.create(file.path(getwd(), "tweets"))
#   file.path = "tweets/tweets.json"
#   if (file.exists(file.path)) {
#     file.remove(file.path)
#   } else {}
#
#   sentiment.colors = c("negative" = "#F8766D", "positive" = "#00BFC4")
#   # first is red, second is blue
#
#   # ________________________________________________________________________
#   # UI
#   app.ui = dashboardPage(
#     shinyjs::useShinyjs(),
#     header = dashboardHeader(
#       title = "Live Sentiment Analysis with Twitter",
#       titleWidth = "350px",
#       tags$li(class = "dropdown",
#               actionButton("reload", "Reload application",
#                            width = "100%",
#                            icon("refresh"),
#                            style = "font-size: 16px; color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 13px"))),
#     sidebar = dashboardSidebar(
#       disable = FALSE,
#       switchInput("active", "Pull tweets", value = FALSE),
#       switchInput("geolocation", "Location", value = FALSE),
#       textInput("hashtag", "Tweet keyword (separate multiple words with whitespace", value = "trump"),
#       textInput("filter", "Remove words (separate multiple words with whitespace)"),
#
#       selectInput("lexicon", "Choose sentiment lexicon",
#                   choices = c("Afinn: Scale -5 to 5",
#                               "Bing: Scale 'positive' & 'negative'",
#                               "Loughran: Scale 'positive' & 'negative'"),
#                   selected = "Bing: Scale 'positive' & 'negative'"),
#       sliderInput("interval", "Choose download interval", min = 2, max = 20,
#                   value = 5),
#       HTML('<hr style="color: purple;">'),
#       sidebarMenu(
#         menuItem("Controls", tabName = "controls", icon = icon("laptop")),
#         menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
#         menuItem("Tweets", tabName = "tweets", icon = icon("twitter")),
#         menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
#         menuItem("Plots", tabName = "plots", icon = icon("bar-chart")),
#         menuItem("Map", tabName = "map", icon = icon("globe")),
#         {
#           if (internal.calc == TRUE) {menuItem("Internal calculations",
#                                                tabName = "internalcalc",
#                                                icon = icon("cogs"))
#           } else {}
#         }
#       )
#     ),
#     body = dashboardBody(
#         tabItems(
#           tabItem(tabName = "controls",
#                   h2("Controls")
#           ),
#           tabItem(tabName = "summary",
#                   h2("Tweet summary statistics"),
#                   # tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 70px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
#                   box(width = 12,
#                       fluidRow(
#                         infoBoxOutput("iter"),
#                         infoBoxOutput("amount"),
#                         infoBoxOutput("nonclassifrate")
#                       ),
#                       fluidRow(
#                         infoBoxOutput("positive"),
#                         infoBoxOutput("negative"),
#                         infoBoxOutput("rating")
#                       )
#                   ),
#                   fluidRow(
#                     box(width = 12,
#                         h2("Marginal sentiment"),
#                         plotOutput("sentiment.count")
#                         ),
#                     box(width = 12,
#                         h2("Cumulative sentiment"),
#                         plotOutput("cumsum.plot")
#                     )
#                   )
#           ),
#           tabItem(tabName = "wordcloud",
#                   h2("Wordcloud"),
#                   # box(width = 12,
#                       # fluidRow(
#                       #   column(width = 6,
#                       #          sliderInput("wordcloud.maxwords", "Maximum number of words in wordcloud", min = 5,
#                       #                      max = 50, value = 20)
#                       #   ),
#                       #   column(width = 6,
#                       #          sliderInput("wordcloud.fontsize", "Font size", min = 1,
#                       #                      max = 5, value = 2)
#                       #   )
#                       # ),
#                   plotOutput("wordcloud", width = "1000", height = "1400px"),
#                   plotOutput("emotioncloud", width = "1100px", height = "1400px")
#                   # )
#           ),
#           tabItem(tabName = "tweets",
#                   h2("Tweets"),
#                   fluidRow(
#                     # verbatimTextOutput("txt")
#                     box(
#                       width = NULL,
#                       status = "primary",
#                       div(style = "overflow-x: scroll",
#                           DT::dataTableOutput("txt")
#                       )
#                     )
#                   )
#           ),
#           tabItem(tabName = "plots",
#                   h2("Plots"),
#                   box(width = 12,
#                       sliderInput("wordcount.height", "Plot height", min = 300,
#                                   max = 1500, value = 300),
#                       plotOutput("wordcount", height = "auto")
#                   )
#           ),
#           tabItem(tabName = "map",
#                   h2("Map"),
#                   conditionalPanel(condition = "input.geolocation", leafletOutput("map"))
#           ),
#           tabItem(tabName = "internalcalc",
#                   h2("Internal calculations")
#                   # tabBox("Data")
#           )
#         )
#       )
#     )
#   # ________________________________________________________________________
#   # Server
#   app.server = function(input, output, session) {
#
#     observeEvent(input$reload, {session$reload()})
#
#     observeEvent(input$lexicon, ignoreNULL = FALSE, ignoreInit = FALSE, {
#       if (input$lexicon == "Afinn: Scale -5 to 5") {
#         df$lexicon = afinn
#       } else if (input$lexicon == "Bing: Scale 'positive' & 'negative'") {
#         df$lexicon = bing
#       } else if (input$lexicon == "Loughran: Scale 'positive' & 'negative'") {
#         df$lexicon = loughran
#       }
#     })
#
#     df = reactiveValues(iter = 0, amount = 0, positive = 0, negative = 0,
#                         nonclassifrate = "0%", lexicon = bing)
#
#     # observe({input$active
#     #
#     #   if (input$active == TRUE) {
#     #     isolate({
#     #       shinyjs::disable("geolocation")
#     #     })
#     #   } else {}
#     # })
#
#     observeEvent(input$active, {
#       shinyjs::toggle("geolocation", condition = {input$active == FALSE})
#       shinyjs::toggleState("hashtag", condition = {input$active == FALSE})
#       shinyjs::toggleState("filter", condition = {input$active == FALSE})
#       shinyjs::toggleState("interval", condition = {input$active == FALSE})
#     })
#
#     observeEvent({
#       input$active}
#       # input$wordcloud.maxwords
#       # input$wordcloud.fontsize}
#       , {
#       df$active = input$active
#       # df$wordcloud.maxwords = input$wordcloud.maxwords
#       # df$wordcloud.fontsize = input$wordcloud.fontsize
#     })
#
#     observeEvent({
#       input$filter
#       input$hashtag}
#       , {
#         df$hashtag = strsplit(input$hashtag, " ")[[1]]
#         if (input$filter != "") {
#           df$filter = strsplit(input$filter, " ")[[1]]
#         } else {
#           df$filter = NULL
#         }
#       })
#
#     observeEvent(input$interval, {
#       df$interval = input$interval
#     })
#
#     observe({input$active
#       if (df$active == TRUE) {
#         invalidateLater(0, session)
#         isolate({
#           pulled.tweets = pullTweets(df$hashtag, file.path, interval = df$interval, geolocation = input$geolocation)
#           df$iter = df$iter + 1
#
#           if (!is.null(pulled.tweets)) {
#             # geolocation == TRUE
#             if (!is.null(pulled.tweets[["coords"]])) {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = pulled.tweets[["coords"]]
#               df$tweets.total = c(df$tweets.total, df$tweets)
#
#               # geolocation == FALSE
#             } else {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = NULL
#               df$tweets.total = c(df$tweets.total, df$tweets)
#             }
#           } else {}
#         })
#       } else {}
#     })
#
#     observeEvent(df$tweets, {
#       req(df$tweets)
#       req(df$bing.sentiment)
#
#       sentims = df$bing.sentiment[["individual"]][["sentiment"]]
#       df$table = data.table("Tweet" = df$text, "Sentiment" = sentims)
#     })
#
#     observeEvent(df$tweets, {
#
#       df$bing.sentiment = getBingSentiments(df$tweets, iter = df$iter, filter = df$filter)
#       recent.sentiment.count = df$bing.sentiment[["aggregated"]]
#
#       df$sentiment.count = bind_rows(df$sentiment.count, recent.sentiment.count) %>%
#         mutate(cumsum = cumsum(diff))
#       print(df$sentiment.count)
#
#       recent.emotions = df$tweets %>%
#         inner_join(nrc) %>%
#         count(word, sentiment, sort = TRUE)
#       df$emotions = rbind(df$emotions, recent.emotions)
#       print(df$emotions)
#
#       df$negative = sum(df$sentiment.count[["negative"]])
#       df$positive = sum(df$sentiment.count[["positive"]])
#       df$neutral = sum(df$sentiment.count[["neutral"]])
#       df$nonclassifrate = paste0(round(digits = 2,
#                                        1-((df$negative + df$neutral + df$positive)/df$amount)) * 100,
#                                  "%")
#
#       recent.sentiment.wordcount = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(word, sentiment, sort = TRUE)
#       df$sentiment.wordcount = rbind(df$sentiment.wordcount, recent.sentiment.wordcount)
#
#       df$sentiment = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(tweetnumber, sentiment) %>%
#         spread(sentiment, n, fill = 0) %>%
#         mutate(sentiment = positive - negative)
#
#     })
#
#     output$iter = renderInfoBox({infoBox("Pulls:",
#                                          value = df$iter,
#                                          icon = shiny::icon("download")
#                                          )})
#
#     output$amount = renderInfoBox({infoBox("Tweets:",
#                                            value = df$amount,
#                                            icon = shiny::icon("twitter")
#                                            )})
#
#     output$positive = renderInfoBox({infoBox("Positive:",
#                                              value = df$positive,
#                                              icon = shiny::icon("smile-o"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#     output$rating = renderInfoBox({infoBox("Rating:",
#                                            value = (
#                                              if(df$positive > df$negative) {
#                                                print("positive")
#                                              } else if (df$positive < df$negative) {
#                                                print("negative")
#                                              } else if (df$positive == df$negative) {
#                                                print("neutral")}
#                                              ),
#                                            icon = shiny::icon("filter"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#
#     output$nonclassifrate = renderInfoBox({infoBox("Unclassif.:",
#                                                    value = df$nonclassifrate,
#                                                    icon = shiny::icon("chain-broken"))})
#
#     output$sentiment.count = renderPlot(sentimCount())
#
#     sentimCount = eventReactive(df$sentiment.count , {
#       req(nrow(df$sentiment.count) >= 1)
#
#       d = df$sentiment.count %>%
#         select(-c(diff, cumsum)) %>%
#         melt("tweetbatch")
#
#       cols = c("negative" = "#F8766D", "positive" = "#00BFC4", "neutral" = "#F1D6AF")
#
#       ggplot(data = d, aes(x = tweetbatch, y = value, group = variable)) +
#         geom_bar(stat = "identity", aes(fill = variable)) +
#         scale_fill_manual(values = cols) +
#         # geom_area(aes(x = tweetbatch, y = positive), fill = "#00BFC4", show.legend = FALSE) +
#         # geom_area(aes(x = tweetbatch, y = -negative), fill = "#F8766D", show.legend = FALSE) +
#         labs(x = "Tweet batch", y = "Marginal sentiment")
#     })
#
#     output$wordcount = renderPlot({wordCount()}, height = function(x) input$wordcount.height)
#
#     wordCount = eventReactive(df$sentiment.wordcount, {
#
#         df = df$sentiment.wordcount %>%
#           filter(n >= 5) %>%
#           mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
#           mutate(word = reorder(word, n))
#
#         ggplot(data = df, aes(word, n, fill = sentiment)) +
#           geom_bar(stat = "identity") +
#           coord_flip() +
#           theme(
#             text = element_text(size = 20),
#             axis.text.x = element_text(angle = 90, hjust = 1)) +
#           ylab("Contribution to sentiment") +
#           scale_fill_manual(values = sentiment.colors)
#     })
#
#     output$timeseries = renderPlot(sentimentTimeseries())
#
#     sentimentTimeseries = eventReactive(df$sentiment, {
#       ggplot(df$sentiment, aes(tweetnumber, sentiment, fill = sentiment > 0)) +
#         geom_bar(stat = "identity", show.legend = FALSE, position = "dodge")
#     })
#
#     output$wordcloud = renderPlot(sentimentWordcloud())
#
#     sentimentWordcloud = eventReactive(df$sentiment.wordcount, {
#
#       termfreq = df$sentiment.wordcount %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = sentiment.colors,
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#     output$emotioncloud = renderPlot(emotionWordcloud())
#
#     emotionWordcloud = eventReactive(df$emotions, {
#
#       termfreq = df$emotions %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = brewer.pal(10, "Paired"),
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#
#     observeEvent(df$coords, {
#       if (!is.null(df$coords)) {
#         leafletProxy("map", deferUntilFlush = FALSE) %>%
#           addCircleMarkers(data = df$coords, radius = 10)
#       } else {}
#     })
#
#     # output$txt = renderPrint({
#     #   capture.output(df$text)
#     # })
#
#     output$txt = DT::renderDataTable(df$table)
#
#     output$cumsum.plot = renderPlot(cumSumPlot())
#
#     cumSumPlot = eventReactive(df$sentiment.count, {
#       req(nrow(df$sentiment.count) >= 2)
#
#       d = df$sentiment.count[, c("tweetbatch", "cumsum")]
#       roots = findRoots(d)
#       d2 = rbind(d, roots)
#
#       ggplot(data = d2, aes(x = tweetbatch, y = cumsum)) +
#         geom_area(data = subset(d2, cumsum <= 0), fill = "#F8766D") +
#         geom_area(data = subset(d2, cumsum >= 0), fill = "#00BFC4") +
#         labs(x = "Tweet batch", y = "Cumulative sentiment")
#
#     })
#
#     output$map = renderLeaflet({
#       leaflet() %>%
#         addTiles(group = "OSM", options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>%
#         setView(lng = -80, lat = 38, zoom = 3)
#     })
#
#     outs = outputOptions(output)
#     lapply(names(outs), function(name) {
#       outputOptions(output, name, suspendWhenHidden = FALSE)
#     })
#
#   }
#   shinyApp(app.ui, app.server)
# }
#
# # library(shiny)
# #
# # shinyApp(
# #   ui = fluidPage(
# #     useShinyjs(),  # Set up shinyjs
# #     # actionButton("btn", "Click me")
# #     switchInput("btn"),
# #     # textInput("text", "Text")
# #     switchInput("text")
# #   ),
# #   server = function(input, output) {
# #     observeEvent(input$btn, {
# #       # Change the following line for more examples
# #       toggle("text", condition = input$btn == FALSE)
# #     })
# #   }
# # )
# # }
#
#
#
# liveSentiment.mod = function(internal.calc = FALSE) {
#
#   dir.create(file.path(getwd(), "tweets"))
#   file.path = "tweets/tweets.json"
#   if (file.exists(file.path)) {
#     file.remove(file.path)
#   } else {}
#
#   sentiment.colors = c("negative" = "#F8766D", "positive" = "#00BFC4")
#   # first is red, second is blue
#
#   # ________________________________________________________________________
#   # UI
#   app.ui = dashboardPage(
#     shinyjs::useShinyjs(),
#     header = dashboardHeader(
#       title = "Live Sentiment Analysis with Twitter",
#       titleWidth = "350px",
#       tags$li(class = "dropdown",
#               actionButton("reload", "Reload application",
#                            width = "100%",
#                            icon("refresh"),
#                            style = "font-size: 16px; color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 13px"))),
#     sidebar = dashboardSidebar(
#       disable = FALSE,
#       switchInput("active", "Pull tweets", value = FALSE),
#       HTML('<hr style="color: purple;">'),
#       sidebarMenu(
#         menuItem("Controls", tabName = "controls", icon = icon("laptop")),
#         menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
#         menuItem("Tweets", tabName = "tweets", icon = icon("twitter")),
#         menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
#         menuItem("Plots", tabName = "plots", icon = icon("bar-chart")),
#         menuItem("Map", tabName = "map", icon = icon("globe")),
#         {
#           if (internal.calc == TRUE) {menuItem("Internal calculations",
#                                                tabName = "internalcalc",
#                                                icon = icon("cogs"))
#           } else {}
#         }
#       )
#     ),
#     body = dashboardBody(
#       tabItems(
#         tabItem(tabName = "controls",
#                 h2("Controls"),
#                 box(width = 12,
#                     switchInput("geolocation", "Location", value = FALSE),
#                     textInput("hashtag", "Tweet keyword without hashtag (separate multiple words with single whitespace)", value = "trump"),
#                     textInput("filter", "Remove words (separate multiple words with single whitespace"),
#                     sliderInput("interval", "Choose download interval", min = 2, max = 20,
#                                 value = 5),
#                     selectInput("lexicon", "Choose sentiment lexicon",
#                                 choices = c("Lexicon: Afinn (Scale -5 to 5)",
#                                             "Lexicon: Bing (Scale 'positive' & 'negative')",
#                                             "Statistical model"),
#                                 selected = "Lexicon: Bing (Scale 'positive' & 'negative')"),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      selectInput("model", "Choose statistical model",
#                                                  choices = c("glmNet"),
#                                                  selected = "glmNet")
#                                      ),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      selectInput("ngram", "Choose ngram for document term matrix",
#                                                  choices = c("1gram", "2gram", "3gram", "4gram"),
#                                                  selected = "1gram")
#                     ),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      actionButton("train", "Train classifier")
#                     )
#                 )
#         ),
#         tabItem(tabName = "summary",
#                 h2("Tweet summary statistics"),
#                 # tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 70px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
#                 box(width = 12,
#                     fluidRow(
#                       infoBoxOutput("iter"),
#                       infoBoxOutput("amount"),
#                       infoBoxOutput("nonclassifrate")
#                     ),
#                     fluidRow(
#                       infoBoxOutput("positive"),
#                       infoBoxOutput("negative"),
#                       infoBoxOutput("rating")
#                     )
#                 ),
#                 fluidRow(
#                   box(width = 12,
#                       h2("Marginal sentiment"),
#                       plotOutput("sentiment.count")
#                   ),
#                   box(width = 12,
#                       h2("Cumulative sentiment"),
#                       plotOutput("cumsum.plot")
#                   )
#                 )
#         ),
#         tabItem(tabName = "wordcloud",
#                 h2("Wordcloud"),
#                 # box(width = 12,
#                 # fluidRow(
#                 #   column(width = 6,
#                 #          sliderInput("wordcloud.maxwords", "Maximum number of words in wordcloud", min = 5,
#                 #                      max = 50, value = 20)
#                 #   ),
#                 #   column(width = 6,
#                 #          sliderInput("wordcloud.fontsize", "Font size", min = 1,
#                 #                      max = 5, value = 2)
#                 #   )
#                 # ),
#                 plotOutput("wordcloud", width = "1000", height = "1400px"),
#                 plotOutput("emotioncloud", width = "1100px", height = "1400px")
#                 # )
#         ),
#         tabItem(tabName = "tweets",
#                 h2("Tweets"),
#                 fluidRow(
#                   # verbatimTextOutput("txt")
#                   box(
#                     width = NULL,
#                     status = "primary",
#                     div(style = "overflow-x: scroll",
#                         DT::dataTableOutput("txt")
#                     )
#                   )
#                 )
#         ),
#         tabItem(tabName = "plots",
#                 h2("Plots"),
#                 box(width = 12,
#                     sliderInput("wordcount.height", "Plot height", min = 300,
#                                 max = 1500, value = 300),
#                     plotOutput("wordcount", height = "auto")
#                 )
#         ),
#         tabItem(tabName = "map",
#                 h2("Map"),
#                 conditionalPanel(condition = "input.geolocation", leafletOutput("map"))
#         ),
#         tabItem(tabName = "internalcalc",
#                 h2("Internal calculations")
#                 # tabBox("Data")
#         )
#       )
#     )
#   )
#   # ________________________________________________________________________
#   # Server
#   app.server = function(input, output, session) {
#
#     observeEvent(input$reload, {session$reload()})
#
#     observeEvent(input$lexicon, ignoreNULL = FALSE, ignoreInit = FALSE, {
#       if (input$lexicon == "Afinn: Scale -5 to 5") {
#         df$lexicon = afinn
#       } else if (input$lexicon == "Bing: Scale 'positive' & 'negative'") {
#         df$lexicon = bing
#       } else if (input$lexicon == "Loughran: Scale 'positive' & 'negative'") {
#         df$lexicon = loughran
#       }
#     })
#
#     df = reactiveValues(iter = 0, amount = 0, positive = 0, negative = 0,
#                         nonclassifrate = "0%", lexicon = bing)
#
#     observeEvent(input$ngram, {
#       df$ngram = input$ngram
#     })
#
#     observeEvent(input$train, {
#       df$classif = withProgress(
#         expr = trainGlmNet(tweets.training, ngram = df$ngram),
#         min = 0, max = 100, value = 100,
#         message = "Training classifier..",
#         detail = "Please wait.")
#     })
#
#     observeEvent(input$active, {
#       shinyjs::toggle("geolocation", condition = {input$active == FALSE})
#       shinyjs::toggleState("hashtag", condition = {input$active == FALSE})
#       shinyjs::toggleState("filter", condition = {input$active == FALSE})
#       shinyjs::toggleState("lexicon", condition = {input$active == FALSE})
#       shinyjs::toggleState("interval", condition = {input$active == FALSE})
#     })
#
#     observeEvent({
#       input$active}
#       # input$wordcloud.maxwords
#       # input$wordcloud.fontsize}
#       , {
#         df$active = input$active
#         # df$wordcloud.maxwords = input$wordcloud.maxwords
#         # df$wordcloud.fontsize = input$wordcloud.fontsize
#       })
#
#     observeEvent({
#       input$filter
#       input$hashtag}
#       , {
#         df$hashtag = strsplit(input$hashtag, " ")[[1]]
#         if (input$filter != "") {
#           df$filter = strsplit(input$filter, " ")[[1]]
#         } else {
#           df$filter = NULL
#         }
#       })
#
#     observeEvent(input$interval, {
#       df$interval = input$interval
#     })
#
#     observe({input$active
#       if (df$active == TRUE) {
#         invalidateLater(0, session)
#         isolate({
#           pulled.tweets = pullTweets(df$hashtag, file.path, interval = df$interval, geolocation = input$geolocation)
#           df$iter = df$iter + 1
#
#           if (!is.null(pulled.tweets)) {
#             # geolocation == TRUE
#             if (!is.null(pulled.tweets[["coords"]])) {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = pulled.tweets[["coords"]]
#               df$tweets.total = c(df$tweets.total, df$tweets)
#
#               # geolocation == FALSE
#             } else {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = NULL
#               df$tweets.total = c(df$tweets.total, df$tweets)
#             }
#           } else {}
#         })
#       } else {}
#     })
#
#     observeEvent(df$tweets, {
#       req(df$tweets)
#       req(df$bing.sentiment)
#
#       sentims = df$bing.sentiment[["individual"]][["sentiment"]]
#       df$table = data.table("Tweet" = df$text, "Sentiment" = sentims)
#     })
#
#     observeEvent(df$tweets, {
#
#       df$bing.sentiment = getBingSentiments(df$tweets, iter = df$iter, filter = df$filter)
#       recent.sentiment.count = df$bing.sentiment[["aggregated"]]
#       # recent.sentiment.count = {
#       #   if (identical(df$lexicon, bing)) {
#       #     getBingSentiments(tweets = df$tweets, iter = df$iter, filter = df$filter)
#       #   } else if (identical(df$lexicon, afinn)) {
#       #     getAfinnSentiments(tweets = df$tweets, iter = df$iter, filter = df$filter)
#       #   }
#       # }
#       # if (identical(df$lexicon, bing)) {
#       #   getBingSentiments(tweets = df$tweets, iter = df$iter)
#       #
#       # } else if (identical(df$lexicon, loughran)) {
#       #   getLoughranSentiments(tweets = df$tweets, iter = df$iter)
#       #
#       # } else if (identical(df$lexicon, afinn)) {
#       #   getAfinnSentiments(tweets = df$tweets, iter = df$iter)
#       # }
#
#       df$sentiment.count = bind_rows(df$sentiment.count, recent.sentiment.count) %>%
#         mutate(cumsum = cumsum(diff))
#       print(df$sentiment.count)
#
#       recent.emotions = df$tweets %>%
#         inner_join(nrc) %>%
#         count(word, sentiment, sort = TRUE)
#       df$emotions = rbind(df$emotions, recent.emotions)
#       print(df$emotions)
#
#       df$negative = sum(df$sentiment.count[["negative"]])
#       df$positive = sum(df$sentiment.count[["positive"]])
#       df$neutral = sum(df$sentiment.count[["neutral"]])
#       df$nonclassifrate = paste0(round(digits = 2,
#                                        1-((df$negative + df$neutral + df$positive)/df$amount)) * 100,
#                                  "%")
#
#       recent.sentiment.wordcount = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(word, sentiment, sort = TRUE)
#       df$sentiment.wordcount = rbind(df$sentiment.wordcount, recent.sentiment.wordcount)
#
#       df$sentiment = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(tweetnumber, sentiment) %>%
#         spread(sentiment, n, fill = 0) %>%
#         mutate(sentiment = positive - negative)
#
#     })
#
#     output$iter = renderInfoBox({infoBox("Pulls:",
#                                          value = df$iter,
#                                          icon = shiny::icon("download")
#     )})
#
#     output$amount = renderInfoBox({infoBox("Tweets:",
#                                            value = df$amount,
#                                            icon = shiny::icon("twitter")
#     )})
#
#     output$positive = renderInfoBox({infoBox("Positive:",
#                                              value = df$positive,
#                                              icon = shiny::icon("smile-o"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#     output$rating = renderInfoBox({infoBox("Rating:",
#                                            value = (
#                                              if(df$positive > df$negative) {
#                                                print("positive")
#                                              } else if (df$positive < df$negative) {
#                                                print("negative")
#                                              } else if (df$positive == df$negative) {
#                                                print("neutral")}
#                                            ),
#                                            icon = shiny::icon("filter"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#
#     output$nonclassifrate = renderInfoBox({infoBox("Unclassif.:",
#                                                    value = df$nonclassifrate,
#                                                    icon = shiny::icon("chain-broken"))})
#
#     output$sentiment.count = renderPlot(sentimCount())
#
#     sentimCount = eventReactive(df$sentiment.count , {
#       req(nrow(df$sentiment.count) >= 1)
#
#       d = df$sentiment.count %>%
#         select(-c(diff, cumsum)) %>%
#         melt("tweetbatch")
#
#       cols = c("negative" = "#F8766D", "positive" = "#00BFC4", "neutral" = "#F1D6AF")
#
#       ggplot(data = d, aes(x = tweetbatch, y = value, group = variable)) +
#         geom_bar(stat = "identity", aes(fill = variable)) +
#         scale_fill_manual(values = cols) +
#         # geom_area(aes(x = tweetbatch, y = positive), fill = "#00BFC4", show.legend = FALSE) +
#         # geom_area(aes(x = tweetbatch, y = -negative), fill = "#F8766D", show.legend = FALSE) +
#         labs(x = "Tweet batch", y = "Marginal sentiment")
#     })
#
#     output$wordcount = renderPlot({wordCount()}, height = function(x) input$wordcount.height)
#
#     wordCount = eventReactive(df$sentiment.wordcount, {
#
#       df = df$sentiment.wordcount %>%
#         filter(n >= 5) %>%
#         mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
#         mutate(word = reorder(word, n))
#
#       ggplot(data = df, aes(word, n, fill = sentiment)) +
#         geom_bar(stat = "identity") +
#         coord_flip() +
#         theme(
#           text = element_text(size = 20),
#           axis.text.x = element_text(angle = 90, hjust = 1)) +
#         ylab("Contribution to sentiment") +
#         scale_fill_manual(values = sentiment.colors)
#     })
#
#     output$timeseries = renderPlot(sentimentTimeseries())
#
#     sentimentTimeseries = eventReactive(df$sentiment, {
#       ggplot(df$sentiment, aes(tweetnumber, sentiment, fill = sentiment > 0)) +
#         geom_bar(stat = "identity", show.legend = FALSE, position = "dodge")
#     })
#
#     output$wordcloud = renderPlot(sentimentWordcloud())
#
#     sentimentWordcloud = eventReactive(df$sentiment.wordcount, {
#
#       termfreq = df$sentiment.wordcount %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = sentiment.colors,
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#     output$emotioncloud = renderPlot(emotionWordcloud())
#
#     emotionWordcloud = eventReactive(df$emotions, {
#
#       termfreq = df$emotions %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = brewer.pal(10, "Paired"),
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#
#     observeEvent(df$coords, {
#       if (!is.null(df$coords)) {
#         leafletProxy("map", deferUntilFlush = FALSE) %>%
#           addCircleMarkers(data = df$coords, radius = 10)
#       } else {}
#     })
#
#     # output$txt = renderPrint({
#     #   capture.output(df$text)
#     # })
#
#     output$txt = DT::renderDataTable(df$table)
#
#     output$cumsum.plot = renderPlot(cumSumPlot())
#
#     cumSumPlot = eventReactive(df$sentiment.count, {
#       req(nrow(df$sentiment.count) >= 2)
#
#       d = df$sentiment.count[, c("tweetbatch", "cumsum")]
#       roots = findRoots(d)
#       d2 = rbind(d, roots)
#
#       ggplot(data = d2, aes(x = tweetbatch, y = cumsum)) +
#         geom_area(data = subset(d2, cumsum <= 0), fill = "#F8766D") +
#         geom_area(data = subset(d2, cumsum >= 0), fill = "#00BFC4") +
#         labs(x = "Tweet batch", y = "Cumulative sentiment")
#
#     })
#
#     output$map = renderLeaflet({
#       leaflet() %>%
#         addTiles(group = "OSM", options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>%
#         setView(lng = -80, lat = 38, zoom = 3)
#     })
#
#     outs = outputOptions(output)
#     lapply(names(outs), function(name) {
#       outputOptions(output, name, suspendWhenHidden = FALSE)
#     })
#
#   }
#   shinyApp(app.ui, app.server)
# }
#
#
#
#
#
# liveSentiment.mod2 = function(internal.calc = FALSE) {
#
#   dir.create(file.path(getwd(), "tweets"))
#   file.path = "tweets/tweets.json"
#   if (file.exists(file.path)) {
#     file.remove(file.path)
#   } else {}
#
#   sentiment.colors = c("negative" = "#F8766D", "positive" = "#00BFC4")
#   # first is red, second is blue
#
#   # ________________________________________________________________________
#   # UI
#   app.ui = dashboardPage(
#     shinyjs::useShinyjs(),
#     header = dashboardHeader(
#       title = "Live Sentiment Analysis with Twitter",
#       titleWidth = "350px",
#       tags$li(class = "dropdown",
#               actionButton("reload", "Reload application",
#                            width = "100%",
#                            icon("refresh"),
#                            style = "font-size: 16px; color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 13px"))),
#     sidebar = dashboardSidebar(
#       disable = FALSE,
#       switchInput("active", "Pull tweets", value = FALSE),
#       HTML('<hr style="color: purple;">'),
#       sidebarMenu(
#         menuItem("Controls", tabName = "controls", icon = icon("laptop")),
#         menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
#         menuItem("Tweets", tabName = "tweets", icon = icon("twitter")),
#         menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud")),
#         menuItem("Plots", tabName = "plots", icon = icon("bar-chart")),
#         menuItem("Map", tabName = "map", icon = icon("globe")),
#         {
#           if (internal.calc == TRUE) {menuItem("Internal calculations",
#                                                tabName = "internalcalc",
#                                                icon = icon("cogs"))
#           } else {}
#         }
#       )
#     ),
#     body = dashboardBody(
#       tabItems(
#         tabItem(tabName = "controls",
#                 h2("Controls"),
#                 box(width = 12,
#                     switchInput("geolocation", "Location", value = FALSE),
#                     textInput("hashtag", "Tweet keyword without hashtag (separate multiple words with single whitespace)", value = "trump"),
#                     textInput("filter", "Remove words (separate multiple words with single whitespace"),
#                     sliderInput("interval", "Choose download interval", min = 2, max = 20,
#                                 value = 5),
#                     selectInput("method", "Choose sentiment lexicon",
#                                 choices = c("Lexicon: Afinn (Scale -5 to 5)",
#                                             "Lexicon: Bing (Scale 'positive' & 'negative')",
#                                             "Statistical model"),
#                                 selected = "Lexicon: Bing (Scale 'positive' & 'negative')"),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      selectInput("model", "Choose statistical model",
#                                                  choices = c("glmNet"),
#                                                  selected = "glmNet")
#                     ),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      selectInput("ngram", "Choose ngram for document term matrix",
#                                                  choices = c("1gram", "2gram", "3gram", "4gram"),
#                                                  selected = "1gram")
#                     ),
#                     conditionalPanel(condition = "input.lexicon == 'Statistical model'",
#                                      actionButton("train", "Train classifier")
#                     )
#                 )
#         ),
#         tabItem(tabName = "summary",
#                 h2("Tweet summary statistics"),
#                 # tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 70px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
#                 box(width = 12,
#                     fluidRow(
#                       infoBoxOutput("iter"),
#                       infoBoxOutput("amount"),
#                       infoBoxOutput("nonclassifrate")
#                     ),
#                     fluidRow(
#                       infoBoxOutput("positive"),
#                       infoBoxOutput("negative"),
#                       infoBoxOutput("rating")
#                     )
#                 ),
#                 fluidRow(
#                   box(width = 12,
#                       h2("Marginal sentiment"),
#                       plotOutput("sentiment.count")
#                   ),
#                   box(width = 12,
#                       h2("Cumulative sentiment"),
#                       plotOutput("cumsum.plot")
#                   )
#                 )
#         ),
#         tabItem(tabName = "wordcloud",
#                 h2("Wordcloud"),
#                 # box(width = 12,
#                 # fluidRow(
#                 #   column(width = 6,
#                 #          sliderInput("wordcloud.maxwords", "Maximum number of words in wordcloud", min = 5,
#                 #                      max = 50, value = 20)
#                 #   ),
#                 #   column(width = 6,
#                 #          sliderInput("wordcloud.fontsize", "Font size", min = 1,
#                 #                      max = 5, value = 2)
#                 #   )
#                 # ),
#                 plotOutput("wordcloud", width = "1000", height = "1400px"),
#                 plotOutput("emotioncloud", width = "1100px", height = "1400px")
#                 # )
#         ),
#         tabItem(tabName = "tweets",
#                 h2("Tweets"),
#                 fluidRow(
#                   # verbatimTextOutput("txt")
#                   box(
#                     width = NULL,
#                     status = "primary",
#                     div(style = "overflow-x: scroll",
#                         DT::dataTableOutput("txt")
#                     )
#                   )
#                 )
#         ),
#         tabItem(tabName = "plots",
#                 h2("Plots"),
#                 box(width = 12,
#                     sliderInput("wordcount.height", "Plot height", min = 300,
#                                 max = 1500, value = 300),
#                     plotOutput("wordcount", height = "auto")
#                 )
#         ),
#         tabItem(tabName = "map",
#                 h2("Map"),
#                 conditionalPanel(condition = "input.geolocation", leafletOutput("map"))
#         ),
#         tabItem(tabName = "internalcalc",
#                 h2("Internal calculations")
#                 # tabBox("Data")
#         )
#       )
#     )
#   )
#   # ________________________________________________________________________
#   # Server
#   app.server = function(input, output, session) {
#
#     observeEvent(input$reload, {session$reload()})
#
#     # observeEvent(input$method, ignoreNULL = FALSE, ignoreInit = FALSE, {
#     #   if (input$method == "Afinn: Scale -5 to 5") {
#     #     df$lexicon = afinn
#     #   } else if (input$method == "Bing: Scale 'positive' & 'negative'") {
#     #     df$lexicon = bing
#     #   } else if (input$method == "Statistical model") {
#     #     #....
#     #   }
#     # })
#
#     df = reactiveValues(iter = 0, amount = 0, positive = 0, negative = 0,
#                         nonclassifrate = "0%", lexicon = bing)
#
#     observeEvent(input$ngram, {
#       df$ngram = input$ngram
#     })
#
#     observeEvent(input$train, {
#       df$classif = withProgress(
#         expr = trainSentimClassif("glmNet", tweets.training, ngram = df$ngram),
#         min = 0, max = 100, value = 100,
#         message = "Training classifier..",
#         detail = "Please wait.")
#     })
#
#     observeEvent(input$active, {
#       shinyjs::toggle("geolocation", condition = {input$active == FALSE})
#       shinyjs::toggleState("hashtag", condition = {input$active == FALSE})
#       shinyjs::toggleState("filter", condition = {input$active == FALSE})
#       shinyjs::toggleState("lexicon", condition = {input$active == FALSE})
#       shinyjs::toggleState("interval", condition = {input$active == FALSE})
#     })
#
#     observeEvent({
#       input$active}
#       # input$wordcloud.maxwords
#       # input$wordcloud.fontsize}
#       , {
#         df$active = input$active
#         # df$wordcloud.maxwords = input$wordcloud.maxwords
#         # df$wordcloud.fontsize = input$wordcloud.fontsize
#       })
#
#     observeEvent({
#       input$filter
#       input$hashtag}
#       , {
#         df$hashtag = strsplit(input$hashtag, " ")[[1]]
#         if (input$filter != "") {
#           df$filter = strsplit(input$filter, " ")[[1]]
#         } else {
#           df$filter = NULL
#         }
#       })
#
#     observeEvent(input$interval, {
#       df$interval = input$interval
#     })
#
#     observe({input$active
#       if (df$active == TRUE) {
#         invalidateLater(0, session)
#         isolate({
#           pulled.tweets = pullTweets(df$hashtag, file.path, interval = df$interval, geolocation = input$geolocation)
#           df$iter = df$iter + 1
#
#           if (!is.null(pulled.tweets)) {
#             # geolocation == TRUE
#             if (!is.null(pulled.tweets[["coords"]])) {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = pulled.tweets[["coords"]]
#               df$tweets.total = c(df$tweets.total, df$tweets)
#
#               # geolocation == FALSE
#             } else {
#               tweets.text = pulled.tweets[["text"]]
#
#               df$text = tweets.text
#               df$tweets = organizeTweetsTokenized(tweets = tweets.text, iter = df$iter, filter = df$filter)
#               df$amount = df$amount + nrow(df$tweets)
#               df$coords = NULL
#               df$tweets.total = c(df$tweets.total, df$tweets)
#             }
#           } else {}
#         })
#       } else {}
#     })
#
#     observeEvent(df$tweets, {
#       req(df$tweets)
#       req(df$bing.sentiment)
#
#       sentims = df$bing.sentiment[["individual"]][["sentiment"]]
#       df$table = data.table("Tweet" = df$text, "Sentiment" = sentims)
#     })
#
#     observeEvent(df$tweets, {
#
#       df$bing.sentiment = getBingSentiments(df$tweets, iter = df$iter, filter = df$filter)
#       recent.sentiment.count = df$bing.sentiment[["aggregated"]]
#
#       df$sentiment.count = bind_rows(df$sentiment.count, recent.sentiment.count) %>%
#         mutate(cumsum = cumsum(diff))
#       print(df$sentiment.count)
#
#       recent.emotions = df$tweets %>%
#         inner_join(nrc) %>%
#         count(word, sentiment, sort = TRUE)
#       df$emotions = rbind(df$emotions, recent.emotions)
#       print(df$emotions)
#
#       df$negative = sum(df$sentiment.count[["negative"]])
#       df$positive = sum(df$sentiment.count[["positive"]])
#       df$neutral = sum(df$sentiment.count[["neutral"]])
#       df$nonclassifrate = paste0(round(digits = 2,
#                                        1-((df$negative + df$neutral + df$positive)/df$amount)) * 100,
#                                  "%")
#
#       recent.sentiment.wordcount = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(word, sentiment, sort = TRUE)
#       df$sentiment.wordcount = rbind(df$sentiment.wordcount, recent.sentiment.wordcount)
#
#       df$sentiment = df$tweets %>%
#         inner_join(df$lexicon) %>%
#         count(tweetnumber, sentiment) %>%
#         spread(sentiment, n, fill = 0) %>%
#         mutate(sentiment = positive - negative)
#
#     })
#
#     output$iter = renderInfoBox({infoBox("Pulls:",
#                                          value = df$iter,
#                                          icon = shiny::icon("download")
#     )})
#
#     output$amount = renderInfoBox({infoBox("Tweets:",
#                                            value = df$amount,
#                                            icon = shiny::icon("twitter")
#     )})
#
#     output$positive = renderInfoBox({infoBox("Positive:",
#                                              value = df$positive,
#                                              icon = shiny::icon("smile-o"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#     output$rating = renderInfoBox({infoBox("Rating:",
#                                            value = (
#                                              if(df$positive > df$negative) {
#                                                print("positive")
#                                              } else if (df$positive < df$negative) {
#                                                print("negative")
#                                              } else if (df$positive == df$negative) {
#                                                print("neutral")}
#                                            ),
#                                            icon = shiny::icon("filter"))})
#
#     output$negative = renderInfoBox({infoBox("Negative:",
#                                              value = df$negative,
#                                              icon = shiny::icon("frown-o"))})
#
#     output$nonclassifrate = renderInfoBox({infoBox("Unclassif.:",
#                                                    value = df$nonclassifrate,
#                                                    icon = shiny::icon("chain-broken"))})
#
#     output$sentiment.count = renderPlot(sentimCount())
#
#     sentimCount = eventReactive(df$sentiment.count , {
#       req(nrow(df$sentiment.count) >= 1)
#
#       d = df$sentiment.count %>%
#         select(-c(diff, cumsum)) %>%
#         melt("tweetbatch")
#
#       cols = c("negative" = "#F8766D", "positive" = "#00BFC4", "neutral" = "#F1D6AF")
#
#       ggplot(data = d, aes(x = tweetbatch, y = value, group = variable)) +
#         geom_bar(stat = "identity", aes(fill = variable)) +
#         scale_fill_manual(values = cols) +
#         # geom_area(aes(x = tweetbatch, y = positive), fill = "#00BFC4", show.legend = FALSE) +
#         # geom_area(aes(x = tweetbatch, y = -negative), fill = "#F8766D", show.legend = FALSE) +
#         labs(x = "Tweet batch", y = "Marginal sentiment")
#     })
#
#     output$wordcount = renderPlot({wordCount()}, height = function(x) input$wordcount.height)
#
#     wordCount = eventReactive(df$sentiment.wordcount, {
#
#       df = df$sentiment.wordcount %>%
#         filter(n >= 5) %>%
#         mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
#         mutate(word = reorder(word, n))
#
#       ggplot(data = df, aes(word, n, fill = sentiment)) +
#         geom_bar(stat = "identity") +
#         coord_flip() +
#         theme(
#           text = element_text(size = 20),
#           axis.text.x = element_text(angle = 90, hjust = 1)) +
#         ylab("Contribution to sentiment") +
#         scale_fill_manual(values = sentiment.colors)
#     })
#
#     output$timeseries = renderPlot(sentimentTimeseries())
#
#     sentimentTimeseries = eventReactive(df$sentiment, {
#       ggplot(df$sentiment, aes(tweetnumber, sentiment, fill = sentiment > 0)) +
#         geom_bar(stat = "identity", show.legend = FALSE, position = "dodge")
#     })
#
#     output$wordcloud = renderPlot(sentimentWordcloud())
#
#     sentimentWordcloud = eventReactive(df$sentiment.wordcount, {
#
#       termfreq = df$sentiment.wordcount %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = sentiment.colors,
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#     output$emotioncloud = renderPlot(emotionWordcloud())
#
#     emotionWordcloud = eventReactive(df$emotions, {
#
#       termfreq = df$emotions %>%
#         acast(word ~ sentiment, value.var = "n", fill = 0)
#
#       comparison.cloud(term.matrix = termfreq,
#                        colors = brewer.pal(10, "Paired"),
#                        random.order = FALSE,
#                        max.words = 200)
#     })
#
#
#     observeEvent(df$coords, {
#       if (!is.null(df$coords)) {
#         leafletProxy("map", deferUntilFlush = FALSE) %>%
#           addCircleMarkers(data = df$coords, radius = 10)
#       } else {}
#     })
#
#     # output$txt = renderPrint({
#     #   capture.output(df$text)
#     # })
#
#     output$txt = DT::renderDataTable(df$table)
#
#     output$cumsum.plot = renderPlot(cumSumPlot())
#
#     cumSumPlot = eventReactive(df$sentiment.count, {
#       req(nrow(df$sentiment.count) >= 2)
#
#       d = df$sentiment.count[, c("tweetbatch", "cumsum")]
#       roots = findRoots(d)
#       d2 = rbind(d, roots)
#
#       ggplot(data = d2, aes(x = tweetbatch, y = cumsum)) +
#         geom_area(data = subset(d2, cumsum <= 0), fill = "#F8766D") +
#         geom_area(data = subset(d2, cumsum >= 0), fill = "#00BFC4") +
#         labs(x = "Tweet batch", y = "Cumulative sentiment")
#
#     })
#
#     output$map = renderLeaflet({
#       leaflet() %>%
#         addTiles(group = "OSM", options = providerTileOptions(minZoom = 1, maxZoom = 5)) %>%
#         setView(lng = -80, lat = 38, zoom = 3)
#     })
#
#     outs = outputOptions(output)
#     lapply(names(outs), function(name) {
#       outputOptions(output, name, suspendWhenHidden = FALSE)
#     })
#
#   }
#   shinyApp(app.ui, app.server)
# }
#
# # library(shiny)
# #
# # shinyApp(
# #   ui = fluidPage(
# #     useShinyjs(),  # Set up shinyjs
# #     # actionButton("btn", "Click me")
# #     switchInput("btn"),
# #     # textInput("text", "Text")
# #     switchInput("text")
# #   ),
# #   server = function(input, output) {
# #     observeEvent(input$btn, {
# #       # Change the following line for more examples
# #       toggle("text", condition = input$btn == FALSE)
# #     })
# #   }
# # )
# # }
#
#
#
