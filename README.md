# livesentiment

Sentiment analysis of real time twitter feeds in R.
Package development still in early alpha stage. 

This R package provides an interactive user interface based on Shiny for analyzing the polarity of real time twitter feeds.

Pre-loading a twitter oauth (and therefore having a twitter account) is necessary prior to being able to use this package.
A tutorial on how to set up the twitter oauth is going to be added soon.
Meanwhile, follow this link to an excellent manual on how to load your twitter authentification in R.
http://pablobarbera.com/big-data-upf/html/02d-twitter-streaming-data-collection.html

After saving your authentification credentials as an .rdata file, just load it via 'load(<my_oath>.rdata)' and start the application with the 'liveSentiment()' call.

Feel free to use this application and leave comments on how to improve it.
