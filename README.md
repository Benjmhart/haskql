# Haskell graphql wrapper for stock market api

## current functionality

running /server will prop up a scotty server on port 3000 that returns a stock quote

running /frontend will currently fail - but should run a miso app that will request a stock quote from the server on button push

## Roadmap

- flesh out miso app to display the reponse (expanding the app state to allow for maybe quote, then if the quote is a just, rendering to screen)
- Wrap the restful api in a graphql layer on the server
- perform the request as graphql query on the client
- break up main functions on front end as seen in example spa
- consider building out the app with shared models files as seen here: https://github.com/y-taka-23/miso-tutorial-app
