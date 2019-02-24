# Haskell graphql wrapper for stock market api

## current functionality

running /server will prop up a scotty server on port 3000 that returns a stock quote

running /frontend will currently fail - but should run a miso app that will request a stock quote from the server on button push

## Roadmap

- get nix package management working for miso app OR move to stack/miso
- flesh out miso app to display the reponse (expanding the app state to allow for maybe quote, then if the quote is a just, rendering to screen)
- Wrap the restful api in a graphql layer on the server
- perform the request as graphql query on the client
