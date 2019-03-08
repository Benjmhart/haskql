# Haskell graphql wrapper for stock market api

## current functionality

Development Servers:

running `./hastock/stack exec -- yesod devel` will start a development server for the backend.

running `./public/purehmr` will start a hot reloading front end server - right now the new component is mounted below the old one - this will be addressed soon.

## Roadmap

- add GraphQL query system
- add a quote data type on the front end. Parse JSON response into the data type, display it
- CSS
- Deployment
