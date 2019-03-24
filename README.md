# Haskell graphql wrapper for stock market api

## current functionality

Development Servers:

running `./hastock/stack exec -- yesod devel` will start a development server for the backend.

running `./public/npm start` will start a hot reloading front end server - right now the new component is mounted below the old one - this will be addressed soon.

running `./public/npm run build:prod` will compile exiting code with spago, then build a bundle using production variables with parcel. the output will be placed in `hastock/static`.

## Roadmap

- add GraphQL query system
- add a quote data type on the front end. Parse JSON response into the data type, display it
- CSS
- Deployment
