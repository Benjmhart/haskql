# Haskell graphql wrapper for stock market api

## current functionality

Development Servers:

running `./hastock/stack exec -- yesod devel` will start a development server for the backend.

running `./public/npm start` will start a hot reloading front end server - right now the new component is mounted below the old one - this will be addressed soon.

running `./public/npm run build:prod` will compile exiting code with spago, then build a bundle using production variables with parcel. the output will be placed in `hastock/static`.

running `./deploy` from the root folder will compile the production halogen app, feed it to the static folder of the yesod app, then compile the yesod app, commit the entire project, and push over origin and heroku remotes. USE WITH EXTREME CAUTION

## Secrets & variables

- variables for the halogen app are kept in two .env files in the public folder. the root index.js has variables process.env... that are replaced at bundle time with the values in the relevant .env file. these are not kept secret and can be committed. these variables are then passed into the purescript stack directly as arguments to the Main function.
- secrets on the server are configured in hastock/config/secrets.yml, and any real secrets, such as API keys should be stored in environment variables both locally and in deployment.

## Roadmap

- add GraphQL query system
- add a quote data type on the front end. Parse JSON response into the data type, display it
- CSS
- Deployment
