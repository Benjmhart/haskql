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

relevant server config/settings are:
PORT - managed automatically by heroku, tells us what port our server needs to run on in order to be public
APIKEY - our api key for the alphavantage stock api
APPROOT - (server) - tells the server the domain in which it is running
APIURL - (client) - tells the client whether it

## Roadmap

- CSS/Componentization
- POSTGRES/AUTH

## Example CURLS

#### RegisterR POST

```
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"name":"Jonathan","email":"jonathan_lorimer@mac.com","password":"Hello123%"}' \
  http://localhost:3000/api/v1/register
```

#### LoginR POST

```
curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"loginEmail":"jonathan_lorimer@mac.com","loginPassword":"Hello123%"}' \
  http://localhost:3000/api/v1/login
```

#### UserR GET

```
curl --header "Content-Type: application/json" \
  --request GET \
  http://localhost:3000/api/v1/user/<your-jwt-goes-here>
```
