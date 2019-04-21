import Main from './output/Main';

function main() {
  const environment = process.env.ENVIRONMENT;
  const apiUrl = process.env.API_URL;
  const baseUrl = process.env.BASE_URL;
  Main.main(environment)(apiUrl)(baseUrl)();
}

if (module.hot) {
  module.hot.accept(() => {
    location.reload();
  });
}

console.log('calling main');
main();
