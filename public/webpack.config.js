'use strict';

const path = require('path');

const webpack = require('webpack');

const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');

const isWatch = process.argv.some(a => a === '--watch');

const plugins =
  isWebpackDevServer || !isWatch
    ? []
    : [
        function() {
          this.plugin('done', function(stats) {
            process.stderr.write(stats.toString('errors-only'));
          });
        }
      ];
module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4008,
    stats: 'errors-only'
  },

  entry: './index.js',

  output: {
    path: path.resolve(__dirname, './webpackbuild'),
    pathinfo: true,
    filename: 'index.js'
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: ['.spago/**/src/**/*.purs', 'src/**/*.purs'],
              bundle: false,
              psc: 'psa',
              watch: isWebpackDevServer || isWatch,
              pscIde: false
            }
          }
        ]
      }
    ]
  },

  resolve: {
    modules: ['node_modules', '.spago'],
    extensions: ['.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ].concat(plugins)
};
