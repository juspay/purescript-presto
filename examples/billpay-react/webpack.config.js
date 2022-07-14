const path = require('path')
const webpack = require('webpack');

module.exports = {
  devtool: "source-map",
  entry: ["./index.js"],
  output: {
    path: __dirname + "/dist",
    filename: "index_bundle.js",
    publicPath: '/dist',
    sourceMapFilename: "index_bundle.js.map"
  },
  module: {
    rules: [
      { test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"],
          }
        }
      },
    ]
  }
}
