var path = require('path');
var plugins = [];

module.exports = {
  devtool: "source-map",
  entry: ["./index.js"],
  output: {
    path: __dirname + "/dist",
    filename: "index_bundle.js",
    publicPath: '/dist/',
    sourceMapFilename: "index_bundle.js.map"
  },
  plugins: plugins,
  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules\//,
        loader: "babel-loader"
      },
    ]
  }
}