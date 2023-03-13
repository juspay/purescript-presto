var path = require('path');
var plugins = [];

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
    loaders: [
      {
        test: /\.js$/,
        loader: "babel-loader",
        query: {
          presets: ['es2015', 'react']
        },
        exclude: /node_modules/,
      },
       {
        test: /\.css$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" }
        ]
      },
      {
        test: /\.(jpe?g|png|gif|svg)$/i,
         use: [
          {
            loader: 'file-loader',
            options: {
              query: {
                name:'assets/[name].[ext]'
              }
            }
          },
          {
            loader: 'image-webpack-loader',
            options: {
              query: {
                mozjpeg: {
                  progressive: true,
                },
                gifsicle: {
                  interlaced: true,
                },
                optipng: {
                  optimizationLevel: 7,
                }
              }
            }
          }]
      }
    ]
  }
}