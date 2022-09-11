const CopyWebpackPlugin = require("copy-webpack-plugin");
const path = require('path');

module.exports = {
  entry : "./0.js",
  output : {
    path : path.resolve(__dirname, "dist"),
    filename : "0.js",
  },
  mode : "development",
  plugins : [ new CopyWebpackPlugin([ 'index.html' ]) ],
  module : {
    rules : [
      {
        test : /\.css$/i,
        use : [ 'style-loader', 'css-loader' ],
      },
    ],
  },
};
