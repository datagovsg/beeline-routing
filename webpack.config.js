module.exports = {
  entry: [
    'babel-polyfill',
    './srcjs/main.js'
  ],
  output: {
    filename: 'bundle.js',
    path: 'static'
  },
  module: {
    loaders: [{
      test: /\.js$/,
      exclude: /node_mod/,
      loader: 'babel',
      query: {
        presets: ['es2015', 'stage-3'],
      },
    }],
  },
};
