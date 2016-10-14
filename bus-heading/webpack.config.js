
module.exports= {
    entry: './src/index.js',
    output: {
        filename: 'dist/index.js',
    },
    module: {
        loaders: [
            {
                test: /\.js$/,
                exclude: /node_mod/,
                loader: 'babel',
                query: {
                    presets: ['es2015', 'stage-3'],
                    plugins: ['transform-runtime']
                },
            },
        ],
    },
};


