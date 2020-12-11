const webpack = require('webpack');

const dotenv = require('dotenv').config();

module.exports = {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                use: ['file-loader?name=[name].[ext]']
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [{
                    loader: 'elm-webpack-loader',
                    options: {}
                }]
            }
        ]
    },

    plugins: [
        new webpack.DefinePlugin({
            'process.env': JSON.stringify(dotenv.parsed),
        })
    ],

    devServer: {
        inline: true,
        stats: 'errors-only'
    }
};
