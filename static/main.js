"use strict";

requirejs.config({
    paths: {
        app: 'app',
        d3: 'lib/d3/d3.v3.min',
        jquery: 'lib/jquery-2.0.3/jquery.min',
        bootstrap: 'lib/bootstrap-3.0.0/js/bootstrap.min',
        handlebars: 'lib/handlebars-1.3.0/handlebars-v1.3.0',
        underscore: 'lib/underscore-1.6.0/underscore-min',
        router: 'lib/router'
    },
    shim: {
        // TODO I don't understand why this is needed here.
        // Handlebars 1.3 is supposed to work fine with RequireJS but
        // if I don't add this exports here, Handlebars is not defined
        // in app/app.js App constructor, even though it's defined as
        // a dependency.
        //
        // I think I should install Handlebars via npm, perhaps that
        // installer adds the right .amd files for this to work the
        // right way with requirejs.
        handlebars: {
            exports: 'Handlebars'
        },
        d3: {
            exports: "d3"
        },
        bootstrap: ['jquery'],
        router: {
            exports: "router"
        },
    }
});

require(['app/app'], function(app) {
    var app = new app.App();
    app.start(); // or whatever startup logic your app uses.
});
