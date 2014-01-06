requirejs.config({
    paths: {
        app: 'app',
        d3: 'lib/d3/d3.v3.min',
        jquery: 'lib/jquery-2.0.3/jquery.min',
        bootstrap: 'lib/bootstrap-3.0.0/js/bootstrap.min',
        handlebars: 'lib/handlebars-1.0.0/handlebars',
    },
    shim: {
        handlebars: {
            exports: 'Handlebars'
        },
        d3: {
            exports: "d3"
        },
        bootstrap: ['jquery']
    }
});

require(['app/app'], function(app) {
    var app = new app.App();
    app.start(); // or whatever startup logic your app uses.
});
