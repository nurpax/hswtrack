requirejs.config({
    paths: {
        app: 'app',
        jsx: 'jsx-build',
        d3: 'lib/d3/d3.v3.min',
        jquery: 'lib/jquery-2.1.2-ajax-only.min',
        underscore: 'lib/underscore-1.6.0/underscore-min',
        history: 'lib/history',
        history_adapter: 'lib/history.adapter.native',
        router: 'lib/router',
        react: 'lib/reactjs-0.13.1/react.min',
    },
    shim: {
        d3: {
            exports: "d3"
        },
        history: {
            exports: "History"
        },
        history_adapter: {
            exports: "History.Adapter"
        },
        router: {
            deps: ['history', 'history_adapter'],
            exports: "Router"
        },
    }
});

require(['app/app'], function(app) {
    "use strict";
    var application = new app.App();
    application.start(); // or whatever startup logic your app uses.
});
