var requirejs = require('requirejs')
  , Q         = require('q')
  , request   = Q.denodeify(require('request'))

requirejs.config({
    // Pass the top-level main.js/index.js require
    // function to requirejs so that node modules
    // are loaded relative to the top-level JS file.
    nodeRequire: require
});

const URL = "http://localhost:8000";

function restUrl(s) {
    return URL + s;
}

requirejs(['underscore', 'foo'], function (_, foo) {

    var resp = request({
        url: restUrl('/rest/weights?days=1'),
        method: 'GET'
    });

    var rr = resp.then(function (res) {
        var r = res[0];
        if (r.statusCode >= 300) {
            console.log(r);
            throw new Error('Server responded with status code ' + r.statusCode + ' - ' + r.body)
        } else {
            return r;
        }
    });

    Q.when(rr, function (r) {
        console.log(r.body);
    }, function (err) {
        console.log("error: " + err.toString());
    }).done();
});
