define(['class'], function(obj) {
    "use strict";

    var Foo = obj.Class.extend({
        init: function () {
            console.log("initialized");
        }
    });

    // export
    return {
        'Foo': Foo,
    };
});
