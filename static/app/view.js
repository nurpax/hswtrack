define(['jquery', 'handlebars', 'app/class'], function($, Handlebars, obj) {
    "use strict";

    function capitalizeWord(w) {
        return w.replace(/^./, function (char) {
            return char.toUpperCase();
        });
    }

    function camelCase(tpl) {
        var ms = tpl.split("-");
        if (ms.length >= 2) {
            var n = ms[0];
            for (var i = 1; i < ms.length-1; i++) {
                n = n + capitalizeWord(ms[i]);
            }
            return n;
        } else {
            throw new Error("malformed template name: "+tpl);
        }
    }

    var View = Class.extend({
        init: function () {
            this.templates = {};
        },

        // Search DOM for 'foo-bar-template' ID'd elements, compile
        // using Handlebars and save result into templates.fooBar
        // field.
        compileTemplates: function (tpls) {
            var self = this;

            _.each(tpls, function (tpl) {
                var n = camelCase(tpl);
                self.templates[n] = Handlebars.compile($("#"+tpl).html());
            });
        }

    });

    // export
    return {
        'View': View,
    };

});
