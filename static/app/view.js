define(['jquery', 'handlebars', 'app/class'], function($, Handlebars, obj) {
    "use strict";

    function capitalizeWord(w) {
        return w.replace(/^./, function (char) {
            return char.toUpperCase();
        });
    }

    function camelCase(tpl, postfix) {
        var ms = tpl.split("-");
        if (ms.length >= 2) {
            var n = ms[0];

            if (_.last(ms) !== postfix) {
                throw new Error("template name must end in -"+postfix);
            }

            for (var i = 1; i < ms.length-1; i++) {
                n = n + capitalizeWord(ms[i]);
            }
            return n;
        } else {
            throw new Error("malformed template name: "+tpl);
        }
    }

    function camelCaseTemplate(tpl) {
        return camelCase(tpl, "template");
    }

    function camelCasePartial(tpl) {
        return camelCase(tpl, "partial");
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
                var n = camelCaseTemplate(tpl);
                self.templates[n] = Handlebars.compile($("#"+tpl).html());
            });
        },

        // Search DOM for 'foo-bar-partial' ID'd elements, compile
        // using Handlebars and register it as a partial into the
        // Handlebars instance.
        registerPartials: function (h, tpls) {
            var self = this;

            _.each(tpls, function (tpl) {
                var n = camelCasePartial(tpl);
                h.registerPartial(n, $("#"+tpl).html());
            });
        }

    });

    // export
    return {
        'View': View,
    };

});
