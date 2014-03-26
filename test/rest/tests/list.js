"use strict";

module.exports = {
    list: function () {
        return [ require("./login.js")
               , require("./weight.js")
               , require("./workout.js")
               ]
    }
};
