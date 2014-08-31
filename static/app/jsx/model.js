define(['jquery', 'underscore', 'app/class'], function($, _, obj) {
  "use strict";

  var Class = obj.Class;

  var ExerciseModel = Class.extend({
    init: function() {
      this.exercises  = [];
      this.setStateCB = null;
    },

    load: function (cb) {
      $.ajax({
        type: "GET",
        url: "/rest/exercise",
        success: function(data) {
          this.exercises = data.payload;
          this.setStateCB(this);
        }.bind(this),
        error: function(xhr, status, err) {
          console.error(this.props.url, status, err.toString());
        }.bind(this)
      });
    },

    add: function (e) {
      $.ajax({
        url: "/rest/exercise",
        type: "POST",
        data: e,
        success: function (resp) {
          this.exercises = this.exercises.concat([resp.payload]);
          this.setStateCB(this);
        }.bind(this),
        error: function(xhr, status, err) {
          console.error("/rest/exercise", status, err.toString());
        }.bind(this)
      });
    }

  });

  return {
    'ExerciseModel': ExerciseModel
  };
});
