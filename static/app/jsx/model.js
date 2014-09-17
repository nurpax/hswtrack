define(['jquery', 'underscore', 'app/class'], function($, _, obj) {
  "use strict";

  var Class = obj.Class;

  function calcExerciseStats(e) {
    if (e.type == "BW") {
      return _.reduce(e.sets, function (a, s) { return a+s.reps; }, 0);
    }
    else if (e.type == "W") {
      return _.reduce(e.sets, function (a, s) { return a+s.reps*s.weight; }, 0);
    }
    console.error("unknown type "+e.type);
  }

  function loadExerciseTypes() {
    return $.ajax({
      type: "GET",
      url: "/rest/exercise"
    });
  }

  function loadWorkout(id) {
    return $.ajax({
      type: "GET",
      url: "/rest/workout",
      data: { id: id }
    });
  }

  var Exercise = Class.extend({
    init: function() {
      this.exercises  = [];
      this.setStateCB = null;
    },

    load: function () {
      $.when(loadExerciseTypes())
          .done(function (data) {
            this.exercises = data.payload;
            this.setStateCB(this);
          }.bind(this));
    },

    add: function (e) {
      $.ajax({
        url: "/rest/exercise",
        type: "POST",
        data: e,
        success: function (resp) {
          this.exercises = this.exercises.concat([resp.payload]);
          this.setStateCB(this);
        }.bind(this)
      });
    }
  });

  var Workout = Class.extend({
    init: function(id) {
      this.workout = { id: id, exercises: [] };
      this.exerciseTypes = [];
      this.canEdit       = false;
      this.setStateCB = null;
    },

    load: function () {
      $.when(loadWorkout(this.workout.id), loadExerciseTypes())
          .done(function (w, e) {
            this.workout = w[0].payload;
            this.exerciseTypes = e[0].payload;
            // Is this workout editable by the currently logged in user?
            if (w[0].loggedIn) {
              this.canEdit = w[0].userId == w[0].payload.userId;
            }
            this.setStateCB(this);
          }.bind(this));
    },

    addSet: function (params) {
      $.ajax({ url: "/rest/workout/exercise",
              type: "POST",
              data: params,
              success: function (resp) {
                var exercise = _.find(this.workout.exercises, function (s) { return s.id == params.exerciseId; });
                if (!exercise) {
                  // No existing exercise.sets in the current workout,
                  // so rather than try to create one locally, reload
                  // the whole workout.
                  this.load();
                  return;
                }
                exercise.sets.push(resp.payload);
                this.setStateCB(this);
              }.bind(this)
      });
    },

    rmSet: function (params) {
      $.ajax({ url: "/rest/workout/exercise",
              type: "DELETE",
              data: params,
              success: function () {
                // Delete the removed set from the client copy of the
                // workout/exercises/sets data structure.
                for (var i = 0; i < this.workout.exercises.length; i++) {
                  var sets = this.workout.exercises[i].sets;
                  if (_.find(sets, function (s) { return s.id == params.id; })) {
                    this.workout.exercises[i].sets = _.filter(sets, function (s) { return s.id != params.id; });
                    this.setStateCB(this);
                    return;
                  }
                }
              }.bind(this)
      });
    },

    makePublic: function (workout, setPublic) {
      $.ajax({ url: "/rest/workout",
              type: "PUT",
              dataType: "json",
              data: JSON.stringify({ id: workout.id, public: setPublic }),
              success: function (resp) {
                this.workout.public = resp.payload.public;
                this.setStateCB(this);
              }.bind(this)
      });
    }
  });

  function loadWorkouts() {
    return $.ajax({
      type: "GET",
      url: "/rest/workout"
    });
  }

  // Workout list for today
  var WorkoutList = Class.extend({
    init: function() {
      this.workouts = [];
      this.setStateCB = null;
    },

    newWorkout: function () {
      $.ajax( { url: "/rest/workout",
               type: "POST",
               data: [],
               success: function (resp) {
                 var w = resp.payload;
                 this.workouts.push(w);
                 this.setStateCB(this);
               }.bind(this)
      });
    },

    load: function () {
      $.when(loadWorkouts())
          .done(function (w) {
            this.workouts = w.payload;
            this.setStateCB(this);
          }.bind(this));
    },
  });

  function loadAppContext() {
    return $.ajax({
      type: "GET",
      url: "/rest/app",
      data: []
    });
  }

  function loadWeights(ndays) {
    return $.ajax({
      type: "GET",
      url: "/rest/weights",
      data: { days: ndays }
    });
  }

  function loadNotes() {
    return $.ajax({
      type: "GET",
      url: "/rest/notes",
      data: []
    });
  }

  /*--------------------------------------------------------------*/
  // Weight tracking
  /*--------------------------------------------------------------*/

  var Weights = Class.extend({
    init: function(weights) {
      this.weights = weights;
      this.setStateCB = null;
    },

    addWeight: function (r) {
      this.weights.push(r);
    },

    deleteWeight: function (r) {
      this.weights = _.filter(this.weights, function (w) { return w.id != r.id; });
    },

    load: function (days) {
      $.when(loadWeights(days)).done(function (w) {
        this.weights = w.payload;
        this.setStateCB(this);
      }.bind(this));
    }
  });

  var Notes = Class.extend({
    init: function(n) {
      this.notes = n;
    },

    deleteById: function (id_) {
      $.ajax({ url: "/rest/note",
              type: "DELETE",
              data: { id: id_ },
              success: function () {
                this.notes = _.filter(this.notes, function (n) { return n.id != id_; });
                this.setStateCB(this);
              }.bind(this)
      });
    },

    addNote: function (text) {
      $.ajax({ url: "/rest/note",
              type: "POST",
              data: { text: text },
              success: function (resp) {
                this.notes.push(resp.payload);
                this.setStateCB(this);
              }.bind(this)
      });
    }

  });

  var WeightTop = Class.extend({
    init: function() {
    },

    load: function(selectedGraphDays) {
      $.when(loadAppContext(), loadWeights(selectedGraphDays), loadNotes()).done(function (a, w, n) {
        this.app     = a[0].payload;
        this.weights = new Weights(w[0].payload);
        this.notes   = new Notes(n[0].payload);
        this.selectedRange = selectedGraphDays;
        this.setStateCB(this);
      }.bind(this));
    },

    loadWeights: function (selectedGraphDays) {
      $.when(loadWeights(selectedGraphDays)).done(function (w) {
        this.weights = new Weights(w.payload);
        this.selectedRange = selectedGraphDays;
        this.setStateCB(this);
      }.bind(this));
    },

    setWeight: function (newWeight) {
      $.ajax({
        type: "POST",
        url: "/rest/weight",
        data: { weight: newWeight },
        success: function (r) {
          var weight = r.payload;
          this.app.context.weight = weight;
          this.weights.addWeight(weight);
          this.setStateCB(this);
        }.bind(this)
      });
    },

    clearWeight: function () {
      if (this.app.context.weight) {
        $.ajax({
          type: "DELETE",
          url: "/rest/weight",
          data: { id: this.app.context.weight.id },
          success: function () {
            this.weights.deleteWeight(this.app.context.weight);
            this.app.context.weight = null;
            this.setStateCB(this);
          }.bind(this)
        });
      }
    }

  });


  return {
    'calcExerciseStats': calcExerciseStats,
    'WeightTop': WeightTop,
    'Exercise': Exercise,
    'Workout': Workout,
    'WorkoutList': WorkoutList
  };
});
