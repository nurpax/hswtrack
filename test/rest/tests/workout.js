"use strict";

var test = require("../test.js")
  , Q    = require("q")

/*------------------------------------------------------------------*/
var CreateExercises = test.Test.extend({
    init: function () {
        this.description = "Workouts: create exercise types";
    },

    run: function() {
        var insertOne = function (name, type) {
            return test.post({
                url: test.restUrl('/rest/exercise'),
                form: { name: name, type: type }
            });
        };

        var r = insertOne("bench", "W")
                .then(function () { return insertOne("chinups", "BW"); })
                .then(function () { return insertOne("deadlift", "W"); })
                .then(function () {
                    return test.get({
                        url: test.restUrl('/rest/exercise')
                    });
                })
                .then(function (resp) {
                    test.assertStatusCodeOK(resp);
                });
        return r;
    }

});

function withExerciseTypes(f)
{
    return test.get({ url: test.restUrl('/rest/exercise') })
        .then(function (r) {
            return f(JSON.parse(r.body));
        });
}

/*------------------------------------------------------------------*/
var Workout1 = test.Test.extend({
    init: function () {
        this.description = "Workouts: create workout, exercises";
    },

    run: function() {
        // Get a list of exercises, add a workout, and some sets to it
        var resp = withExerciseTypes(function (types) {
            return test.post( {
                url: test.restUrl('/rest/workout')
            }).then(function (w) {
                test.assertStatusCodeOK(w);

                var workout = JSON.parse(w.body);
                test.assert(workout.id);
                test.assert('time' in workout);

                var insertReps = function (type, reps, weight) {
                    return test.post( {
                        url: test.restUrl('/rest/workout/exercise'),
                        form: { workoutId: workout.id,
                                exerciseId: type.id,
                                reps: reps,
                                weight: weight,
                              }
                    });
                }

                // Insert reps, then query the whole workout
                return insertReps(types[0], 4, 90)
                       .then(function () { return insertReps(types[2], 5, 100); })
                       .then(function () { return insertReps(types[2], 6, 100); })
                       .then(function () {
                           return test.get({
                               url: test.restUrl('/rest/workout'),
                               form: { id: workout.id }
                           });
                       });
            }).then(function (r) {
                test.assertStatusCodeOK(r);
                var workout = JSON.parse(r.body);
                test.assert(workout.exercises[0].name == "bench");
                test.assert(workout.exercises[0].sets[0].reps == 4);
                test.assert(workout.exercises[0].sets[0].weight == 90);
                test.assert(workout.exercises[1].name == "deadlift");
                test.assert(workout.exercises[1].sets[0].reps == 5);
                test.assert(workout.exercises[1].sets[0].weight == 100);
                test.assert(workout.exercises[1].sets[1].reps == 6);
            });
        });
        return resp;
    }

});

/*------------------------------------------------------------------*/
module.exports = [new CreateExercises, new Workout1];
