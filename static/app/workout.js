define(['jquery', 'handlebars', 'app/model', 'app/view', 'hbs!templates/workouts', 'hbs!templates/workout', 'hbs!templates/exercise', 'hbs!templates/add-exercise', 'hbs!templates/add-set-control', 'hbs!templates/new-exercise'], function($, Handlebars, model, view, templateWorkouts, templateWorkout, templateExercise, templateAddExercise, templateAddSetControl, templateNewExercise) {
    "use strict";

    var View = view.View;

    // Page for adding new exercise types
    var ExerciseTypeView = View.extend({
        init: function () {
            var self = this;
            this._super();
            this.exercises = new model.ExerciseTypes();
            this.exercises.setUpdateHandler(function (c) { self.renderExerciseList(); });
        },

        renderExerciseList: function () {
            var self = this;
            $("#app-container").html(templateNewExercise({ exercises: self.exercises.exerciseTypes }));

            $("form#new-exercise").each(function () {
                var form = this;
                $("button#new-workout", form).click(function (e) {
                    e.preventDefault();
                    var exerciseName = $("input#exercise-name", form).val();
                    var type         = $("input[name=exercise-type-radios]:checked", form).val();

                    self.exercises.addExercise({ name: exerciseName, type:type });
                });
            });
        },

        render: function () {
            this.exercises.load();
        }
    });


    var WorkoutView = View.extend({

        init: function () {
            var self = this;
            this._super();

            Handlebars.registerPartial('addSetControl', templateAddSetControl);

            this.model = new model.WorkoutCont();
        },


        attachAddExercise: function (elt, addSetCB, exercise, workoutId) {
            var self = this;

            $("form.add-set", elt).each(function () {
                var form = this;

                $('a#collapse-weighted', form).click(function(e) {
                    e.preventDefault();
                    var c = $(this).closest('.collapse-group').find('.collapse');
                    c.collapse('toggle');
                    $(this).hide();
                });


                $("button", this).click(function (e) {
                    e.preventDefault();
                    var reps   = $("input.reps", form).val();
                    var weight = $("input.weight", form).val();

                    if (reps === '' || reps === '0') {
                        return;
                    }

                    if (exercise.type == 'BW') {
                        if (weight === '') {
                            weight = 0;
                        }
                    } else {
                        if (weight === '') {
                            return;
                        }
                    }

                    var data = {
                        reps: reps,
                        weight: weight,
                        exerciseId: exercise.id,
                        workoutId: workoutId
                    };

                    // attachAddExercise may either need to refresh just
                    // the current exercise sets list, or the whole
                    // workout where the set is being added.  This is why
                    // the set addition is parametrized, and we don't
                    // simply call exercise.addSet here.
                    addSetCB(data);
                });
            });
        },

        renderExercise: function (exercise, workoutId, elt) {
            var self = this;
            $(elt).html(templateExercise(exercise));
            this.attachAddExercise(elt, function (d) { exercise.addSet(d); }, exercise, workoutId);

            // Activate delete buttons
            $("table tr", elt).each(function (idx) {
                $("a.rm-set", this).click(function (e) {
                    e.preventDefault();

                    if (!confirm("OK to delete set?"))
                        return;

                    var data = { id: exercise.sets[idx].id };
                    exercise.deleteSet(data);
                });
            });
        },

        renderWorkout: function (workout, elt) {
            var self = this;

            var c = { exercises: workout.getExercises() };
            $(elt).html(templateWorkout(c));

            $(".exercise", elt).each(function (exerciseIdx) {
                var exerciseElt = this;
                var exercise    = c.exercises[exerciseIdx];
                exercise.setUpdateHandler(function (e) { self.renderExercise(e, workout.id, exerciseElt); });
            });

            $("div.add-exercise", elt).html(templateAddExercise(
                { exerciseTypes: self.model.getExerciseTypes() }
            ));

            $("select.select-exercise", elt).change(function () {
                var selectedExerciseId = $(this).val();
                var addExerciseScope = $(".add-exercise-sets", elt);
                var e = self.model.getExerciseById(selectedExerciseId);
                addExerciseScope.html(templateAddSetControl(e));

                self.attachAddExercise(addExerciseScope,
                                       function (d) { workout.addExerciseSet(e, d); },
                                       e,
                                       workout.id);
            });
        },

        renderWorkouts: function (workoutCont) {
            var self = this;
            var workouts = { workouts: workoutCont.getWorkouts() };

            $("#app-container").html(templateWorkouts(workouts));

            $(".workout").each(function (workoutIdx) {
                var workoutElt = this;
                var workout    = workouts.workouts[workoutIdx];
                workout.setUpdateHandler(function (w) { self.renderWorkout(w, workoutElt); });
            });

            $("button#new-workout").click(function (elt) {
                workoutCont.newWorkout();
            });
        },

        render: function () {
            var self = this;
            self.model.setUpdateHandler(function (c) { self.renderWorkouts(c); });
            self.model.load();
        }

    });

    // export
    return {
        'WorkoutView': WorkoutView,
        'ExerciseTypeView': ExerciseTypeView,
    };
});
