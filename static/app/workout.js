define(['jquery', 'handlebars', 'app/model', 'app/view', 'hbs!templates/workouts', 'hbs!templates/workout', 'hbs!templates/exercise', 'hbs!templates/exercise-no-edit', 'hbs!templates/add-exercise', 'hbs!templates/add-set-control', 'hbs!templates/new-exercise'], function($, Handlebars, model, view, templateWorkouts, templateWorkout, templateExercise, templateExerciseNoEdit, templateAddExercise, templateAddSetControl, templateNewExercise) {
    "use strict";

    var View = view.View;

    var WorkoutView = View.extend({

        init: function (w) {
            var self = this;
            this._super();

            if (w) {
                this.model = w;
            } else {
                this.model = new model.Workout();
            }
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

            var c = {
                exercises: workout.getExercises()
            };
            $.extend(c, workout);

            $(elt).html(templateWorkout(c));

            $("#workout-public", elt).click(function () {
                workout.setPublic(this.checked);
            });

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

        renderWorkoutReadOnly: function (workout, elt) {
            var self = this;

            var c = {
                exercises: workout.getExercises(),
            };
            $.extend(c, workout);

            $(elt).html(templateWorkout(c));

            $(".exercise", elt).each(function (exerciseIdx) {
                var e = c.exercises[exerciseIdx];
                $(this).html(templateExerciseNoEdit(e));
            });
        },

        renderPrivate: function (elt) {
            var self = this;

            self.model.setUpdateHandler(function (c) {
                if (!self.model.editable)
                    self.renderWorkoutReadOnly(c, elt);
                else
                    self.renderWorkout(c, elt);
            });
        },

        render: function (id) {
            var self = this;
            var elt  = $("#app-container");
            self.renderPrivate(elt);
            self.model.load(id);
        }

    });

    var WorkoutListView = View.extend({

        init: function () {
            var self = this;
            this._super();

            Handlebars.registerPartial('addSetControl', templateAddSetControl);

            this.model = new model.WorkoutCont();
        },

        renderWorkouts: function (workoutCont) {
            var self = this;

            var workouts = {
                workouts: workoutCont.getWorkouts(),
                editable: workoutCont.editable
            };

            $("#app-container").html(templateWorkouts(workouts));

            $(".workout").each(function (workoutIdx) {
                var workoutElt  = this;
                var workout     = workouts.workouts[workoutIdx];
                var workoutView = new WorkoutView(workout);
                workout.editable = workouts.editable && workout.userId == workoutCont.loginUserId;
                workoutView.renderPrivate(workoutElt);
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
        'WorkoutListView': WorkoutListView
    };
});
