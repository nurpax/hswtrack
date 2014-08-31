/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model', 'jsx/workout'], function(_, React, model, workout) {
  "use strict";

  function dateString (v) {
    return (new Date(v)).toLocaleString();
  }

  var Exercise = React.createClass({
    render: function() {
      var sets, unitKgOrReps;
      var total = model.calcExerciseStats(this.props);
      if (this.props.type == "BW") {
        unitKgOrReps = "";
        sets = this.props.sets.map(function (s) {
          return (
            <tr key={s.id}>
              <td>Set:</td>
              <td className="text-right">{s.reps}</td>
              <td>reps</td>
              <td>{s.weight ? '(+'+s.weight+' kg)' : '' }</td>
            </tr>
          )
        });
      } else {
        if (this.props.type != 'W')
          console.error(this.props.type, "exercise type must be 'W'");
        unitKgOrReps = "kg";
        sets = this.props.sets.map(function (s) {
          return (
            <tr key={s.id}>
              <td>Set:</td>
              <td className="text-right">{s.reps}</td>
              <td>&times;</td>
              <td>{s.weight}</td>
            </tr>
          )
        });
      }

      return (
        <div className="exercise">
          <h4>{this.props.name}</h4>
          <table>
            {sets}
            <tr className="sets_total"><td colSpan="4">Total: {total} {unitKgOrReps}</td></tr>
          </table>
        </div>
      );
    }
  });

  var Workout = React.createClass({
    render: function() {
      var wid = this.props.workout.id;
      var exs = this.props.workout.exercises.map(function (e) {
        // Conjure up a dummy id -- exercises inside a workout don't have
        // a db rowid.
        var id = wid + '-' + e.id;
        return (
          <Exercise key={id} type={e.type} sets={e.sets} name={e.name} />
        )
      });

      var url = "/workout/"+this.props.workout.id;
      var timestamp = dateString(this.props.workout.time);
      return (
        <div className="workout">
          <h3><a href={url}>Workout {this.props.workout.id}</a> <small>{timestamp}</small></h3>
          {exs}
        </div>
      )
    }
  });

  return {
    'Workout': Workout,
    'Exercise': Exercise
  }
});
