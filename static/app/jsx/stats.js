/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model'], function(_, React, model) {
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

  var WorkoutList = React.createClass({
    getInitialState: function() {
      return {data: []};
    },

    componentDidMount: function() {
      $.ajax({
        type: "GET",
        data: { limit: 14 },
        url: "/rest/stats/workout",
        success: function(data) {
          this.setState({data: data.payload});
        }.bind(this),
        error: function(xhr, status, err) {
          console.error(this.props.url, status, err.toString());
        }.bind(this)
      });
    },

    render: function () {
      var nodes = this.state.data.map(function (w) {
        return (
          <Workout key={w.id} workout={w} />
        )
      });

      return (
        <div className="workoutList">
          <h2>Past 14 workouts</h2>
          {nodes}
        </div>
      );
    }
  });

  var renderStats = function () {
    React.renderComponent(
      <div className="container">
        <ul className="nav nav-tabs">
          <li className="active"><a href="#history-tab" data-toggle="tab">History</a></li>
          <li><a href="#pr-tab" data-toggle="tab">Personal Records</a></li>
        </ul>
        <div className="tab-content">
          <div className="tab-pane active" id="history-tab">
            <WorkoutList />,
          </div>
          <div className="tab-pane" id="pr-tab">tab2</div>
        </div>
      </div>,
      document.getElementById('app-container')
    );
  };

/*
   $('a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
     var target = $(e.target).attr("href");
   });
*/

  // export
  return {
    'render': renderStats
  };

});
