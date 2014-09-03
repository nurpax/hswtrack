/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model', 'jsx/workout'], function(_, React, model, workout) {
  "use strict";

  function dateString (v) {
    return (new Date(v)).toLocaleString();
  }

  var Set = React.createClass({
    handleRmSet: function () {
      var msg;
      if (this.props.type == 'W') {
        msg = this.props.reps + " x " + this.props.weight + " kg";
      } else {
        msg = this.props.reps + " reps (+ " + this.props.weight + " kg)";
      }
      if (!confirm(msg + "\n\nOK to delete set?"))
        return false;
      this.props.onRmSetSubmit({ id: this.props.setId });
      return false;
    },

    render: function () {
      var rmSet = this.props.readonly ?
                  null :
                  <td><a setId={this.props.setId} onClick={this.handleRmSet} href="#">&times;</a></td>;
      if (this.props.type == "BW") {
        return (
          <tr>
            <td>Set:</td>
            <td className="text-right">{this.props.reps}</td>
            <td>reps</td>
            <td>{this.props.weight ? '(+'+this.props.weight+' kg)' : '' }</td>
            {rmSet}
          </tr>
        )
      } else {
        return (
          <tr>
            <td>Set:</td>
            <td className="text-right">{this.props.reps}</td>
            <td>&times;</td>
            <td>{this.props.weight} kg</td>
            {rmSet}
          </tr>
        )
      }
    }
  });

  var Exercise = React.createClass({
    render: function () {
      var sets, unitKgOrReps;
      var total = model.calcExerciseStats(this.props);
      var type  = this.props.type;

      if (type != 'W' && type != 'BW')
        console.error(this.props.type, "unknown exercise type " + type);

      unitKgOrReps = type == 'BW' ? "" : "kg";

      sets = this.props.sets.map(function (s) {
        return <Set key={s.id} type={type} reps={s.reps} weight={s.weight} setId={s.id}
                    readonly={this.props.readonly} onRmSetSubmit={this.props.onRmSetSubmit} />;
      }.bind(this));

      return (
        <div>
          <h4>{this.props.name}</h4>
          <table className="sets">
            <tbody>
              {sets}
            </tbody>
          </table>
          <p>Total: {total} {unitKgOrReps}</p>
        </div>
      );
    }
  });

  var WorkoutTitle = React.createClass({
    render: function () {
      var url = "/workout/"+this.props.workout.id;
      var timestamp = dateString(this.props.workout.time);
      return <h3><a href={url}>Workout {this.props.workout.id}</a> <small>{timestamp}</small></h3>;
    }
  });

  var Workout = React.createClass({
    getDefaultProps: function () {
      return {
        readonly: true
      }
    },

    render: function () {
      var wid = this.props.workout.id;
      var exs = this.props.workout.exercises.map(function (e) {
        var addSet = <AddSetForm onAddSetSubmit={this.props.onAddSetSubmit}
                                 workoutId={wid} exercise={e} />;
        // Conjure up a dummy id -- exercises inside a workout don't have
        // a db rowid.
        var id = wid + '-' + e.id;
        return (
          <div key={id}>
            <Exercise type={e.type} sets={e.sets} name={e.name} readonly={this.props.readonly}
                      onRmSetSubmit={this.props.onRmSetSubmit} />
            {this.props.readonly ? null : addSet}
          </div>
        )
      }.bind(this));

      var url = "/workout/"+this.props.workout.id;
      return (
        <div>
          <WorkoutTitle workout={this.props.workout} />
          {this.props.readonly ? null : <label><input type="checkbox" /> Allow public viewing?</label>}
          {exs}
        </div>
      )
    }
  });

  var AddSetForm = React.createClass({
    handleSubmit: function () {
      var reps   = this.refs.reps.getDOMNode().value.trim();
      var weight = this.refs.weight.getDOMNode().value.trim();
      if (!reps) {
        return false;
      }
      if (weight == '')
        weight = 0;

      this.refs.reps.getDOMNode().value = '';
      this.refs.weight.getDOMNode().value = '';

      this.props.onAddSetSubmit({
        reps: reps,
        weight: weight,
        exerciseId: this.props.exercise.id,
        workoutId: this.props.workoutId,
      });
      return false;
    },

    render: function () {
      if (!this.props.exercise)
        return null;

      var inp = null;
      if (this.props.exercise.type == 'BW') {
        inp =
          <div className="collapse-group">
            <input className="collapse form-control" type="number" ref="weight" placeholder="Weight.." />
            <a id="collapse-weighted" className="btn" href="#">Advanced &raquo;</a>
          </div>;
      } else if (this.props.exercise.type == 'W') {
        inp = <input className="form-control" type="number" ref="weight"  placeholder="Weight.." />;
      }
      return (
        <form onSubmit={this.handleSubmit}>
          <div className="row">
            <div className="col-xs-4">
              <input className="form-control" type="number" ref="reps" placeholder="Reps.." />
            </div>
            <div className="col-xs-4">
              {inp}
            </div>
            <div className="col-xs-2">
              <button className="btn btn-default" type="submit">Add Set</button>
            </div>
          </div>
        </form>
      );
    }
  });

  var AddExerciseForm = React.createClass({
    getInitialState: function () {
      return { selectedExercise: null };
    },

    exerciseSelected: function (e) {
      this.setState({ selectedExercise: this.props.exerciseTypes[e.target.value] });
    },

    render: function () {
      var exs = this.props.exerciseTypes.map(function (et, index) {
        return <option key={et.id} value={index}>{et.name}</option>
      });
      return (
        <div className="well">
          <div className="row">
            <div className="col-md-4"><b>Add a New Exercise</b></div>
          </div>
          <div className="row">
            <div className="col-md-6">
              <select onChange={this.exerciseSelected} className="form-group">
                <option defaultSelected disabled hidden></option>
                {exs}
              </select>
            </div>
          </div>
          <AddSetForm onAddSetSubmit={this.props.onAddSetSubmit}
                      workoutId={this.props.workout.id}
                      exercise={this.state.selectedExercise} />
          <div>
            <label>Favorite exercise missing?</label>&nbsp;<a href="/exercise/edit">Add it!</a>
          </div>
        </div>
      )
    }
  });

  var WorkoutEdit = React.createClass({
    getInitialState: function () {
      this.props.model.setStateCB = function (s) { this.setState(s); }.bind(this);
      return this.props.model;
    },

    componentDidMount: function () {
      this.props.model.load();
    },

    handleAddSetSubmit: function (e) {
      this.props.model.addSet(e);
    },

    handleRmSetSubmit: function (e) {
      this.props.model.rmSet(e);
    },

    render: function () {
      var addExercise = null;
      if (this.state.canEdit) {
        addExercise = <AddExerciseForm workout={this.state.workout}
                                       onAddSetSubmit={this.handleAddSetSubmit}
                                       exerciseTypes={this.state.exerciseTypes} />;
      }
      return (
        <div>
          <Workout workout={this.state.workout}
                   readonly={!this.state.canEdit}
                   onRmSetSubmit={this.handleRmSetSubmit}
                   onAddSetSubmit={this.handleAddSetSubmit} />
          {addExercise}
        </div>
      );
    }
  });

  var render = function (id) {
    React.renderComponent(
      <div>
        <WorkoutEdit model={new model.Workout(id)}/>
      </div>,
      document.getElementById('app-container')
    );
  };

  var WorkoutList = React.createClass({
    getInitialState: function () {
      this.props.model.setStateCB = function (s) { this.setState(s); }.bind(this);
      return this.props.model;
    },

    componentDidMount: function () {
      this.props.model.load();
    },

    handleNewWorkout: function () {
      this.props.model.newWorkout();
    },

    render: function () {
      var workouts = this.state.workouts.map(function (w) {
        return <WorkoutTitle key={w.id} workout={w} />;
      }.bind(this));
      return (
        <div>
          <h2>Today's workouts</h2>
          {workouts}
          <button onClick={this.handleNewWorkout} className="btn btn-primary">Add a Workout</button>
        </div>
      );
    }
  });

  var renderToday = function () {
    React.renderComponent(
      <WorkoutList model={new model.WorkoutList()}/>,
      document.getElementById('app-container')
    );
  };

  return {
    'render':      render,
    'renderToday': renderToday,
    'Workout':     Workout,
    'Exercise':    Exercise
  }
});
