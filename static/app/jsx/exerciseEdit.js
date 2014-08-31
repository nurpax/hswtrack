/** @jsx React.DOM */
define(['underscore', 'react'], function(_, React) {
  "use strict";

  var ExerciseList = React.createClass({
    render: function () {
      var el = this.props.exercises.map(function (e) {
        return (
          <li key={e.id}>{e.name}</li>
        );
      });
      return (<div>{el}</div>);
    }
  });

  var NewExerciseForm = React.createClass({
    getInitialState: function () {
      return { exerciseType: 'W' }
    },
    typeChanged: function (e) {
      this.setState({ exerciseType: e.target.value });
    },

    handleSubmit: function () {
      var exerciseName = this.refs.name.getDOMNode().value.trim();
      if (!exerciseName) {
        return false;
      }
      this.props.onExerciseSubmit({ name: exerciseName, type: this.state.exerciseType });
      this.refs.name.getDOMNode().value = '';
      return false;
    },

    render: function () {
      return (
        <div className="well">
          <form id="new-exercise" onSubmit={this.handleSubmit}>
            <div className="row">
              <div className="col-md-4">
                <label htmlFor="exercise-name">Add a new exercise</label>
              </div>
              <div className="col-md-8">
                <input id="exercise-name" type="text" ref="name" placeholder="Exercise name.."/>
              </div>
            </div>

            <div className="row">
              <div className="col-md-12">
                <div className="radio">
                  <label>
                    <input type="radio" onChange={this.typeChanged} value="W" checked={this.state.exerciseType == "W"} />
                    Weighted exercises like barbell bench.  Weight input always required.
                  </label>
                </div>
                <div className="radio">
                  <label>
                    <input type="radio" onChange={this.typeChanged} value="BW" checked={this.state.exerciseType == "BW"}/>
                    Bodyweight exercises like push-ups or pull-ups.  Extra weight can be input but is not required.
                  </label>
                </div>
              </div>
            </div>
            <button id="new-workout" className="btn btn-primary" type="submit">Add</button>
          </form>
        </div>
      );
    }
  });

  var ExerciseEdit = React.createClass({
    getInitialState: function() {
      return { data: [] };
    },
    componentDidMount: function() {
      $.ajax({
        type: "GET",
        url: "/rest/exercise",
        success: function(data) {
          this.setState({data: data.payload});
        }.bind(this),
        error: function(xhr, status, err) {
          console.error(this.props.url, status, err.toString());
        }.bind(this)
      });
    },

    handleExerciseSubmit: function (exercise) {
      var exercises = this.state.data;
      $.ajax({
        url: "/rest/exercise",
        type: "POST",
        data: exercise,
        success: function (resp) {
          this.setState({ data: exercises.concat([resp.payload]) });
        }.bind(this),
        error: function(xhr, status, err) {
          console.error(this.props.url, status, err.toString());
        }.bind(this)
      });
    },

    render: function () {
      return (
        <div>
          <h4>Existing exercises:</h4>
          <ul className="list-unstyled">
            <ExerciseList exercises={this.state.data}/>
            <NewExerciseForm onExerciseSubmit={this.handleExerciseSubmit}/>
          </ul>
        </div>
      );
    }
  });

  var renderExerciseEditor = function () {
    React.renderComponent(
      <div>
        <ExerciseEdit />
        <a href="/workout">Back to workouts..</a>
      </div>,
      document.getElementById('app-container')
    );
  };

  return {
    'render': renderExerciseEditor
  };

});
