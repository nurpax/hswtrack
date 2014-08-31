/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model', 'jsx/workout'], function(_, React, model, workout) {
  "use strict";

  var Exercise = workout.Exercise;
  var Workout  = workout.Workout;

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

  // export
  return {
    'render': renderStats
  };

});
