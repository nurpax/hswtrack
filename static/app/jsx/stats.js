/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model', 'jsx/components', 'jsx/workout'], function(_, React, model, components, workout) {
  "use strict";

  var Exercise = workout.Exercise;
  var Workout  = workout.Workout;
  var Tabs     = components.Tabs;

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
      <Tabs tabs={["History", "Personal Records"]}>
        <WorkoutList />
        <div>
          Personal records not implemented yet.
        </div>
      </Tabs>,
      document.getElementById('app-container')
    );
  };

  // export
  return {
    'render': renderStats
  };

});
