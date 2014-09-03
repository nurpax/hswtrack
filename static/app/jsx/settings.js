/** @jsx React.DOM */
define(['underscore', 'react'], function(_, React) {
  "use strict";

  var Settings = React.createClass({
    getInitialState: function() {
      return { context: { login: ""} };
    },

    componentDidMount: function() {
      $.ajax({
        type: "GET",
        data: null,
        url: "/rest/app",
        success: function(data) {
          this.setState(data.payload);
        }.bind(this)
      });
    },

    render: function () {
      return (
        <div>
          <h2>Settings</h2>
          <p>Hi {this.state.context.login} -- sorry no settings to edit yet.</p>
        </div>
      );
    }
  });

  var render = function () {
    React.renderComponent(<Settings />, document.getElementById('app-container'));
  };

  // export
  return {
    'render': render
  };

});
