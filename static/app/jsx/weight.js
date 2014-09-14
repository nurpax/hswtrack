/** @jsx React.DOM */
define(['underscore', 'react', 'app/jsx/model'], function(_, React, model) {
  "use strict";

  var DEFAULT_DAYS = 90;

  var TodayWeight = React.createClass({
    handleClearWeight: function () { 
      this.props.onClearWeight();
    },

    handleWeightSubmit: function () {
      var w = this.refs.weight.getDOMNode().value.trim();
      if (!w) {
        return false;
      }
      this.props.onWeightSubmit(w);
      this.refs.weight.getDOMNode().value = '';
      return false;
    },

    render: function () {
      if (this.props.weight) {
        return (
          <div>
            <p>Your weight today is: {this.props.weight.weight} kg &nbsp;
            <button onClick={this.handleClearWeight} className="btn btn-default btn-xs">Clear</button>
            </p>
          </div>
        );
      } else {
        return (
          <div className="well">
            <p>Please enter your weight (kg):</p>
            <input ref="weight" type="number" placeholder="Enter weight.."></input>
            <button onClick={this.handleWeightSubmit} className="btn btn-primary">Save</button>
          </div>
        );
      }
    }
  });

  var Weights = React.createClass({
    getInitialState: function () {
      this.props.model.setStateCB = function (s) { this.setState(s); }.bind(this);
      return this.props.model;
    },

    componentDidMount: function () {
      this.props.model.load(DEFAULT_DAYS);
    },

    clearWeight: function () {
      this.props.model.clearWeight();
    },

    setWeight: function (newWeight) {
      this.props.model.setWeight(newWeight);
    },

    render: function () {
      if (!this.state.app)
        return null;
      return (
        <div>
          <p>Hi {this.state.app.context.login}!</p>
          <TodayWeight onWeightSubmit={this.setWeight}
                       onClearWeight={this.clearWeight}
                       weight={this.state.app.context.weight} />
        </div>
      );
    }
  });

  var render = function () {
    React.renderComponent(<Weights model={new model.WeightTop()} />, document.getElementById('app-container'));
  };

  // export
  return {
    'render': render
  };

});
