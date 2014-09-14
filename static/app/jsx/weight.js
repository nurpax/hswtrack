/** @jsx React.DOM */
define(['underscore', 'react', 'app/jsx/model'], function(_, React, model) {
  "use strict";

  var DEFAULT_DAYS = 90;

  var Comment = React.createClass({
    deleteComment: function () {
      if (!confirm("OK to delete note?"))
        return false;
      this.props.onDeleteComment(this.props.id);
      return false;
    },

    render: function () {
      return <li>{this.props.text} <a onClick={this.deleteComment} href="#">&times;</a></li>
    }
  });

  var CommentList = React.createClass({
    getInitialState: function () {
      this.props.model.setStateCB = function (s) { this.setState(s); }.bind(this);
      return this.props.model;
    },

    handleDeleteComment: function (id) {
      this.props.model.deleteById(id);
    },

    handleAddComment: function () {
      var text = this.refs.comment.getDOMNode().value.trim();
      if (!text) {
        return false;
      }
      this.props.model.addNote(text);
      this.refs.comment.getDOMNode().value = '';
      return false;
    },

    render: function () {
      var comments = this.state.notes.map(function (c) {
        return <Comment onDeleteComment={this.handleDeleteComment}
                        key={c.id}
                        id={c.id}
                        text={c.text}/>;
      }.bind(this));

      return (
        <div>
          <h4>Comments</h4>
          <div className="col-md-6">
            <ul className="list-unstyled">
              {comments}
              <li>
                <form onSubmit={this.handleAddComment}>
                  <input ref="comment" size="24" type="text" placeholder="Add comment.."></input>
                  <button className="btn btn-default btn-xs">Save</button>
                </form>
              </li>
            </ul>
          </div>
        </div>
      );
    }
  });

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
            <form onSubmit={this.handleWeightSubmit}>
              <input ref="weight" type="number" placeholder="Enter weight.."></input>
              <button className="btn btn-primary">Save</button>
            </form>
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
          <br/>
          <CommentList model={this.state.notes}/>
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
