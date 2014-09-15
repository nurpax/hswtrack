/** @jsx React.DOM */
define(['underscore', 'react', 'd3', 'jsx/model'], function(_, React, d3, model) {
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

  function renderPlotPriv(props, svg, origWidth, origHeight) {
    var self    = this;
    var data    = props.weights.weights;

    var margin = { top: 10, right: 25, bottom: 30, left: 25 },
        width  = origWidth - margin.left - margin.right,
        height = origHeight - margin.top - margin.bottom;

    var parseDate = d3.time.format("%Y-%m-%d").parse;

    var x = d3.time.scale()
                   .range([0, width]);

    var y = d3.scale.linear()
                    .range([height, 0]);

    var xAxis = d3.svg.axis()
                      .scale(x)
                      .orient("bottom");

    var yAxis = d3.svg.axis()
                      .scale(y)
                      .orient("left");

    var line = d3.svg.line()
                     .x(function(d) { return x(d.date); })
                     .y(function(d) { return y(d.weight); });

    // TODO enter() etc?? The below code just force-renders the whole plot
    // by deleting all the SVG elems.
    svg.selectAll("g").remove();

    var svgg =
      svg.attr("width", width + margin.left + margin.right)
         .attr("height", height + margin.top + margin.bottom)
         .append("g")
         .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var pd = data.map(function (d) { return { date: parseDate(d.date), weight: d.weight }; });
    x.domain(d3.extent(pd, function(d) { return d.date; }));

    if (props.context.options.minGraphWeight)
      y.domain([props.context.options.minGraphWeight, d3.max(pd, function(d) { return d.weight; })]);
    else
      y.domain(d3.extent(pd, function(d) { return d.weight; }));

    svgg.append("g")
       .attr("class", "x axis")
       .attr("transform", "translate(0," + height + ")")
       .call(xAxis);

    svgg.append("g")
       .attr("class", "y axis")
       .call(yAxis)
       .append("text")
       .attr("transform", "rotate(-90)")
       .attr("y", 6)
       .attr("dy", ".71em")
       .style("text-anchor", "end")
       .text("Weight (kg)");

    svgg.append("path")
       .datum(pd)
       .attr("class", "line")
       .attr("d", line);
  }

  function renderPlot(elt, props) {
    return function(me) {
      var width  = elt.offsetWidth;
      var height = width / 2;
      renderPlotPriv(props, me, width, height);
    };
  }

  var WeightPlot = React.createClass({
    componentDidMount: function () {
      var elt = this.getDOMNode();
      d3.select(elt).call(renderPlot(elt, this.props));
    },
    shouldComponentUpdate: function(props) {
      var elt = this.getDOMNode();
      d3.select(elt).call(renderPlot(elt, props));
      // always skip React's render step
      return false;
    },

    render: function () {
      return <svg width="100%" height="100%" className="col-md-12 weight-plot" />;
    }
  });

  var Weights = React.createClass({
    getInitialState: function () {
      this.props.model.setStateCB = function (s) { this.setState(s); }.bind(this);
      return this.props.model;
    },

    reloadWeights: function (nDays) {
      this.props.model.loadWeights(nDays);
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

    selectRange: function (e) {
      this.reloadWeights(e.target.value);
    },

    render: function () {
      var choices = [{t:"3 months",  n:90},
                     {t:"12 months", n:365},
                     {t:"24 months", n:365*2},
                     {t:"Lifetime",  n:0}];
      var radios = choices.map(function (c, ndx) {
        var cl = "btn btn-default btn-sm";
        var classes = c.n == this.state.selectedRange ? cl+" active" : cl;
        return (
          <label key={c.n} className={classes}>
            <input onClick={this.selectRange} value={c.n} type="radio" name="graph-range" /><small>{c.t}</small>
          </label>);
      }.bind(this));

      if (!this.state.app)
        return null;
      return (
        <div>
          <TodayWeight onWeightSubmit={this.setWeight}
                       onClearWeight={this.clearWeight}
                       weight={this.state.app.context.weight} />
          <br/>

          <div>
            <div className="row">
              <WeightPlot weights={this.state.weights} context={this.state.app.context} />
            </div>
            <div className="btn-group" data-toggle="buttons">
              {radios}
            </div>
          </div>
          <br/>
          <CommentList model={this.state.notes} />
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
