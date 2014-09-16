/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model', 'jsx/workout'], function(_, React, model, workout) {
  "use strict";

  // Put content hidden behind a clickable link.  Once the link is 
  // clicked, the hidden child content is displayed.
  var Unhide = React.createClass({
    getInitialState: function () {
      return { 
        visible: false
      };
    },

    toggleVis: function (e) {
      this.setState( { visible: !this.state.visible } );
    },

    render: function () {
      var vis    = this.state.visible  ? "show" : "hide";
      var notVis = !this.state.visible ? "show" : "hide";

      return (
        <div>
          <a onClick={this.toggleVis} className={notVis} href="#">{this.props.title}</a>
          <div className={vis}>
            {this.props.children}
          </div>
        </div>
      );
    }
  });

  var Tabs = React.createClass({
    getInitialState: function () {
      return { selectedTab : 0 };
    },

    selectTab: function (e) {
      this.setState({ selectedTab: e.target.dataset.id });
    },

    selectedClass: function (idx) {
      return this.state.selectedTab == idx ? "active" : "";
    },

    render: function () {
      var tabHeaders = this.props.tabs.map(function (tabName, idx) {
        var cls = this.selectedClass(idx);
        return (
          <li key={idx} className={cls}>
            <a onClick={this.selectTab} data-id={idx} href="#" data-toggle="tab">{tabName}</a>
          </li>
        );
      }.bind(this));
      var tabContent = this.props.children.map(function (child, idx) {
        var cls = "tab-pane " + this.selectedClass(idx);
        return <div key={idx} className={cls}>{child}</div>;
      }.bind(this));
      return (
        <div className="container">
          <ul className="nav nav-tabs">
            {tabHeaders}
          </ul>
          <div className="tab-content">
            {tabContent}
          </div>
        </div>
      );
    }
  });


  return {
    'Unhide': Unhide,
    'Tabs': Tabs
  }
});
