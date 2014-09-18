/** @jsx React.DOM */
define(['underscore', 'react', 'jsx/model'], function(_, React, model) {
  "use strict";

  var AlertGeneric = React.createClass({
    getInitialState: function () {
      return { visibleClass: "show" };
    },

    onClick: function () {
      this.setState({ visibleClass: "hide" });
    },

    render: function () {
      var cls = "alert alert-dismissable " + this.props.alertClass + " " + this.state.visibleClass;
      return (
        <div className={cls}>
          <button onClick={this.onClick}
                  type="button"
                  className="close"
                  data-dismiss="alert" aria-hidden="true">&times;</button>
          {this.props.message}
        </div>
      );
    }
  });

  var Alert = React.createClass({
    render: function () {
      if (!this.props.error)
        return null;
      return <AlertGeneric message={this.props.error} alertClass="alert-danger" />;
    }
  });

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

  var FormGroup = React.createClass({
    render: function () {
      return (
        <div className="well">
          <form onSubmit={this.props.onSubmit}>
            {this.props.children}
            <div className="form-group row">
              <div className="col-md-12">
                <button className="btn btn-primary" type="submit">{this.props.submitTitle}</button>
              </div>
            </div>
          </form>
        </div>
      );
    }
  });

  return {
    'Alert':        Alert,
    'AlertGeneric': AlertGeneric,
    'Unhide':       Unhide,
    'Tabs':         Tabs,
    'FormGroup':    FormGroup
  };
});
