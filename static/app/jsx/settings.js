/** @jsx React.DOM */
define(['jquery', 'underscore', 'react', 'jsx/components'], function($, _, React, components) {
  "use strict";

  var AlertGeneric = components.AlertGeneric;
  var FormGroup    = components.FormGroup;

  var ChangePasswordForm = React.createClass({
    getInitialState: function () {
      return {
        msg: null,
        alertClass: null
      };
    },

    onUpdatePasswordSubmit: function () {
      var pass1 = this.refs.password1.getDOMNode().value;
      var pass2 = this.refs.password2.getDOMNode().value;

      this.refs.password1.getDOMNode().value = '';
      this.refs.password2.getDOMNode().value = '';

      if (pass1 != pass2) {
        this.setState({ alertClass:"alert-danger", msg: "Both passwords must be the same!" });
        return false;
      }

     if (pass1 == '') {
        this.setState({ alertClass:"alert-danger", msg: "Password cannot be empty!" });
        return false;
      }

      $.ajax({
        url: "/rest/user",
        type: "PUT",
        dataType: "json",
        data: JSON.stringify({ password: pass1 }),
        success: function (resp) {
          this.setState({ alertClass:"alert-success", msg:"Password successfully updated." });
        }.bind(this)
      });

      return false;
    },

    render: function () {
      var alert = this.state.msg ? <AlertGeneric alertClass={this.state.alertClass}
                                                 message={this.state.msg} /> : null;
      return (
        <div>
          {alert}
          <FormGroup onSubmit={this.onUpdatePasswordSubmit}
                     submitTitle="Update Password">

            <div className="form-group row">
              <div className="col-md-12">
                <input className="form-control" ref="password1" type="password"
                       placeholder="New password.." />
              </div>
            </div>

            <div className="form-group row">
              <div className="col-md-12">
                <input className="form-control" ref="password2" type="password"
                       placeholder="Repeat your new password.." />
              </div>
            </div>
          </FormGroup>
        </div>
      );
    }

  });

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
          <p>Hi {this.state.context.login}!</p>
          <h3>Change Password</h3>
          <ChangePasswordForm />
        </div>
      );
    }
  });

  var render = function () {
    React.render(<Settings />, document.getElementById('app-container'));
  };

  // export
  return {
    'render': render
  };

});
