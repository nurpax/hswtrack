/** @jsx React.DOM */
define(['underscore', 'react'], function(_, React) {
  "use strict";

  var LoginError = React.createClass({
    render: function () {
      if (!this.props.loginError)
        return null;
      return (
        <div className="alert alert-danger alert-dismissable">
          <button type="button" className="close" data-dismiss="alert" aria-hidden="true">&times;</button>
          {this.props.loginError}
        </div>
      );
    }
  });

  var LoginScreen = React.createClass({
    handleSubmit: function () {
      var p = {
        login:    this.refs.login.getDOMNode().value,
        password: this.refs.password.getDOMNode().value,
      };
      if (this.refs.remember) {
        p.remember = this.refs.remember.getDOMNode().checked
      }
      this.props.onSubmit(p);
      return false;
    },

    render: function () {
      var remember =
        this.props.formType == "new_user" ?
                               null :
                               <div className="form-group row">
                                 <div className="col-md-12">
                                   <label>
                                     <input type="checkbox" ref="remember" defaultChecked />&nbsp;Remember me
                                   </label>
                                 </div>
                               </div>;

      return (
        <div className="well">
          <form onSubmit={this.handleSubmit}>
            <div className="form-group row">
              <div className="col-md-12">
                <input className="form-control" type="text" ref="login" placeholder="Username" />
              </div>
            </div>
            <div className="form-group row">
              <div className="col-md-12">
                <input className="form-control" type="password" ref="password" placeholder="Password" />
              </div>
            </div>
            {remember}
            <div className="form-group row">
              <div className="col-md-12">
                <button className="btn btn-primary" type="submit">
                  {this.props.formType == "new_user" ? "Create a New User" : "Login"}
                </button>
              </div>
            </div>

          </form>
        </div>)
      }
  });

  var NewUserForm = React.createClass({
    render: function () {
      return <LoginScreen formType="new_user"/>;
    }
  });

  var LoginForm = React.createClass({
    render: function () {
      return <LoginScreen formType="login"/>;
    }
  });

  var renderLogin = function (formType, context, onSubmit) {
    var createUserLink =
      formType == "login" ? <p>Don't have a login yet? <a href="/new_user">Create a new user</a></p> : null;

    React.renderComponent(
      <div>
        <LoginError loginError={context.loginError} />
        <LoginScreen formType={formType} onSubmit={onSubmit} />
        {createUserLink}
      </div>,
      document.getElementById('app-container'));
  }

  // export
  return {
    'renderLogin':  renderLogin
  };

});
