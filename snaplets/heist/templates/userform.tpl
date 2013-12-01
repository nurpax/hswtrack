<div class="col-md-3">

    <div class="alert alert-error">
       <a class="close" data-dismiss="alert" href="#">×</a><loginError/>
    </div>

    <div class="well">
      <form method="post" action="${postAction}">
      <div class="form-group row">
          <div class="col-md-12">
              <input class="form-control" type="text" id="username" name="login" placeholder="Username" />
          </div>
      </div>

      <div class="form-group row">
          <div class="col-md-12">
              <input class="form-control" type="password" id="password" name="password" placeholder="Password" />
          </div>
      </div>

      <div class="form-group row">
        <div class="col-md-12">
          <label>
            <input type="checkbox" id="remember" name="remember" value="1">Remember me
          </label>
        </div>
      </div>

      <div class="row">
         <button class="btn btn-primary col-md-12" type="submit"><submitText/></button>
      </div>

    </form>
    </div>

    <p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>

</div>
