<script id="home-template" type="text/x-handlebars-template">
  <p>Hello {{login}}</p>
  {{#weight}}
  <p>Your weight today: {{../weight}}</p>
  {{/weight}}
  {{^weight}}
  <p>No weight specified for today.  Enter it</p>
  {{/weight}}

  <div class="row">
    <div id="weight-plot" class="col-md-12"></div>
  </div>

  <div class="btn-group" data-toggle="buttons">
    <label class="btn btn-default btn-sm" id="graph-3-mo">
      <input type="radio" name="graph-range"><small>3 months</small>
    </label>
    <label class="btn btn-default btn-sm" id="graph-12-mo">
      <input type="radio" name="graph-range"><small>12 months</small>
    </label>
    <label class="btn btn-default btn-sm" id="graph-24-mo">
      <input type="radio" name="graph-range"><small>24 months</small>
    </label>
    <label class="btn btn-default btn-sm" id="graph-all">
      <input type="radio" name="graph-range"><small>Lifetime</small>
    </label>
  </div>

</script>

<script id="settings-template" type="text/x-handlebars-template">
  Settings for {{login}}
</script>
