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

</script>

<script id="settings-template" type="text/x-handlebars-template">
  Settings for {{login}}
</script>
