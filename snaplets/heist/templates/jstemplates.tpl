<script id="home-template" type="text/x-handlebars-template">
  <p>Hello {{login}}</p>
  {{#weight}}
  <p>Your weight today: {{../weight}}</p>
  {{/weight}}
  {{^weight}}
  <p>No weight specified for today.  Enter it</p>
  {{/weight}}
</script>

<script id="settings-template" type="text/x-handlebars-template">
  Settings for {{login}}
</script>
