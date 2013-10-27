<apply template="base">

  <ifLoggedIn>
<apply template="jstemplates"/>
  </ifLoggedIn>

  <!-- Page content of course! -->
  <main class="bs-masthead" id="content" role="main">
    <div class="container">
      <!-- App contents will be applied into this div by JS -->
      <ifLoggedIn>
        <div id="app-container">
        </div>
      </ifLoggedIn>

    </div>
  </main>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
