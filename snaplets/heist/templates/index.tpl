<apply template="base">

   <!-- Page content of course! -->
   <main class="bs-masthead" id="content" role="main">
     <div class="container">
       <h1>Weight Tracker 2.0</h1>
       <p class="lead">Sleek, intuitive, and powerful mobile first front-end framework for faster and easier web development.</p>
     </div>
   </main>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
