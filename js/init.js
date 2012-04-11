$script(["/talk/stuff/bonzo/bonzo.js",
         "/talk/stuff/qwery/qwery.js",
         "/talk/stuff/reqwest/reqwest.js"], "plaintalk-bundle");

$script.ready("plaintalk-bundle", function() {
  $script("/talk/stuff/js/client.js", function () {
    plaintalk.comm();
  });
});
