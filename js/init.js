window.done = function () { console.log("done"); };
$script(["/talk/stuff/libs/bonzo/bonzo.js",
         "/talk/stuff/libs/qwery/qwery.js",
         "/talk/stuff/libs/reqwest/reqwest.js"], "plaintalk-bundle");

$script.ready("plaintalk-bundle", function() {
  $script("/talk/stuff/js/client.js", function () {
    // Should probably do something to signal readyness?
  });
});
