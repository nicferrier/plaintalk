window.done = function () { console.log("done"); }; // this is for phantomjs - probably can drop it.
$script(["/talk/stuff/js/swfobject.js", 
         "/talk/stuff/libs/bonzo/bonzo.js",
         "/talk/stuff/libs/qwery/qwery.js",
         "/talk/stuff/libs/reqwest/reqwest.js"],function () {
           $script(["/talk/stuff/js/client.js",
                    "/talk/stuff/js/videoclient.js"],
                   "plaintalk-bundle");
         });

$script.ready("plaintalk-bundle", function() {
  function $(selector) {return bonzo(qwery(selector));}
  console.log("init");
});
