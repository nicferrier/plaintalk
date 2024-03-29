window.done = function () { console.log("done"); }; // this is for phantomjs - probably can drop it.
$script(["/talk/stuff/js/swfobject.js", 
         "/talk/stuff/libs/bonzo/bonzo.js",
         "/talk/stuff/libs/bean/bean.js",
         "/talk/stuff/libs/qwery/qwery.js",
         "/talk/stuff/libs/humane-js/humane.js",
         "/talk/stuff/libs/reqwest/reqwest.js"],function () {
           $script(["/talk/stuff/js/client.js",
                    "/talk/stuff/js/videoclient.js"],
                   "plaintalk-bundle");
         });

$script.ready("plaintalk-bundle", function() {
  function $(selector) {return bonzo(qwery(selector));}

  bean.add($("#content .button")[0], "click", function (evt) {
    $("#content .button").addClass("hidden");
    $("#talkform input")[0].tabindex = 0;
    $("#talkform input").each(function (e) {
      bean.add(e, "change", function (evt) {
        if ($("#talkform form")[0].checkValidity()) {
          $("#talkform .button").removeClass("disabled");
        }
        else {
          $("#talkform .button").addClass("disabled");
        }
      });
    });
    $("#talkform").removeClass("hidden");
  });

  bean.add($("#talkform .button")[0], "click", function (evt) {
    if ($("#talkform form")[0].checkValidity()) {
      $("#talkform").addClass("hidden");
      plaintalk.nameinit(
        $("#talkform form input[name=me]")[0].value,
        $("#talkform form input[name=them]")[0].value,
        function () {
          plaintalk_video.display();
        }
      );
    }
  });
});
