$script(["http://ajax.googleapis.com/ajax/libs/swfobject/2.2/swfobject.js",
         "/talk/stuff/libs/bonzo/bonzo.js",
         "/talk/stuff/libs/qwery/qwery.js"],
        "swfobject");

function $(selector) {return bonzo(qwery(selector));}
$script.ready("swfobject", function() {
  swfobject["talklog"] = function (str) {
    console.log(str);
  };

  var flashvars = {
    size: "600",
    height: "300",
    wowza: "some url",
    log: "swfobject.talklog",
  };

  var params = {
    allowScriptAccess: "always",
    wmode: "opaque",
    bgcolor: "#000000"
  };

  swfobject.embedSWF(
    "/talk/stuff/vidclient/vidclient.swf",
    "video",
    "600px",
    "300px",
    "10.0.0",
    "",
    flashvars,
    params
  );

  swfobject.talklog("js finished");
});
