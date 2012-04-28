var plaintalk_video = (function() {
  swfobject["talklog"] = function (str) {
    console.log(str);
  };
  var height = 320;
  var flashvars = {
    size: "2",
    height: "" + height,
    wowza: "some url",
    log: "swfobject.talklog",
  };

  var params = {
    allowScriptAccess: "always",
    wmode: "opaque",
    bgcolor: "#000000"
  };

  return {
    display: function () {
      swfobject.embedSWF(
        "/talk/stuff/vidclient/vidclient.swf",
        "video",
        "" + (height * 1.3333 * 2) + "px",
        height + "px",
        "10.0.0",
        "",
        flashvars,
        params
      );
    }
  };
})();
