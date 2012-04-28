var plaintalk_video = (function() {
  function $(selector) {return bonzo(qwery(selector));}
  swfobject["talklog"] = function (str) {
    console.log(str);
  };
  return {
    flash_inited: function () {
      var camlist = swfobject.getObjectById('video').cameras();
      swfobject.getObjectById('video').camera_select(camlist);
    },

    display: function () {
      // Work out the height of the video.
      var height = $("#video")[0].clientHeight;
      var flashvars = {
        size: "2",
        height: "" + height,
        wowza: "some url",
        log: "swfobject.talklog",
        flash_inited: "plaintalk_video.flash_inited",
      };
      var params = {
        allowScriptAccess: "always",
        wmode: "opaque",
        bgcolor: "#000000",
      };

      swfobject.embedSWF(
        "/talk/stuff/vidclient/vidclient.swf",
        "video",
        "" + (height * 1.3333 * 2) + "px",
        height + "px",
        "10.0.0",
        "",
        flashvars, params
      );
    }
  };
})();
