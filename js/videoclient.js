var plaintalk_video = (function() {
  function $(selector) {return bonzo(qwery(selector));}
  swfobject["talklog"] = function (str) {
    console.log(str);
  };
  return {
    display: function () {
      if (document.location.hostname == "localhost") {
        /** 
            FIXME
            
            humane doesn't seem to work??
        **/
        console.log("localhost and flash video == no no");
        humane.timeout = 8000;
        humane.error("localhost does not work with flash video");
      }
      // Work out the height of the video.
      var height = $("#video")[0].clientHeight - 1;
      var width = $("#video")[0].clientWidth - 1;
      swfobject.talklog("height = " + height);
      var flashvars = {
        size: "2",
        height: "" + height,
        width: "" + width,
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
        width + "px",
        height + "px",
        "10.0.0",
        "",
        flashvars, params
      );

      $("#chat").addClass("enabled");
      $("input[name=chatline]").removeAttr("disabled");
      bean.add($("form[name=chat]")[0], "submit", function (evt) {
        var to_send = $("input[name=chatline]")[0].value;
        $("input[name=chatline]")[0].value = "";
      });
      $("#filler").addClass("hidden");
    },

    flash_inited: function () {
      var camlist = swfobject.getObjectById('video').cameras();
      swfobject.getObjectById('video').camera_select(camlist);
    },

  };
})();
