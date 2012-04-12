
var plaintalk = (function () {
  function $(selector) {return bonzo(qwery(selector));}
  return {
    comm: function () {
      try {
        console.log("inside comm");
        reqwest('/talk/to/?c=1&userid=u33223', function (resp) {
          console.log(resp);
          window.done();
        });
        console.log("after talk/to call");
      }
      catch (e) {
        console.log("whoops! " + e);
      }
    },
    
    talk: function (userid, text) {
    }
  };
})();
