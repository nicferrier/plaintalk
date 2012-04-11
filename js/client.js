
var plaintalk = (function () {
  function $(selector) {return bonzo(qwery(selector));}
  return {
    comm: function () {
      try {
        console.log("inside comm");
        reqwest('/talk/stuff/html/frag.html', function (resp) {
          $('#content').html(resp);
          console.log($('#content').html());
        });
      }
      catch (e) {
        console.log("whoops! " + e);
      }
    },
    
    talk: function (userid, text) {
    }
  };
})();
