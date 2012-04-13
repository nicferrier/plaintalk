
var plaintalk = (function () {
  function $(selector) {return bonzo(qwery(selector));}
  var userid = null;
  var conversationid = null;
  var waiting = null;
  return {
    init: function (user, conversation) {
      userid = user;
      conversationid = conversation;
      return true;
    },

    comm: function () {
      try {
        waiting = reqwest('/talk/to/?c=' + conversationid + '&userid=' + userid, function (resp) {
          waiting = null;
          console.log("comm responded with " + resp);
        });
        console.log("comm waiting: " + waiting);
      }
      catch (e) {
        console.log("comm failure: " + e);
      }
      return waiting;
    },
    
    talk: function (text) {
      if (waiting != null) {
        waiting.abort();
        waiting = null;
      }
      waiting = reqwest('/talk/to/?c=' + conversationid + '&userid=' + userid + '&text=' + text, function (resp) {
        waiting = null;
        console.log("talk responded with " + resp);
      });
      console.log("talk waiting: " + waiting);
      return waiting;
    }
  };
})();
