
var plaintalk = (function () {
  function $(selector) {return bonzo(qwery(selector));}
  var my_user_id = null;
  var other_user_id = null;
  var conversation_id = null;
  var waiting = null;
  return {
    nameinit: function (me, them, cont) {
      waiting = reqwest({
        url: '/talk/make/?me=' + me + '&them=' + them, 
        type: 'json',
        method: 'post',
        success: function (resp) {
          my_user_id = resp[me];
          other_user_id = resp[them];
          conversation_id = resp["_id"];
          document.cookie = "plaintalk_user=" + my_user_id + "; expires=Thu, 23 Aug 2013 20:47:11 UTC; path=/";
          cont();
        }
      });
    },

    // this is kinda deprecated....
    // it's the old init func, the new one is nameinit
    init: function (user, conversation) {
      userid = user;
      conversationid = conversation;
      return true;
    },

    comm: function () {
      try {
        waiting = reqwest('/talk/con/' + conversation_id + '/?userid=' + my_user_id, function (resp) {
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
      waiting = reqwest('/talk/to/?c=' + conversation_id + '&userid=' + my_user_id + '&text=' + text, function (resp) {
        waiting = null;
        console.log("talk responded with " + resp);
      });
      console.log("talk waiting: " + waiting);
      return waiting;
    }
  };
})();
