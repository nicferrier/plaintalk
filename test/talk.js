/* Simple wrapper for phantom and talk */

try {
  var system = require('system');
  var page = require('webpage').create();

  page.onConsoleMessage = function(msg) {
    if (msg == "__quit__") {
      console.log("all done");
      phantom.exit();
    }
    else {
      console.log("page: " + msg);
    }
  };

  page.onError = function (msg, trace) {
    console.log(msg);
    trace.forEach(function(item) {
      console.log('  ', item.file, ':', item.line);
    });
  };

  var url = "http://localhost:8005/talk/stuff/html/index.html";
  page.open(url, function (status) {        
    console.log("page opened");
    if (status != "success") {
      console.log("page load error");
    }
    else {
      page.evaluate(function () {
        $script("/talk/stuff/js/init.js", function () {
          window.done = function () { console.log("__quit__"); };
        });
      });
    }
  });
}
catch (e) {
  console.log(e);
  phantom.exit();
}

// End
