/* Simple wrapper for phantom and talk */

try {
  var system = require('system');
  var page = require('webpage').create();

  page.onConsoleMessage = function(msg) {
    console.log("page: " + msg);
  };

  /*
    page.onError = function (msg, trace) {
    console.log(msg);
    trace.forEach(function(item) {
      console.log('  ', item.file, ':', item.line);
    });
  };
  */

  var url = "http://localhost:8005/talk/stuff/html/index.html";
  page.open(url, function (status) {        
    console.log("opening page");

    if (status != "success") {
      console.log("an error loading the page");
    }

    setTimeout(function () { phantom.exit(); }, 5000);
  });
}
catch (e) {
  console.log(e);
  phantom.exit();
}

// End
