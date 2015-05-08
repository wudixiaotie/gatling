var connect_fail_count = 0;
var connect_max = 2;

var Gatling = {
  host: "",
  connect_fail_count: 0,
  connect_max: 10,
  start: function() {
    if(Gatling.host.length > 0) {
      Gatling.connect(Gatling.host);
    }
    else {
      console.log("undefine host for Gatling");
    }
  },
  connect: function(ws_host) {
    console.log("connecting to " + ws_host);
    var ws = new WebSocket(ws_host);

    if(ws.readyState == 1) {
      Gatling.connect_fail_count = 0;
      Gatling.record_and_send(ws);
    }
    else {
      Gatling.connect_fail_count += 1;

      if(Gatling.connect_fail_count < Gatling.connect_max) {
        var t = setTimeout(function() {
          Gatling.connect(ws_host);
        }, 5000);
      }
      else {
        console.log("reach to the max connect count, all failed, shut down!");
      }
    }
  },
  record_and_send: function(ws) {
    ws.send("this is test");
  }
}