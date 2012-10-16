var thrift = require('thrift'),
    ttransport = require('thrift/transport');

var UserStorage = require('./gen-nodejs/UserStorage'),
    ttypes = require('./gen-nodejs/user_types');

var users = {};

var store = function(user, result) {
  console.log("stored:", user.uid);
  users[user.uid] = user;
  result(null);
};
var retrieve = function(uid, result) {
  console.log("retrieved:", uid);
  result(null, users[uid]);
};

var server_framed = thrift.createServer(UserStorage, {
  store: store,
  retrieve: retrieve
});
server_framed.listen(9090);
var server_buffered = thrift.createServer(UserStorage, {
 store: store,
 retrieve: retrieve
}, {transport: ttransport.TBufferedTransport});
server_buffered.listen(9091);
