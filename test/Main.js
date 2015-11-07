// module Test.Main

exports.failure = function (msg) {
  return function () {
    console.log(msg);
    process.exit(1);
  }
}
