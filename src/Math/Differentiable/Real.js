// module Math.Differentiable.Real

exports.modImpl = function (x) {
  return function (y) {
    return x % y;
  }
}
