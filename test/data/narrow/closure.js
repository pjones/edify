let makeCounter = function(startingValue) {
  let n = startingValue;

  // <<: closure
  return function() {
    return n += 1;
  };
  // :>>
};
