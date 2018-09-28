exports.getValueFromLocalStoreImpl = function(key) {
  return JBridge.getFromSharedPrefs(key);
};

exports.setValueToLocalStoreImpl = function(key, value) {
  return JBridge.setInSharedPrefs(key, value);
};
