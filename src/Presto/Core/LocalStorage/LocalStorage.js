exports.getValueFromLocalStoreImpl = function(key) {
  return JBridge.getFromSharedPrefs(key);
};

exports.setValueToLocalStoreImpl = function(key, value) {
  JBridge.setInSharedPrefs(key, value);
};
