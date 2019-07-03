exports.getValueFromLocalStoreImpl = function(key) {
  return JBridge.getFromSharedPrefs(key);
};

exports.setValueToLocalStoreImpl = function(key, value) {
  return JBridge.setInSharedPrefs(key, value);
};

exports.deleteValueFromLocalStoreImpl = function(key){
  JBridge.removeDataFromSharedPrefs(key);
};