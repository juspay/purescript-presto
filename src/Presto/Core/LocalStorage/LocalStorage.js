exports.getValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.fetchAndDecrypt) {
    try {
      return window.JOS.fetchAndDecrypt(key);
    } catch (err) { }
  }
  return JBridge.getFromSharedPrefs(key);
};

exports.setValueToLocalStoreImpl = function(key, value) {
  if (window.JOS && window.JOS.encryptAndStore) {
    try {
      return window.JOS.encryptAndStore(key)(value);
    } catch (err) { }
  }
  return JBridge.setInSharedPrefs(key, value);
};

exports.deleteValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.deleteEncKeys) {
    try {
      window.JOS.deleteEncKeys(key);
    } catch (err) {
      JBridge.removeDataFromSharedPrefs(key);
    }
  } else {
    JBridge.removeDataFromSharedPrefs(key);
  }
};