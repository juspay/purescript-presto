exports.getValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.fetchAndDecrypt) {
    return window.JOS.fetchAndDecrypt(key);
  } else {
    return JBridge.getFromSharedPrefs(key);
  }
};

exports.setValueToLocalStoreImpl = function(key, value) {
  if (window.JOS && window.JOS.encryptAndStore) {
    return window.JOS.encryptAndStore(key)(value);
  } else {
    return JBridge.setInSharedPrefs(key, value);
  }
};

exports.deleteValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.encryptAndStore) {
    window.JOS.deleteEncKeys(key);
  } else {
    JBridge.removeDataFromSharedPrefs(key);
  }
};