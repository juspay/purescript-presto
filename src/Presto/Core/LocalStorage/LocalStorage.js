export const getValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.fetchAndDecrypt) {
    try {
      return window.JOS.fetchAndDecrypt(key);
    } catch (err) { }
  }
  return window.JBridge.getFromSharedPrefs(key);
};

export const setValueToLocalStoreImpl = function(key, value) {
  if (window.JOS && window.JOS.encryptAndStore) {
    try {
      return window.JOS.encryptAndStore(key)(value);
    } catch (err) { }
  }
  return window.JBridge.setInSharedPrefs(key, value);
};

export const deleteValueFromLocalStoreImpl = function(key) {
  if (window.JOS && window.JOS.deleteEncKeys) {
    try {
      window.JOS.deleteEncKeys(key);
    } catch (err) {
      window.JBridge.removeDataFromSharedPrefs(key);
    }
  } else {
    window.JBridge.removeDataFromSharedPrefs(key);
  }
};