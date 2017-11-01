exports["getValueFromLocalStore'"] = function(key) {
    return function() {
        return JBridge.getFromSharedPrefs(key);
    };
};

exports["setValueToLocalStore'"] = function(key) {
    return function(value) {
        return function() {
            JBridge.setInSharedPrefs(key, value);
        };
    };
};
