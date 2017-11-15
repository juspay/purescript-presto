window.__duiShowScreen = function(callBack, data) {
  window.callBack = callBack;
  document.getElementById("text").innerHTML = data.contents;
};

document.getElementById("button").addEventListener("click", function() {
  window.callBack("{}")();
});
