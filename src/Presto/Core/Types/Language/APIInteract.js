
const loopedFunction = function(){
  return loopedFunction
}
const getTracker = function(){
  var trackerJson = window.JOS && window.JOS.tracker || {};
  if (typeof trackerJson._trackException != "function"){
    trackerJson._trackException = loopedFunction;
  }
  if (typeof trackerJson._trackAction != "function"){
    trackerJson._trackAction = loopedFunction;
  }
  return trackerJson;
}
const tracker = getTracker();

export const _trackException = function(category){
  return function(subcategory){
    return function(label){
      return function(key){
        return function(value) {
          tracker._trackException(category)(subcategory)(label)(key)(value)();
        }
      }
    }
  }
}

export const _trackApiCall = function(resp){
  tracker._trackAction("system")("debug")("api_response")(resp)();
}

export const _trackNetworkRetrySuccess = function(resp){
  tracker._trackAction("system")("info")("network_retry_success")(resp)();
}