
const loopedFunction = function(){
    return loopedFunction
}
const getTracker = function(){
    var trackerJson = window.JOS && window.JOS.tracker || {};
    if (typeof trackerJson._trackException != "function"){
        trackerJson._trackException = loopedFunction;
    }
    return trackerJson;
}
const tracker = getTracker();

exports._trackException = function(category){
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

exports._trackApiCall = function(resp){
    tracker._trackAction("system")("debug")("api_response")(resp)();
}