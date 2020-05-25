
const loopedFunction = function(){
    return loopedFunction
}
const getTracker = function(){
    var trackerJson = JOS.tracker || {};
    if (typeof trackerJson.trackException != "function"){
        trackerJson.trackException = loopedFunction;
    }
    return trackerJson;
}
const tracker = getTracker();

exports._trackException = tracker._trackException