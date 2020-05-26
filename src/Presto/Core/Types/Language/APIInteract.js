
const loopedFunction = function(){
    return loopedFunction
}
const getTracker = function(){
    var trackerJson = JOS.tracker || {};
    if (typeof trackerJson._trackException != "function"){
        trackerJson._trackException = loopedFunction;
    }
    return trackerJson;
}
const tracker = getTracker();

exports._trackException = tracker._trackException