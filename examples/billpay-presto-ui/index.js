const meta = require('./screenMeta');
const purescriptObj = require('./output/Core/index.js');
const init = require("presto-ui").init;

window.pages = require('./screens').pages;
init(meta, purescriptObj);
