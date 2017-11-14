var sourceMap = require('source-map');
var fs = require('fs');
var lineNo;
var colNo;

// Usage: node debug.js lineNo, colNo
process.argv.forEach(function (val, index, array) {
	if (index == 2)
	lineNo = val;
	if (index == 3)
	colNo = val;
});

fs.readFile('./dist/index_bundle.js.map', 'utf8', function(err, contents) {
  var file_data=JSON.parse(contents);
  var smc = new sourceMap.SourceMapConsumer(file_data);
  console.log(smc.originalPositionFor({
 		line: lineNo,
 		column: colNo,
	}));
});