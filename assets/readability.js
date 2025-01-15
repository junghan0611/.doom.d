var { Readability } = require('@mozilla/readability');
var { JSDOM } = require('jsdom');
var fs = require("fs");
var str = fs.readFileSync(process.stdin.fd).toString();
var doc = new JSDOM(str);
var reader = new Readability(doc.window.document);
var article = reader.parse();
console.log(article.content);
