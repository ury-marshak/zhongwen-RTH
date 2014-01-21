
// usage: node icedict.js "/Users/ury/work/popup/data/cedict_ts.u8" >cedict.idx


if (process.argv.length<3) {
    process.stderr.write("usage: node icedict.js /path/to/cedict_ts.u8 >cedict.idx\n");
    process.exit(1);
}



var pos = 0;

var wordsIndex = {};
var wordsList = [];

function addWord(word, pos) {
    if ((word != "") && (word != undefined)) {

        // remove commas
        word = word.split(",")[0];
        
        if (wordsIndex[word] == undefined) {
            wordsIndex[word] = [];
            wordsList.push(word);
        }
        
        var arr = wordsIndex[word];
        if (arr.indexOf(pos) == -1) {
            arr.push(pos);
        }    
    }
}

require('fs').readFileSync(process.argv[2])
    .toString()
    .split(/\n/)
    .forEach(function(line){

        var words = line.split(" ");
        var word_s = words[0];
        var word_t = words[1];

        if (line[0] !== "#") {
            // console.log(pos);
            // console.log(word_s);
            addWord(word_s, pos);
            addWord(word_t, pos);
        }
        pos += line.length + 1;
});

wordsList.sort();

for(var i = 0; i < wordsList.length; i++) {
    var word = wordsList[i];
    var positions  = wordsIndex[word];
    console.log(word+","+positions);
}


//console.log(wordsList);
