/* Create an element showing a group of results */
function createResults(name, results) {
    var div = $(document.createElement('div'));
    div.append($(document.createElement('h2')).html(name));

    for(i in results) div.append(createResult(results[i]));

    return div;
}

/* Create an element showing a given result */
function createResult(result) {
    var div = $(document.createElement('div'));
    var canvas = document.createElement('canvas');
    $(canvas).attr('width', '500px').attr('height', '30px');
    div.append(canvas); 

    var ctx = canvas.getContext('2d');
    var width = canvas.width;
    var height = canvas.height;

    /* Bar */
    ctx.fillStyle = barColor(result.normalizedMean);
    ctx.fillRect(0, 0, result.normalizedMean * width, height);

    /* Benchmark name */
    ctx.fillStyle = 'black';
    ctx.font      = 'bold 16px sans-serif';
    ctx.textAlign = 'left';
    ctx.fillText(result.name, 10, 23);
    ctx.font      = '16px sans-serif';
    ctx.textAlign = 'right';
    ctx.fillText(result.mean + 's', width - 10, 23);

    return div;
}

/* Calculate the bar color, based on normalized mean */
function barColor(normalizedMean) {
    var r, g, b;
    var round = function(x) { return Math.round(255 * x); };
    r = 0.8 * normalizedMean;
    g = 1 - 0.8 * normalizedMean;
    b = 0.0;
    return 'rgb(' + round(r) + ', ' + round(g) + ', ' + round(b) + ')';
}

/* Calculate normalized means */
function normalizeMeans(results) {
    var max = 0;
    for(i in results) if(results[i].mean > max) max = results[i].mean;
    for(i in results) results[i].normalizedMean = results[i].mean / max;
}

/* Main handler, create the page */
$(function() {
    for(i in criterionResults) {
        var group = criterionResults[i];
        normalizeMeans(group.results);
        $('body').append(createResults(group.name, group.results));
    }
});
