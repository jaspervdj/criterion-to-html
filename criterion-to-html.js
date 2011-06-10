/* Create an element showing a group of results */
function createResults(group) {
    /* If attributes are not set, we should set them */
    group.sort = group.sort ? true : false;

    var div = $(document.createElement('div')).addClass('results');

    var controls = $(document.createElement('div')).addClass('controls');
    div.append(controls);

    div.append($(document.createElement('h2')).html(group.name));

    /* Create a box and label to sort the results */
    var sortBox = $(document.createElement('input'))
        .attr('type', 'checkbox')
        .attr('id', 'sort-' + sanitizeName(group.name))
        .attr('checked', group.sort);
    var sortLabel = $(document.createElement('label'))
        .attr('for', 'sort-' + sanitizeName(group.name))
        .html('Sort results');
    controls.append(sortLabel);
    controls.append(sortBox);

    /* When the sort box is changed, we need to reset this div */
    sortBox.click(function() {
        group.sort = sortBox.attr('checked');
        div.replaceWith(createResults(group));
    });

    /* Create an array copy in order to sort them */
    var results = group.results.slice(0);
    prepareResults(results, group.sort);
    for(i in results) div.append(createResult(results[i]));

    return div;
}

/* Sanitize a name to something we can use as ID */
function sanitizeName(name) {
    return name.toLowerCase().replace(/[^a-z0-9]/, '-');
}

/* Create an element showing a given result */
function createResult(result) {
    var div = $(document.createElement('div'));
    var canvas = document.createElement('canvas');
    $(canvas).attr('width', '600px').attr('height', '30px');
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

/* Prepare results */
function prepareResults(results, sort) {
    /* Sort if necessary */
    if(sort) {
        results.sort(function(r1, r2) { return r1.mean - r2.mean; });
    }

    var max = 0;
    for(i in results) if(results[i].mean > max) max = results[i].mean;
    for(i in results) results[i].normalizedMean = results[i].mean / max;
}

/* Main handler, create the page */
$(function() {
    for(i in criterionResults) {
        var group = criterionResults[i];
        $('body').append(createResults(group));
    }
});
