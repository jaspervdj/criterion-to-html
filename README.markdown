criterion-to-html
=================

What
----

This is a small program to convert [Criterion][criterion] to HTML reports.

[criterion]: http://hackage.haskell.org/package/criterion

At this point, the HTML reports are (very) minimal, they simply provide a simple
barplot overview. If there's anything you'd like me to add, please create an
[issue][issue].

[issue]: http://github.com/jaspervdj/criterion-to-html/issues

Usage
-----

To generate the CSV file, run your criterion benchmark using the @-u@ flag:

    some-criterion-benchmark -u results.csv

You can then convert the CSV results to some simple HTML plots using this
program:

    criterion-to-html results.csv
