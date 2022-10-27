# simMetric 0.1.1

* avoid unnecessary use of base R pipe (`|>`) and associated dependency on R>=4.1.0.
* avoid unnecessary dependency on `{purrr}`.
* add citation for entry on QUT research data finder (<https://researchdatafinder.qut.edu.au/display/n43368>)

# simMetric 0.1.0

First submission to CRAN. `simMetric` can be used to:


* obtain simulation study evaluation metrics within existing packages for simulation study frameworks (e.g. `{simTool}`)
* obtain many metrics and their uncertainty at once with groupings on one or more selected variables.
* obtain metrics without the provided helper function, allowing users to add metrics within their usual tidy workflow (e.g. within `dplyr::summarise()`)
* studies that compare multiple methods or data generating inputs by using these multiple indexes for grouping and estimating metrics.

Metrics included are taken from a recent, and widely read [tutorial](https://doi.org/10.1002/sim.8086) on simulation study conduct and reporting.
