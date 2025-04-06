
# Version 1.3.0

* Got rid of mutate_ and select_ as these functions are now deprecated.
* Removed most of the horrible base plots from the vignette
* Fixed a bug in plot.IsoFitInactivation. It was giving an error when the data was grouped
* Implemented the one-step Geeraerd model
* Updated the documentation to markdown
* Fixed some broken links in the documentation
* Updated the CITATION to bibentry format

# Version 1.2.3

* Included function to calculate treatment time to reach X log reductions.
* Implemented function to get a table with indexes for the goodness of fit.
* Extended examples in documentation with new functions.
* Improved the DESCRIPTION
* Updated the vignette.

# Version 1.2.2

* Corrected bug in the plotting of dynamic predictions when there were NAs in logN.
* Added the Metselaar model.

# Version 1.2.1

* Corrected a bug in the plotting of dynamic predictions when the
profile was isothermal.

# Version 1.2.0

* Implemented the ggplot2 plotting of isothermal fit objects.
* Added the option to include the temperature profile in the
plot.
* The fitting and prediction functions now also accept logN0, as well
as N0. If logN0 is a fitting parameters, this variable is fitted, rather
than N0 (which is more stable numerically).
* Added the Arrhenius model.

# Version 1.1.5

* Corrected the DOI in the citation file.

# Version 1.1.4

* Added CITATION file with the information of the paper recently published in Food Research International.

# Version 1.1.3

* Added the possibility to pass additional arguments to ode when making predictions.
* Minor corrections and improvements to the vignette.

# Version 1.1.2

* Corrected a bug in Geeraerd's model.

# Version 1.1.1

* Added graphics::legend to NAMESPACE.
* Added stats::coef and stats::vcov to NAMESPACE.

# Version 1.1.0

* The adjustment is, by default, made targetting the logarithmic count for all cases now.
* Added a tolerance to avoid observations at time 0 causing singularities.
* Function `predict_inactivation_MCMC` for the calculation of prediction
intervals using Monte Carlo methods.
* Plots with `ggplot2`.
* MCMC prediction intervals using MCMC.
* `is.` methods for all the objects defined.
* Some minor corrections in function documentation.
* Extended README file.
* Corrected names of the authors in DESCRIPTION.
