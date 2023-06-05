# ALFAM2
Model for ammonia volatilization (loss) from field-applied manure.

# Model description
The following paper describes the model in some detail:

Hafner, S.D., Pacholski, A., Bittman, S., Carozzi, M., Chantigny, M., Génermont, S., Häni, C., Hansen, M.N., Huijsmans, J., Kupper, T., Misselbrook, T., Neftel, A., Nyord, T., Sommer, S.G., 2019. A flexible semi-empirical model for estimating ammonia volatilization from field-applied slurry. Atmospheric Environment 199: 474-484. <https://doi.org/10.1016/j.atmosenv.2018.11.034>

Download a copy [here](https://drive.google.com/file/d/1UEzmjApe2kMs4CyX6dUQIqn0ZOZ8elV9/view?usp=sharing).

As shown in the figure below, the ALFAM2 model tracks applied ammonia (as "total ammoniacal nitrogen", TAN) after field application.
The numerical value of the "primary" parameters shown below (r and f terms) determine the rate and trajectory of emission.
The value of these parameters are determined by the value of predictor variables and the "secondary" parameter set.

![schematic](schematic.png)

# Installation
Installation of packages from GitHub requires a package called devtools.
You can run the code below to install devtools and ALFAM2.

If you don't already have it, first install devtools from CRAN.

```
install.packages("devtools")
```

Then install the latest ALFAM2 release with the following command.

```
devtools::install_github("sashahafner/ALFAM2@*release", build_vignettes = TRUE)
```

You can get other releases with this syntax (e.g., `sashahafner/ALFAM2@v3.0`), just pull from the master branch (`sashahafner/ALFAM2`), or other branches using the `ref` argument.

# Package use
Once the package is installed, load it.

```
library(ALFAM2)
```

And see a vignette to get started.

```
vignette("ALFAM2-start")
```

# Bugs and requests
Please use the Issues page.

# Project information
See www.alfam.dk for more information and resources.
