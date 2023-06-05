# ALFAM2
Model for ammonia volatilization (loss) from field-applied manure.

# Model schematic
![schematic](schematic.png)

# Model description
See the following paper for more details on the model.

Hafner, S.D., Pacholski, A., Bittman, S., Carozzi, M., Chantigny, M., Génermont, S., Häni, C., Hansen, M.N., Huijsmans, J., Kupper, T., Misselbrook, T., Neftel, A., Nyord, T., Sommer, S.G., 2019. A flexible semi-empirical model for estimating ammonia volatilization from field-applied slurry. Atmospheric Environment 199: 474-484. <https://doi.org/10.1016/j.atmosenv.2018.11.034>

Download a copy [here](https://drive.google.com/file/d/1UEzmjApe2kMs4CyX6dUQIqn0ZOZ8elV9/view?usp=sharing).


# Installation
Installation of packages from GitHub requires a package called devtools.
You can run the code below to install devtools and ALFAM2.

First, install devtools from CRAN.

```
install.packages("devtools")
```

Then install ALFAM2.

```
devtools::install_github("sashahafner/ALFAM2", build_vignettes = TRUE)
```

# Latest version
To update ALFAM2 to the latest version, run the following two commands in R.
This assumes that devtools is installed and you want the *latest* version of ALFAM2.

```
remove.packages("ALFAM2")
devtools::install_github("sashahafner/ALFAM2", ref = "dev", build_vignettes = TRUE)
```

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
