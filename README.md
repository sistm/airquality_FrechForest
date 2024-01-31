# airquality_FrechForest

This repository contains the code of the analysis of the
[airquality](https://archive.ics.uci.edu/dataset/360/air+quality) dataset by
[Fréchet Random Forests](https://github.com/Lcapitaine/FrechForest) method
developed in the SISTM team. The observations correspond to daily measurements
from March 2004 to February 2005, on the field in a significantly polluted area,
at road level, within an Italian cityThe observations correspond to daily
measurements from March 2004 to February 2005, on the field in a significantly
polluted area, at road level, within an Italian city.

This analysis consists, for a given day, in predicting Carbone Monoxide (CO)
concentration curves for the second half-day, using concentration curves of
other pollutants, temperature and humidity for the first half-day.

## Code structure

After a data management part, standard Random Forests are applied. These
commands are quite fast and can be re-run in a few seconds.

However, the subsequent code concerns the application of Fréchet Random Forests,
which can take quite long computation times. We commented the long computations
and provide results objects (that where exactly obtained with the commented
lines) in order to be able to get the results directly.

To give an idea, **one** Fréchet Random Forests on airquality data took about 10
minutes in parallel on a computing node made of 32 cores with an Intel Xeon Gold
SKL-6130 2,1 GHz processor.