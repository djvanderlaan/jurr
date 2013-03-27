`jurr` : Jan's useful R routines
================================

A small collection of personal routines that I kept on using in different 
projects. In order to make reuse a little bit easier, I bundled them in a 
package. Below is an overview of most of the routines.


Plotting routines
-----------------

- [`add_alpha`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/plot_var.R): 
  add an alpha channel to a colour
- [`hcl_palette`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/plot_var.R): 
  create color palette for categorical variables using hcl. The example below shows a 
  generated palette using 10 colours:
  ![palette example](http://github.com/djvanderlaan/jurr/raw/master/examples/hcl_palette.png)
- [`histplot`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/histplot.R): 
  plot a histogram   
  ![histplot example](http://github.com/djvanderlaan/jurr/raw/master/examples/histplot.png)
- [`histplot2`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/histplot.R): 
  plot a two dimensional histogram; offers three visualisation methods: area, 
  color and text. The first image below demonstrates the area variant; the 
  second image combines the color and text variants   
  ![histplot2 example](http://github.com/djvanderlaan/jurr/raw/master/examples/histplot2_area.png)
  ![histplot2 example](http://github.com/djvanderlaan/jurr/raw/master/examples/histplot2_colour.png)
- [`linesby`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/plot_var.R): 
  plot a line for each level of a factor  
  ![linesby example](http://github.com/djvanderlaan/jurr/raw/master/examples/linesby.png)


Statistical routines
--------------------

- [`histw`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/histw.R):
  Weighted histogram
- [`histw2`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/histw.R): 
  Weighted two-dimensional histogram
- [`stochround`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/sample.R): 
  Stochastical rounding: a value of 11.2 is rounded with a probability of 20%
  to 12 and with a probability of 80% to 11. 
- [`cumround`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/sample.R): 
  Cumulative rounding: ensures that the sum of the rounded figures differs less
  than one from the unrounded figures.
- [`sampwor`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/sample.R): 
  Sampling without replacement with unequal probabilities
- [`wbstr`](https://github.com/djvanderlaan/jurr/blob/master/jurr/R/sample.R): 
  Weighted bootstrap for finite population


