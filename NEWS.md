# NetFACS 0.5.1

- Removed netfacs_tutorial vignette since the information is duplicated in the README file
- Minor fixes to comply with R CMD build check notes

# NetFACS 0.5.0

## Breaking changes
- netfacs() function no longer calculates specificity by default. Specificity must now be calculated separately using the specificity() function. This change gives the user more flexibility in calculating specificity based on the raw data or by pre-upsampling minority conditions.
- element.specificity() is replaced by specificity_increase()

## Other changes

- New functions: specificity(), upsample()
- Objects created using the specificity() now have class "netfacs_specificity"

# NetFACS 0.4.0

## Depricated functions and arguments
- The following functions have been deprecated and replaced to avoid clashes when using method calls:
  - multiple.netfacs() is replaced by netfacs_multiple()
  - netfacs.extract() is replaced by netfacs_extract()
- netfacs.extract() function argument "levels" is also deprecated. Instead a "combination.size" argument has been added to be more consistent with the arguments in other functions of this package. Default values of function arguments have changed to return all results, without filtering.

## Other changes
- Objects created using the netfacs() function are now of class "netfacs"
- Objects created using the netfacs_multiple() function are now of class "netfacs_multiple"
- Print methods added for objects of class "netfacs" and "netfacs_multiple"


# NetFACS 0.3.1
- Fix so that the order of elements in random probabilities matches the order of elements in the results of the netfacs() function 

# NetFACS 0.3.0

- Networks created by the NetFACS package are now of class tbl_graph as well as igraph
- netfacs() function now gives more informative error messages
- Now imports dplyr, ggraph, magrittr, tibble, tidygraph and tidyr
- Added more unit tests

# NetFACS 0.2.0

- Increase size of nodes and labels in plots
- Initialize unit tests
- Fix functions exported when running parallel netfacs() on Windows  

# NetFACS 0.1.0

Initial release.
