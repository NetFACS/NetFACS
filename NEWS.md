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
