# NetFACS 0.3.1.9003

## Breaking changes
- netfacs.extract() function argument "levels" changed to "combination.size" to be more consistent with arguments in other functions in the package. Default values of function arguments have also changed to become less strict.

## Other changes
- Objects created using the netfacs() function are now of class "netfacs"
- Print method added for objects of class "netfacs"

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
