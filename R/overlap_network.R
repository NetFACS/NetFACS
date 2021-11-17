#' Plots the overlap of multiple conditions as bipartite network
#'
#' The function takes multiple netfacs objects and plots how different elements
#' connect the conditions, based on the conditional probabilities that the
#' element occurs in the condition and that the condition is seen when the
#' element is present
#'
#' @param netfacs.list list of objects resulting from \code{\link{netfacs}} or
#'   \code{\link{multiple.netfacs}}
#' @param min.prob minimum conditional probability that should be shown in the
#'   graph
#' @param min.count minimum number of times that a combination should occur
#'   before being included in the graph
#' @param significance sets the level of significance that combinations have to
#'   pass before added to the network
#' @param clusters boolean; if TRUE, the cluster_fast_greedy algorithm is used
#'   to detect underlying community structure, based on the occurrence
#'   probability network
#' @param ignore.element string vector, can be used to exclude certain elements
#'   when creating the plots
#' @param specificity for the 'reduced' graph, select only elements that surpass
#'   this context specificity value
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be
#'   surrounded by bubbles; if FALSE, the edges connect the names directly
#'
#' @return Function returns a \code{\link[ggraph]{ggraph}} plot where each
#'   condition is connected to those elements that occur significantly in this
#'   condition, and each element is connected to each condition under which it
#'   occurs significantly more than expected. Creates four graphs: context
#'   specificity, occurrence in that context, a combined graph, and a 'reduced'
#'   graph where edges are only included if they pass the 'specificity' value
#'   set by the user
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 aes
#' @importFrom ggraph scale_edge_alpha
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph create_layout
#' @importFrom ggraph facet_edges
#' @importFrom ggraph ggraph
#' @importFrom ggraph geom_node_label
#' @importFrom ggraph geom_node_text
#' @importFrom ggraph theme_graph
#' @importFrom ggraph circle
#' @importFrom igraph bipartite_mapping
#' @importFrom igraph E
#' @importFrom igraph modularity
#' @importFrom igraph cluster_fast_greedy
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidygraph as_tbl_graph
#' @importFrom tidygraph activate
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces <- multiple.netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   ran.trials = 10,
#'   combination.size = 2
#' )
#'
#' overlap <- overlap.network(emo.faces,
#'   min.prob = 0.01,
#'   min.count = 3,
#'   significance = 0.01,
#'   specificity = 0.5,
#'   ignore.element = "25",
#'   clusters = TRUE,
#'   plot.bubbles = TRUE
#' )
overlap.network <- function(netfacs.list,
                            min.prob = 0,
                            min.count = 5,
                            significance = 0.01,
                            specificity = 0.1,
                            ignore.element = NULL,
                            clusters = FALSE,
                            plot.bubbles = FALSE) {
  
  min.spec <- specificity # so filter() works properly later
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  
  # if the netfacs.list object doesn't have names for the conditions, they are set to numbers
  if (is.null(names(netfacs.list))) {
    names(netfacs.list) <- 1:length(netfacs.list)
  }
  
  # from the different netfacs objects in the list, reduce them all to single elements that meet the criteria specified by the user
  net.data <- 
    netfacs.list %>% 
    lapply(function(x) {
      x$result %>% 
        filter(.data$combination.size == 1,
               .data$pvalue <= significance, # select significance level
               .data$observed.prob > .data$expected.prob, # have to be MORE likely than expected
               .data$count >= min.count, # have to occur at least this many times
               !.data$combination %in% ignore.element, # remove the 'ignore.element' elements
               .data$observed.prob >= min.prob # minimum probability of occurrence
        )
    })
  
  # create a dataframe that connects the condition with the elements
  multi.net <- 
    net.data %>% 
    bind_rows(.id = "condition") %>% 
    select("condition", "combination", "observed.prob", "specificity")
  
  # create two conditional probability objects: one for the probability that the condition is present given the element, and one the opposite
  condition.element <-
    multi.net %>% 
    rename(A = .data$condition,
           B = .data$combination,
           probability = .data$specificity) %>% 
    select("A", "B", "probability", -"observed.prob") %>% 
    mutate(type = "Context Specificity (P[Condition|Element])")
  
  element.condition <-
    multi.net %>% 
    rename(A = .data$combination,
           B = .data$condition,
           probability = .data$observed.prob) %>% 
    select("A", "B", "probability", -"specificity") %>% 
    mutate(type = "Occurrence Probability (P[Element|Condition])")
  
  
  # if clusters should be detected, assign the color to each community
  modularity.net <- NA # has to be set to NA if 'clusters' == FALSE
  if (clusters) {
    # create undirected unweighted network based on the occurrence rate
    g.cluster <-
      element.condition %>% 
      graph_from_data_frame(directed = FALSE, vertices = NULL) %>% 
      as_tbl_graph()
    
    # colors based on clusters
    g.cluster2 <- 
      g.cluster %>% 
      activate(nodes) %>%
      mutate(
        community = cluster_fast_greedy(g.cluster,
                                        weights = E(g.cluster)$probability)$membership,
        color = as.character(.data$community))
    
    # determine modularity
    modularity.net <-
      cluster_fast_greedy(g.cluster, weights = E(g.cluster)$probability) %>% 
      modularity()
  }
  
  
  ########### create the four graphs: occurrence probability alone, specificity alone, both combined, and the reduced graph
  
  ### create basic network object
  g.occurrence <- basic_net(element.condition, .occurrence = TRUE)
  g.specificity <- basic_net(condition.element)
  g.both <- rbind(condition.element, element.condition) %>% basic_net()
  g.reduced <- multi.net %>% filter(specificity > min.spec) %>% basic_net()
  
  if (clusters) {
    set_cluster_colors <- function(g) {
      g %>% 
        activate(nodes) %>%
        select(-"color") %>% 
        # colors if there are clusters
        left_join(g.cluster2 %>% activate(nodes) %>% as_tibble(), by = "name")  
    }
    
    g.occurrence <- set_cluster_colors(g.occurrence)
    g.specificity <- set_cluster_colors(g.specificity)
    g.both <- set_cluster_colors(g.both)
    g.reduced <- set_cluster_colors(g.reduced)
  }
  
  ### set layouts
  # occurrence as parent layout
  all.layout    <- g.occurrence %>% create_layout(layout = "igraph", algorithm = "kk") 
  # take on same layout as first graph
  spec.layout   <- g.specificity %>% create_layout(layout = "igraph", algorithm = "kk")
  spec.layout$x <- all.layout$x[match(spec.layout$name, all.layout$name)]
  spec.layout$y <- all.layout$y[match(spec.layout$name, all.layout$name)]
  # take on same layout as first graph
  both.layout   <- g.both %>% create_layout(layout = "igraph", algorithm = "kk")
  both.layout$x <- all.layout$x[match(both.layout$name, all.layout$name)]
  both.layout$y <- all.layout$y[match(both.layout$name, all.layout$name)]
  # take on same layout as first graph
  red.layout    <- g.reduced %>% create_layout(layout = "igraph", algorithm = "kk")
  red.layout$x  <- all.layout$x[match(red.layout$name, all.layout$name)]
  red.layout$y  <- all.layout$y[match(red.layout$name, all.layout$name)]
  
  
  ### Plots without bubbles
  p.occurrence <- 
    basic_plot(all.layout, 
               .title = "Occurrence Probability P(Element|Condition)")
  
  p.specificity <- 
    basic_plot(spec.layout, 
               .title = "Context Specificity P(Condition|Element)")
  
  p.both <- 
    basic_plot(both.layout,
               .title = NULL) + 
    facet_edges(~type)
  
  p.reduced <- 
    basic_plot_reduced(red.layout,
                       .title = "Edges with high specificity and occurrence")
  
  
  if (plot.bubbles) {
    p.occurrence  <- p.occurrence  %>% add_bubbles()
    p.specificity <- p.specificity %>% add_bubbles()
    p.both        <- p.both        %>% add_bubbles()
    p.reduced     <- p.reduced     %>% add_bubbles()
  }
  
  list(
    specificity = p.specificity,
    occurrence = p.occurrence,
    both = p.both,
    reduced = p.reduced,
    data = multi.net,
    network = g.both,
    modularity = modularity.net
  )
}

# helpers -----------------------------------------------------------------

basic_net <- function(d, .occurrence = FALSE) {
  nodes <- NULL # to avoid R CMD check when calling activate(nodes)
  g <-
    d %>%
    graph_from_data_frame(directed = TRUE, vertices = NULL) %>%
    as_tbl_graph()
  
  g2 <-
    g %>%
    activate(nodes) %>%
    mutate(
      # assign bipartite type as either condition or element
      type = bipartite_mapping(g)$type,
      # color set if there are no clusters
      color = ifelse(.data$type, "lightblue", "salmon"),
      shape = ifelse(.data$type, "bold", "italic"))
  
  if (.occurrence) {
    g2 <-
      g2 %>%
      activate(nodes) %>%
      mutate(across(.data$color, ~ifelse(!.data$type, "lightblue", "salmon")))
  }
  return(g2)
}

basic_plot <- function(g.layout, .title) {
  ggraph(g.layout) +
    geom_node_text(
      aes(color = .data$color,
          label = .data$name,
          size = 50,
          fontface = .data$shape),
      show.legend = FALSE
    ) +
    scale_edge_alpha(guide = "none") +
    theme_graph(base_family = "sans") +
    ggtitle(.title) +
    geom_edge_fan(
      aes(label = round(.data$probability, 2),
          colour = .data$type),
      label_size = 4,
      arrow = NULL,
      colour = "grey",
      fontface = 'bold',
      label_dodge = unit(2, "mm"),
      angle_calc = "along",
      show.legend = FALSE
    )
}

basic_plot_reduced <- function(g.layout, .title) {
  ggraph(g.layout) +
    geom_node_text(
      aes(color = .data$color,
          label = .data$name,
          size = 50,
          fontface = .data$shape),
      show.legend = FALSE
    ) +
    scale_edge_alpha(guide = "none") +
    theme_graph(base_family = "sans") +
    ggtitle(.title) +
    geom_edge_fan(
      arrow = NULL,
      end_cap = circle(4, "mm"),
      start_cap = circle(4, "mm"),
      colour = "grey",
      fontface = 'bold',
      label_dodge = unit(2, "mm"),
      angle_calc = "along", 
      show.legend = FALSE
    )
}

add_bubbles <- function(p) {
  p +
    geom_node_label(
      aes(label = .data$name,
          color = .data$color,
          size = 50,
          fontface = .data$shape),
      show.legend = FALSE
    )
}
