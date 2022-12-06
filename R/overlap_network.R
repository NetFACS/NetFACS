#' Plots the overlap of multiple conditions as bipartite network
#'
#' The function takes multiple netfacs objects and plots how different elements
#' connect the conditions, based on the conditional probabilities that the
#' element occurs in the condition and that the condition is seen when the
#' element is present
#'
#' @param x list of objects resulting from \code{\link{specificity}} or
#'   \code{\link{netfacs}}
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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' data(emotions_set)
#' emo.faces <- netfacs_multiple(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   ran.trials = 10,
#'   combination.size = 2
#' )
#' # calculate element specificity
#' spec <- specificity(emo.faces)
#' 
#' overlap <- overlap_network(spec,
#'                            min.prob = 0.01,
#'                            min.count = 3,
#'                            significance = 0.01,
#'                            specificity = 0.5,
#'                            ignore.element = "25",
#'                            clusters = TRUE,
#'                            plot.bubbles = TRUE)
#' }
overlap_network <- function(
    x,
    min.prob = 0,
    min.count = 5,
    significance = 0.01,
    specificity = 0.1,
    ignore.element = NULL,
    clusters = FALSE,
    plot.bubbles = TRUE
) {
  UseMethod("overlap_network")
}

#' @export
overlap_network.netfacs_specificity <- function(
    x,
    min.prob = 0,
    min.count = 5,
    significance = 0.01,
    specificity = 0.1,
    ignore.element = NULL,
    clusters = FALSE,
    plot.bubbles = FALSE
) {
  
  d <- 
    x %>% 
    dplyr::filter(
      .data$combination.size == 1,
      .data$observed.prob >= min.prob,
      .data$count >= min.count,
      .data$pvalue <= significance,
      .data$observed.prob > .data$expected.prob,
      !.data$combination %in% ignore.element
    ) %>% 
    select("condition", "combination", "observed.prob", "specificity")
  
  all_plots(d, specificity, clusters, plot.bubbles)
  }

#' @export
overlap_network.netfacs <- function(
    x,
    min.prob = 0,
    min.count = 5,
    significance = 0.01,
    specificity = 0.1,
    ignore.element = NULL,
    clusters = FALSE,
    plot.bubbles = FALSE
) {
  
  rlang::inform("Specificity calculated by upsampling minority conditions. Use specificity(x, .upsample = FALSE) to change.")
  
  sp <- specificity(x)
  
  overlap_network.netfacs_specificity(sp,
                                      min.prob,
                                      min.count,
                                      significance,
                                      specificity,
                                      ignore.element,
                                      clusters,
                                      plot.bubbles)
}

#' (Deprecated) Plots the overlap of multiple conditions as bipartite network.
#'
#' This function is deprecated. Please see \code{\link{overlap_network}}
#' instead
#'
#' @param netfacs.list list of objects resulting from \code{\link{netfacs}} or
#'   \code{\link{netfacs_multiple}}
#' @inheritParams overlap_network
#' @export
overlap.network <- function(netfacs.list,
                            min.prob = 0,
                            min.count = 5,
                            significance = 0.01,
                            specificity = 0.1,
                            ignore.element = NULL,
                            clusters = FALSE,
                            plot.bubbles = FALSE) {
  
  .Deprecated("overlap_network")
  
  overlap_network.netfacs(netfacs.list,
                          min.prob,
                          min.count,
                          significance,
                          specificity,
                          ignore.element,
                          clusters,
                          plot.bubbles)
  
}


# helpers -----------------------------------------------------------------

all_plots <- function(d,
                      specificity,
                      clusters,
                      plot.bubbles) {
  min.spec <- specificity # so left_join() works properly later
  nodes <- NULL # to avoid R CMD check when calling tidygraph::activate(nodes)
  
  # create two conditional probability objects: one for the probability that the condition is present given the element, and one the opposite
  condition.element <-
    d %>% 
    dplyr::rename(A = "condition",
                  B = "combination",
                  probability = "specificity") %>% 
    dplyr::select("A", "B", "probability", -"observed.prob") %>% 
    dplyr::mutate(type = "Context Specificity (P[Condition|Element])")
  
  element.condition <-
    d %>% 
    dplyr::rename(A = "combination",
                  B = "condition",
                  probability = "observed.prob") %>% 
    dplyr::select("A", "B", "probability", -"specificity") %>% 
    dplyr::mutate(type = "Occurrence Probability (P[Element|Condition])")
  
  
  # if clusters should be detected, assign the color to each community
  modularity.net <- NA # has to be set to NA if 'clusters' == FALSE
  if (clusters) {
    # create undirected unweighted network based on the occurrence rate
    g.cluster <-
      element.condition %>% 
      igraph::graph_from_data_frame(directed = FALSE, vertices = NULL) %>% 
      tidygraph::as_tbl_graph()
    
    # colors based on clusters
    g.cluster2 <- 
      g.cluster %>% 
      tidygraph::activate(nodes) %>%
      dplyr::mutate(
        community = igraph::cluster_fast_greedy(
          g.cluster,
          weights = igraph::E(g.cluster)$probability
        )$membership,
        color = as.character(.data$community)
      )
    
    # determine modularity
    modularity.net <-
      igraph::cluster_fast_greedy(
        g.cluster, 
        weights = igraph::E(g.cluster)$probability
      ) %>% 
      igraph::modularity()
  }
  
  
  ########### create the four graphs: occurrence probability alone, specificity alone, both combined, and the reduced graph
  
  ### create basic network object
  g.occurrence <- basic_net(element.condition, .occurrence = TRUE)
  g.specificity <- basic_net(condition.element)
  g.both <- rbind(condition.element, element.condition) %>% basic_net()
  g.reduced <- 
    d %>% dplyr::filter(.data$specificity > min.spec) %>% basic_net()
  
  if (clusters) {
    set_cluster_colors <- function(g) {
      g %>% 
        tidygraph::activate(nodes) %>%
        dplyr::select(-"color") %>% 
        # colors if there are clusters
        dplyr::left_join(
          g.cluster2 %>% tidygraph::activate(nodes) %>% tibble::as_tibble(), 
          by = "name"
        )  
    }
    
    g.occurrence <- set_cluster_colors(g.occurrence)
    g.specificity <- set_cluster_colors(g.specificity)
    g.both <- set_cluster_colors(g.both)
    g.reduced <- set_cluster_colors(g.reduced)
  }
  
  ### set layouts
  # occurrence as parent layout
  all.layout    <- 
    g.occurrence %>% ggraph::create_layout(layout = "igraph", algorithm = "kk") 
  # take on same layout as first graph
  spec.layout   <- 
    g.specificity %>% ggraph::create_layout(layout = "igraph", algorithm = "kk")
  spec.layout$x <- all.layout$x[match(spec.layout$name, all.layout$name)]
  spec.layout$y <- all.layout$y[match(spec.layout$name, all.layout$name)]
  # take on same layout as first graph
  both.layout   <- 
    g.both %>% ggraph::create_layout(layout = "igraph", algorithm = "kk")
  both.layout$x <- all.layout$x[match(both.layout$name, all.layout$name)]
  both.layout$y <- all.layout$y[match(both.layout$name, all.layout$name)]
  # take on same layout as first graph
  red.layout    <- 
    g.reduced %>% ggraph::create_layout(layout = "igraph", algorithm = "kk")
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
    ggraph::facet_edges(~type)
  
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
    data = d,
    network = g.both,
    modularity = modularity.net
  )
}

basic_net <- function(d, .occurrence = FALSE) {
  nodes <- NULL # to avoid R CMD check when calling tidygraph::activate(nodes)
  g <-
    d %>%
    igraph::graph_from_data_frame(directed = TRUE, vertices = NULL) %>%
    tidygraph::as_tbl_graph()
  
  g2 <-
    g %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(
      # assign bipartite type as either condition or element
      type = igraph::bipartite_mapping(g)$type,
      # color set if there are no clusters
      color = ifelse(.data$type, "lightblue", "salmon"),
      shape = ifelse(.data$type, "bold", "italic")
    )
  
  if (.occurrence) {
    g2 <-
      g2 %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(dplyr::across(
        "color", ~ifelse(!.data$type, "lightblue", "salmon")
      ))
  }
  return(g2)
}

basic_plot <- function(g.layout, .title) {
  ggraph::ggraph(g.layout) +
    ggraph::geom_node_text(
      ggplot2::aes(color = .data$color,
                   label = .data$name,
                   size = 50,
                   fontface = .data$shape),
      show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha(guide = "none") +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::ggtitle(.title) +
    ggraph::geom_edge_fan(
      ggplot2::aes(label = round(.data$probability, 2),
                   colour = .data$type),
      label_size = 4,
      arrow = NULL,
      colour = "grey",
      fontface = 'bold',
      label_dodge = grid::unit(2, "mm"),
      angle_calc = "along",
      show.legend = FALSE
    )
}

basic_plot_reduced <- function(g.layout, .title) {
  ggraph::ggraph(g.layout) +
    ggraph::geom_node_text(
      ggplot2::aes(color = .data$color,
                   label = .data$name,
                   size = 50,
                   fontface = .data$shape),
      show.legend = FALSE
    ) +
    ggraph::scale_edge_alpha(guide = "none") +
    ggraph::theme_graph(base_family = "sans") +
    ggplot2::ggtitle(.title) +
    ggraph::geom_edge_fan(
      arrow = NULL,
      end_cap = ggraph::circle(4, "mm"),
      start_cap = ggraph::circle(4, "mm"),
      colour = "grey",
      fontface = 'bold',
      label_dodge = grid::unit(2, "mm"),
      angle_calc = "along", 
      show.legend = FALSE
    )
}

add_bubbles <- function(p) {
  p +
    ggraph::geom_node_label(
      ggplot2::aes(label = .data$name,
                   color = .data$color,
                   size = 50,
                   fontface = .data$shape),
      show.legend = FALSE
    )
}
