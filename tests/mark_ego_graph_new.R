#' Function earmark the companies in the neighbourhood of a company, this neighbourhood is sometimes
#' called [ego graphs](http://mathworld.wolfram.com/NeighborhoodGraph.html)
#'
#' @param graph A graph containing company/company relations data.
#' @param id_company A company id of which you want to mark the neighbourhood
#' @param target_attribute The name of the new attribute you want to mark the neighborhood in
#' @param distance The number of 'hops' in the company network that should be included, default = 1
#' @param direction The direction to calculate the propagated value from (can be "in", "out", "all")
#' @return A graph containing the newly added neighbourhood marking attribute
#' @keywords graph company hierarchy ego graph
#' @export
#' @examples
#' graph_all_companies <- mark_ego_graph(graph_all_companies, id_company = "910716048", target_attribute = "is_neighbour")
mark_ego_graph <- function(graph, id_company, target_attribute, distance = 1, direction = "in"){

  if(!direction %in% c("in", "out", "all")) { stop("Incorrect direction value") }
  if(is.infinite(distance) & distance > 0) { distance = 999 }

  vertx_neighbors <- character()

  if(direction %in% c("in")){
    vertx_neighbors <- c(vertx_neighbors,
                         igraph::ego(graph,
                                     order = distance,
                                     mode = "in",
                                     nodes = igraph::V(graph)[id_company])[[1]]$name)
  }

  if(direction %in% c("out")) {
    vertx_neighbors <- c(vertx_neighbors,
                         igraph::ego(graph,
                                     order = distance,
                                     mode = "out",
                                     nodes = igraph::V(graph)[id_company])[[1]]$name)
  }

  if(direction %in% c("all")) {

    vertx_neighbors <- c(vertx_neighbors,
                         igraph::ego(graph,
                                     order = distance,
                                     mode = "all",
                                     nodes = igraph::V(graph)[id_company])[[1]]$name)
  }
  igraph::vertex_attr(graph, target_attribute) <- igraph::V(graph)$name %in% vertx_neighbors

  return(graph)
}
