#' functions by Giulio Dalla Riva
#' email: gvdr@zoology.ubc.ca
trajectories_bm <- function(tree,sigma){
  bm_path <- function(n_steps, sigma){
    path <- c(0, cumsum(rnorm(n = n_steps, sd = sigma)))
    return(path)
  }
  
  tree <- reorder(tree)
  
  Trajectories <- lapply(tree$edge.length, function(x) bm_path(x,sigma))
  
  
  final_state <- tree$edge.length + 1
  for (i in 1:nrow(tree$edge)) {
    end_node <- which(tree$edge[, 2] == tree$edge[i, 1])
    if (length(pp) > 0) {
      Trajectories[[i]] <- Trajectories[[i]] + Trajectories[[end_node]][final_state[end_node]]
    } else {
      Trajectories[[i]] <- Trajectories[[i]] + Trajectories[[1]][1]
    }
  }
  
  H <- nodeHeights(tree)
  get_df <- function(i){
    data.frame(
      times = seq(H[i,1],H[i,2]),
      from = tree$edge[i,1],
      to = tree$edge[i,2],
      coord = Trajectories[[i]]
    )
  }
  
  df_traj <- plyr::ldply(seq_along(Trajectories),get_df)
  return(df_traj)
}

radius_bm <- function(tree,sigma,min_rad,trend){
  bm_path <- function(n_steps, sigma){
    path <- c(0, cumsum(rnorm(mean=trend,n = n_steps, sd = sigma)))
    return(path)
  }
  
  Trajectories <- lapply(tree$edge.length, function(x) bm_path(x,sigma))
  tree <- reorder(tree)
  
  final_state <- tree$edge.length + 1
  for (i in 1:nrow(tree$edge)) {
    end_node <- which(tree$edge[, 2] == tree$edge[i, 1])
    if (length(pp) > 0) {
      Trajectories[[i]] <- Trajectories[[i]] + Trajectories[[end_node]][final_state[end_node]]
    } else {
      Trajectories[[i]] <- Trajectories[[i]] + Trajectories[[1]][1]
    }
  }
  H <- nodeHeights(tree)
  get_df <- function(i){
    data.frame(
      times = seq(H[i,1],H[i,2]),
      from = tree$edge[i,1],
      to = tree$edge[i,2],
      radius = Trajectories[[i]]
    )
  }
  
  df_traj <- plyr::ldply(seq_along(Trajectories),get_df)
  df_traj$radius <- abs(df_traj$radius) + min_rad
  return(df_traj)
}