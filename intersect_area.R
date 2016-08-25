intersect_area <- function(x1, y1, r1, x2, y2, r2){
  rr1 <- r1^2
  rr2 <- r2^2
  d <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

  # Circles do not overlap
  if (d > r1 + r2) return(0)
  
  # Circle2 is completely inside circle1
  if (d <= abs(r2 - r1) && r1 >= r2) return(pi * rr2)

  # Circle1 is completely inside circle2
  if (d <= abs(r2 - r1) && r2 >= r1) return(pi * rr1)

  # Circles partially overlap
  phi = (acos((rr1 + (d * d) - rr2) / (2 * r1 * d))) * 2
  theta = (acos((rr2 + (d * d) - rr1) / (2 * r2 * d))) * 2
  area1 = 0.5 * theta * rr2 - 0.5 * rr2 * sin(theta)
  area2 = 0.5 * phi * rr1 - 0.5 * rr1 * sin(phi)
    
  # Return area of intersection
  return(area1 + area2)
}