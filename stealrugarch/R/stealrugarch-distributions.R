# ------------------------------------------------------------------------------
# Distribution Bounds
# ------------------------------------------------------------------------------
.DistributionBounds = function(distribution)
{
  ghlambda = 0
  ghlambda.LB = 0
  ghlambda.UB = 0
  if (distribution == "norm"){
    skew 	= 0
    skew.LB = 0
    skew.UB = 0
    shape 	= 0
    shape.LB = 0
    shape.UB = 0}
  if (distribution == "ged"){
    skew 	= 0
    skew.LB = 0
    skew.UB = 10
    shape 	= 2
    shape.LB = 0.1
    shape.UB = 50}
  if (distribution == "std"){
    skew 	= 0
    skew.LB = 0
    skew.UB = 0
    shape 	= 4
    shape.LB = 2.1
    shape.UB = 100}
  if (distribution == "snorm"){
    skew 	= 0.9
    skew.LB	= 0.1
    skew.UB	= 10
    shape 	= 0
    shape.LB = 0
    shape.UB = 0}
  if (distribution == "sged"){
    skew 	= 1
    skew.LB	= 0.01
    skew.UB	= 30
    shape 	= 2
    shape.LB = 0.1
    shape.UB = 60}
  if (distribution == "sstd"){
    skew 	= 1
    skew.LB = 0.01
    skew.UB = 30
    shape 	= 4
    shape.LB = 2.01
    shape.UB = 60}
  if (distribution == "nig"){
    skew 	= 0.2
    skew.LB = -0.99
    skew.UB	= 0.99
    shape 	= 0.4
    shape.LB = 0.01
    shape.UB = 25
  }
  if(distribution == "ghyp"){
    skew 	= 0.2
    skew.LB = -0.99
    skew.UB	= 0.99
    shape 	= 2
    shape.LB = 0.25
    shape.UB = 25
    ghlambda = -0.5
    ghlambda.LB = -6
    ghlambda.UB = 6
  }
  if(distribution == "jsu"){
    skew 	= 0
    skew.LB	= -20
    skew.UB	= 20
    shape 	= 1
    shape.LB = 0.1
    shape.UB = 10
  }
  if(distribution == "ghst"){
    skew 	= 0
    skew.LB	= -80
    skew.UB	= 80
    shape 	= 8.2
    shape.LB = 4.1
    shape.UB = 25
  }
  # johnson has 2 shape parameters. The second one we model with the "skew"
  # representation in rugarch
  skewed.dists = c("snorm", "sged", "sstd", "nig", "ghyp", "jsu", "ghst")
  shaped.dists = c("ged", "sged", "std", "sstd", "nig", "ghyp", "jsu", "ghst")
  skew0  = 0
  shape0 = 0
  if(any(skewed.dists == distribution)) include.skew=TRUE else include.skew=FALSE
  if(any(shaped.dists == distribution)) include.shape=TRUE else include.shape=FALSE
  if(distribution == "ghyp") include.ghlambda = TRUE else include.ghlambda = FALSE
  return( list(shape = shape, shape.LB = shape.LB, shape.UB = shape.UB, skew = skew,
               skew.LB = skew.LB, skew.UB = skew.UB, include.skew = include.skew,
               include.shape = include.shape, skew0 = skew0, shape0 = shape0,
               include.ghlambda = include.ghlambda, ghlambda = ghlambda,
               ghlambda.LB = ghlambda.LB, ghlambda.UB = ghlambda.UB) )
}
