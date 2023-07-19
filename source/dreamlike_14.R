seeds <- 2200:2299

pollinate <- function(seed) {
  
  library(Rcpp)
  library(dplyr)
  library(cairobasic)
  
  sys_id <- "14"
  sys_name <- "dreamlike"
  sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))
  
  # seed
  cat(seed, "\n")
  set.seed(seed)
  
  # fixed / default
  px <- 3000
  layers <- 5
  million <- 10^6
  iter <- 1000 * million
  #zoom <- .4
  alpha <- .01
  
  
  # palette specification ---------------------------------------------------
  
  ncl <- 1024
  pal <- sample(colorir::colores$colour, 6)
  bg <- pal[1] 
  pal <- (colorRampPalette(pal))(ncl)
  

  # choose shape ------------------------------------------------------------
  n_sides <- sample(c(4:8, 100), 1)
  rot_off <- runif(1, 0, 120)
  zoom <- runif(1, min = .35, max = .45)

  
  # helper functions --------------------------------------------------------
  
  generate_data <- function(seed, iter, layers, px, zoom, alpha, n_sides, rot_off) {
    set.seed(seed)
    df <- raster_data(iter, layers, px, zoom, alpha, n_sides, rot_off)
    return(df)
  }
  
  transform_data <- function(df) {
    df <- rank(abs(df))
    df <- df - min(df)
    df <- df / max(df)
    df <- as.integer(df * (ncl - 1)) + 1
    return(df)
  }
  
  colourise_data <- function(df) {
    df <- pal[df]
    df <- matrix(df, px, px, byrow = TRUE)
    return(df)
  }
  
  render_data <- function(df, fpath, px, bg) {
    rs <- as.raster(df)
    jpeg(
      filename = fpath,
      width = px,
      height = px,
      bg = bg 
    )
    op <- par(mar = c(0,0,0,0))
    plot(rs)
    dev.off()
    par(op)
  }
  
  fpath <- function(seed) {
    dir <- paste0("sys_", sys_id)
    dir <- here::here("output", dir)
    if(!dir.exists(dir)) dir.create(dir)
    prefix <- paste0(sys_name, "_", sys_id, "_")
    fname <- paste0(prefix, seed, ".jpg")
    fp <- file.path(dir, fname)
    return(fp)
  }
  
  # generate the data -------------------------------------------------------
  
  cat("generating...\n")
  
  
  df1 <- generate_data(seed, iter, layers, px, zoom, alpha, n_sides, rot_off)
  
  cat("transforming...\n")
  
  rank1 <- transform_data(df1)
  cols1 <- colourise_data(rank1)
  
  cat("rendering...\n")
  
  render_data(cols1, fpath(seed), px, bg)
  
}

for(s in seeds) pollinate(s)