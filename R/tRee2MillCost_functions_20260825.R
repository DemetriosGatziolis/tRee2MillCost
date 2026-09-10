#' @title Examine parameters and data
#'
#' @description
#' Examines data and parameter values with settings and expectations of this
#' package. The intended workflow relies on a vector representation of a road
#' network (\emph{rdName}) and two sets of point layers, the first denoting
#' travel destination or end locations (\emph{toName}) and the second travel
#' origin or start locations (\emph{fromName}). \emph{toName} and
#' \emph{fromName} must contain a field or data attribute column named
#' \emph{toField} and \emph{fromField} respectively. \emph{rdName} must contain
#' a field name \emph{rdField} only if \emph{costType} is set to 1 (travel cost
#' expressed in time)
#'
#' @param rdName character, filename/path of a line vector layer representing
#' the road network
#' @param rdField character, name of column expected to be present in
#' \emph{rdName} (only if \emph{costType} = 1) representing the expected travel
#' speed of each road segment
#' @param toName character, filename/path of a point vector layer representing
#' travel destinations
#' @param toField charcter, name of column expected to be present in
#' \emph{toName} as travel destination identifier
#' @param fromName character, filename/path of a point vector layer representing
#' travel origin or start location identifier
#' @param fromField character, name of column expected to be present in
#' \emph{fromName} as travel origin or start location identifier
#' @param rasterResolution numeric, the resolution of the raster that will
#' represent the road network in the length unit of the projection used
#' @param roadRasterName character filename/path of the road raster. TIFF format
#' and .tif extension is expected
#' @param movedToName character, filename/path of a layer representing locations
#' where travel destinations in \emph{toName} will be moved to be on the
#' network. Geopackage format is expected
#' @param movedFromName character, filename/path of a layer representing
#' locations where travel origins in \emph{fromName} will be moved to be on the
#' network. Geopackage format is expected
#' @param costType integer, 1 for travel calculations based on time, 2 for
#' travel calculations based on distance
#' @param costCSVName character, filename/path of the travel cost matrix between
#' travel origins and destinations. .csv extension is expected
#'
#' @details The values in \emph{toField} and \emph{fromField} must be unique. If
#' \emph{rdField} is required (\emph{costType} = 1), its values should be
#' positive and expressed in miles per hour, even if the linear unit of the
#' projection shared by \emph{rdName}, \emph{toName}, and \emph{fromName} is in
#' feet. The resolution of the raster representation of the road network should
#' not be excessively fine (<5m or ~15ft) nor too coarse (>50m or ~164ft). Fine
#' resolutions do not improve the information content of the raster network
#' representation and often lead to network renditions with unwarrantedly large
#' number of raster cells or graph nodes likely to exhaust hardware resources of
#' computing platforms and result in prolonged processing time. Coarse
#' resolutions tend to connect road network segments that are proximal but not
#' actually connected, thereby yielding an underestimation of travel time. An
#' example of the latter would be two roads along opposite sides of a river
#' without a bridge between them
#'
#' \emph{rdField}, \emph{toField}, and \emph{fromField} are case sensitive
#'
#' @examples
#' \dontrun{
#' rdName    <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' fromName  <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#' fromField <- "sampleID"
#' toName    <- system.file( "extdata", "mills.gpkg", package="tRee2MillCost" )
#' toField   <- "COMPANY_NAME"
#' rasterResolution <- 20 # implies meters, the linear unit of the projection
#'                        # in rdName, fromName, and toName
#' roadRasterName = "road_net.tif"
#' movedToName   <- "moved_mills.gpkg"
#' movedFromName <- "moved_roads.gpkg"
#' costType      <- 1 # cost as travel time
#' costCSVName   <- "cost_matrix.csv"
#'
#' checkInputs( rdName, rdField,
#'              toName, toField,
#'              fromName, fromField,
#'              rasterResolution,
#'              roadRasterName,
#'              movedToName, movedFromName,
#'              costType, costCSVName )
#' }
#'
#' @export
checkInputs <- function( rdName, rdField,
                         toName, toField,
                         fromName, fromField,
                         rasterResolution, roadRasterName,
                         movedToName, movedFromName,
                         costType,
                         costCSVName ) {

  ## input spatial info
  s      <- c( rdName, toName, fromName )
  sAtt   <- list( c("LINESTRING", "MULTILINESTRING"), "POINT", "POINT" )
  sNames <- c( rdField, toField, fromField )
  for( j in s ) {
    if( !file.exists(j) )
      warning( "\n", j, " does not exist\n",
               immediate.=TRUE, call.=FALSE )
  }

  ## check if spatial inputs have .shp or .gpkg extension
  for( j in 1:length(s) ) {
    if( toupper(tools::file_ext(s[j])) != "SHP" & toupper(tools::file_ext(s[j])) != "GPKG" )
      warning( "\n", s[j], " is not a shapefile nor a geopackage\n",
               immediate.=TRUE, call.=FALSE )
  }

  ## input fields and projection
  compareList <- list()
  for( j in 1:length(s) ) {
    # if( toupper(tools::file_ext(s[j])) == ".SHP" )
    #   layerName <- gsub( ".SHP", "", toupper(basename(s[j])) )
    # if( toupper(substring(s[j], nchar(s[j])-4, nchar(s[j]))) == ".GPKG" )
    #   layerName <- gsub( ".GPKG", "", toupper(basename(s[j])) )
    layerName = sf::st_layers( s[j] )[1]
    shpInfo <- sf::st_read( s[j],
                            query=sprintf("SELECT * FROM %s LIMIT 1", layerName ),
                            quiet=TRUE )
    if( ( sf::st_geometry_type(shpInfo) %in% sAtt[[j]] ) == FALSE ) {
      warning( "\n", s[j], " should be of ", paste0(sAtt[[j]], sep= " "),
               "type but it is ", as.character(sf::st_geometry_type(shpInfo)), " \n",
               immediate.=TRUE, call.=FALSE )
    }
    if( ( sNames[j] %in% names(shpInfo) == FALSE ) ) {
      warning( "\nAttribute ", sNames[j], " does not exist in ", s[j], ". The attribute is case sensitive\n",
               immediate.=TRUE, call.=FALSE )
    }
    compareList[[j]] <- shpInfo
  }
  ## does projection support terra::linearUnits()?
  unit <- terra::linearUnits( terra::vect(compareList[[1]]) )
  if( (abs(unit - 0.3048) > 0.0001) & (abs(unit - 1) > 0.0001) )
    stop( "\nUnit for ", rdName, " should be either meters or feet\n" )

  if( (sf::st_crs(compareList[[1]])$input != sf::st_crs(compareList[[1]])$input) |
      (sf::st_crs(compareList[[1]])$units != sf::st_crs(compareList[[1]])$units)    ) ## minimal test
    warning( "\n", s[1], " and ", s[2], " do not share the same projection\n", immediate.=TRUE, call.=FALSE )
  if( (sf::st_crs(compareList[[1]])$input != sf::st_crs(compareList[[3]])$input) |
      (sf::st_crs(compareList[[1]])$units != sf::st_crs(compareList[[3]])$units) )
    warning( "\n", s[1], " and ", s[3], " do not share the same projection\n", immediate.=TRUE, call.=FALSE )

  ## output
  s <- c( movedToName, movedFromName )
  for( j in s ) {
    if( file.exists(j) )
      warning( "\n", j, " already exists, likely from a previous run, and will be overwritten\n", immediate.=TRUE, call.=FALSE )
    if( toupper(tools::file_ext(j)) != "SHP" & toupper(tools::file_ext(j)) != "GPKG" )
      warning( "\n", j, " is not a shapefile nor a geopackage\n", immediate.=TRUE, call.=FALSE )
  }

  ## TIF, output of road rasterization
  if( toupper(tools::file_ext(roadRasterName)) != "TIF" )
    warning( "\n", roadRasterName, " does not have the required .tif extension" )
  if( file.exists(roadRasterName) )
    warning( "\n", roadRasterName, " already exists, likely from a previous run, and will be overwritten\n", immediate.=TRUE, call.=FALSE )
  if( file.exists(costCSVName) )
    warning( "\n", costCSVName, " already exists, likely from a previous run, and will be overwritten\n", immediate.=TRUE, call.=FALSE )

  ## numeric parameters and value ranges
  if( !is.numeric(rasterResolution) | rasterResolution <= 0.0 ) {
    warning( "\nRaster resolution should be positive. You specified ", rasterResolution, "\n",
             immediate.=TRUE, call.=FALSE )
  } else {
    if( unit == 1 ) {
      unit.txt = "m"
    } else {
      unit.txt = "ft"
    }
    if( rasterResolution / unit < 5 )
      warning( paste0("\nSpecified raster resolution (", rasterResolution, unit.txt, ") likely too fine\n") )
    if( rasterResolution / unit > 50 )
      warning( paste0("\nSpecified raster resolution (", rasterResolution, unit.txt, ") likely too coarse\n") )
  }

  ## cost type
  if( !is.numeric(costType) )
    warning( "\n costType should be numeric\n" )
  if( costType != 1 & costType != 2 )
    warning( "\n costType should either be 1, for travel calculations in time (hours), or 2, for travel calculations in distance (kilometers)\n" )

  ## value uniqueness for input vector data fields
  s       <- c( toName, fromName )
  sFields <- c( toField, fromField )
  for( j in 1:2 ) {
    shp <- terra::vect( s[j] )
    v <- data.frame( shp[, sFields[j]] )
    if( length(v) != length( unique(v) ) )
      warning( "\nValues in field ", sFields[j], " of file ", s[j], " are not unique\n",
               immediate.=TRUE, call.=FALSE)
  }
}

#' @title Calculate runtime between processing instances
#'
#' @param t1 date-time or date object, preceding chronologically object t2
#' @param t2 date-time or date object, chronologically subsequent to object t1
#'
#' @noRd
#'
#' @export
reportTime <- function( t1, t2 ) {
  s <- as.numeric( as.character( round( difftime( t2, t1, units="secs"), 2 ) ) )
  sUnit <- "second(s)"
  if( s > 60 ) { s <- round( s / 60.0, 2 ); sUnit <- "minute(s)" }
  if( s > 60 ) { s <- round( s / 60.0, 2 ); sUnit <- "hour(s)" }
  return( paste( s, sUnit ) )
}

#' @title Convert a numeric raster to integer type
#' @param r spatRast object
#' @param name character, filename/path for the output integer raster
#'
#' @details The range of values in \emph{r} should in 0-255. The data type of
#' the output is set to INT1U. Large rasters will be processed in tiles. TIF
#' format is expected. If the file exists, it will be overwritten
#'
#' @export
setCellValueToInteger <- function( r, name ) {
  cat( "\nsetCellValueToInteger(): setting cell values to integers to conserve memory ..." ); flush.console()
  if( ncell(r) < (2^31-1) ) {
    setValues( r, as.integer(values(r)) )
    writeRaster(r, filename=name, overwrite=TRUE, datatype="INT1U" )
  } else {
    cat("\n")
    warning( "Owing to its size, ", ncell(r), " cells, ",
             "the raster will be processed in tiles", immediate.=TRUE, call.=FALSE )

    tmpName <- basename( tempfile() )
    tileNames <- makeTiles( r, y=c(40000,40000),
                            filename=paste0(tmpName, "_tile_.tif") )
    for( j in 1:length(tileNames) ) {
      tile.r <- rast( tileNames[j] )
      setValues( tile.r, as.integer(values(tile.r)) )
      invisible( gc() )
      writeRaster( tile.r, gsub(".tif", "_int.tif", tileNames[j]), datatype="INT1U" )
      rm( tile.r ); invisible( gc() )
      unlink( tileNames[j] )
    }
    fl <- list.files( pattern=tmpName )
    fl <- fl[substring(fl, nchar(fl)-3, nchar(fl)) == ".tif"] # avoid .ovr, .xml, etc
    r <- vrt( fl )
    writeRaster(r, filename=name, overwrite=TRUE, datatype="INT1U" )
    unlink( list.files(pattern=tmpName) )
    invisible( gc() )
  }
}

#' @title Rasterize a line vector file
#'
#' @param shpName character, filename/path of a line vector
#' @param resolution numeric, the resolution of the raster to be created
#' @param rasterName character, filename/path of the raster to be created
#' @param shpField character, optional, name of column in \emph{shpName}
#'
#' @details \emph{shpName} is expected to be a vector (line) layer representing
#' the road network. If \emph{rasterName} is expected to be for calculations
#' based on travel speed, \emph{shpField} must be specified, and provide speed
#' values in the (0,200] range and in miles per hour for each road segment. A
#' cell with more than one road segment present would inherit the highest
#' \emph{shpField} value. If \emph{shpField} is missing, travel cost
#' computations will be based on distance and all on-network cells will get a
#' value of 1.
#'
#' \emph{resolution} coarser than 50m (164ft) will induce unwarranted
#' over-generalization of the road network. Conversely, a very fine
#' \emph{resolution}, finer than 5m, can create a very large raster and routing
#' graph with numerous disconnected segments. These 'coarse' or 'fine'
#' \emph{resolution} thresholds have been determined empirically
#'
#' If \emph{rasterName} exists, it will be overwritten. TIFF format is expected.
#' Data type will be set to INT1U and NODATA cell values to 255
#'
#' @seealso [checkInputs]
#'
#' @examples
#' \dontrun{
#' rasterizeRoads("roads.gpkg", 20, "road_net.tif", "MPH" )
#' }
#'
#' @export
rasterizeRoads <- function ( shpName, resolution, rasterName, shpField="" ) {
  startTime <- Sys.time()

  cat( "\nrasterizeRoads(): reading", shpName, "..." ); flush.console()
  shp <- terra::vect( shpName )
  shp.ext <- terra::ext( shp )

  if( !missing(shpField) & nchar(shpField) > 0 ) {
    if( !(shpField %in% names(shp)) ) {
      stop( "\n\nrasterizeRoads(): ", shpField, " is not present in ", shpName, "\n" )
    }
    shpValRange <- as.numeric( range( shp[, shpField] ) )
    if( shpValRange[1] <= 0.0 | shpValRange[2] > 200.0 ) {
      stop( "\n\nrasterizeRoads(): ", shpField, " values in ", shpName, " should be in (0,199]\n\n" )
    }
  }

  ## modify bounding box to ensure origin is at [0,0]. Include one cell wide border
  if( shp.ext[1] %% resolution == 0.0 ) shp.ext[1] <- shp.ext[1] - 0.0001 ## xmin
  if( shp.ext[3] %% resolution == 0.0 ) shp.ext[3] <- shp.ext[3] - 0.0001 ## ymin
  if( shp.ext[2] %% resolution == 0.0 ) shp.ext[2] <- shp.ext[2] + 0.0001 ## xmax
  if( shp.ext[4] %% resolution == 0.0 ) shp.ext[4] <- shp.ext[4] + 0.0001 ## ymax

  shp.ext[1] <- floor(   shp.ext[1] / resolution ) * resolution - resolution ## xmin
  shp.ext[3] <- floor(   shp.ext[3] / resolution ) * resolution - resolution ## ymin
  shp.ext[2] <- ceiling( shp.ext[2] / resolution ) * resolution + resolution ## xmax
  shp.ext[4] <- ceiling( shp.ext[4] / resolution ) * resolution + resolution ## ymax

  r <- terra::rast( shp.ext, res=resolution )
  terra::crs( r ) <- terra::crs( shp )

  terra::mem_info( r )

  invisible( gc() )

  cat( "\nrasterizeRoads(): performing rasterization ..." ); flush.console()

  # gdalUtilities::gdal_rasterize() executes faster than the terra::rasterize()
  # gdal_rasterize(), however, does not support a function that conditions the
  # cell values. If costType is 1, and to ensure the maximum shpField value
  # among vector segments present in a raster cell is used for the raster, the
  # road segments in the input vector must be first sorted by their shpField in
  # ascending order. That is accomplished via an sql query applied as argument
  # to the function. If in another application the lower value is to be kept,
  # then the sorting should be in descending order ["ORDER BY", rdField,
  # "DESC"]. If another metric is needed (e.g. mean), the terra::rasterize()
  # must be used instead.
  # Note that using "TILED=YES" as a 'co' option in gdal_rasterize, will likely
  # create a raster smaller in bytes compared to one created without it.
  # Runtime does not seem to be affected much by the use of the 'co' option.
  if( !missing(shpField) & nchar(shpField) > 0 ) { # cost based on travel speed
    sql.txt <- paste("SELECT geom,", shpField, "FROM",  sf::st_layers(shpName)$name, "ORDER BY", shpField, "ASC")
    gdalUtilities::gdal_rasterize(shpName, rasterName, a=shpField, sql=sql.txt,
                                  tr=c(resolution,resolution),
                                  te=c(shp.ext[1], shp.ext[3], shp.ext[2], shp.ext[4]),
                                  ot="Byte",
                                  co=c("COMPRESS=DEFLATE", "TILED=YES"),
                                  a_nodata = 255)
  } else { # cost based on travel distance
    gdalUtilities::gdal_rasterize(shpName, rasterName,
                                  burn=1,
                                  tr=c(resolution,resolution),
                                  te=c(shp.ext[1], shp.ext[3], shp.ext[2], shp.ext[4]),
                                  ot="Byte",
                                  co=c("COMPRESS=DEFLATE", "TILED=YES"),
                                  a_nodata = 255)
  }

  #r <- terra::rasterize( shp, r, field=shpField, fun=max, touches=FALSE, filename=rasterName, background=255, wopt=list(datatype="INT1U", NAflag=255), overwrite=T )
  cat( "\nrasterizeRoads(): Output saved as", rasterName ); flush.console()
  cat( paste( "\nrasterizeRoads(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" ) )
}

#' @title Set raster background cell values to NA
#'
#' @description Function useful in fully automated processing of numerous road
#' network rasters contributed by third parties to determine their background,
#' no road, value. Background values of such rasters have been found to be 0,
#' <0, -9999, or 255. These values are incompatible with processing provisions
#' of this package
#'
#' @details A cell value is labeled 'background' in \emph{rasterName} and is
#' subsequently set to NA if it represents at least 85% for the raster. Road
#' network rasters are sparse matrices where cells corresponding to the road
#' network are collectively a small percentage of the total number of cells. If
#' the background value must change to NA, the value cells are encoded as
#' integers
#'
#' @param rasterName character, filename/path of the raster to be evaluated
#'
#' @export
backgroundCellValue <- function( rasterName ) {
  startTime <- Sys.time()
  cat( "\ncheckBackgroundCellValue(): Checking validity of", rasterName, "background cell values" ); flush.console()
  r <- terra::rast( rasterName )
  tbl <- table( values( r ) ) / ncell( r )
  df <- data.frame( cbind( as.numeric( names( tbl ) ), tbl ) )
  names( df ) <- c( "speed", "ratio" )
  w <- df$ratio > 0.85
  if( nrow( df[w,] ) > 0 ) {
    v <- df$speed[w]
    cat( "\ncheckBackgroundCellValue(): Converting", rasterName, "background value from", v, "to NA" ); flush.console()
    r[r == v] <- NA
    cat( "\ncheckBackgroundCellValue(): Overwriting", rasterName, "...\n" ); flush.console()
    terra::writeRaster( r, rasterName,
                        datatype="INT1U",
                        gdal=c( "COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9", "NUM_THREADS=ALL_CPUS" ),
                        overwrite=TRUE )
  }
  cat( "\ncheckBackgroundCellValue(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" )
}

#' @title Create a cell connectivity data frame from raster
#'
#' @description Uses a raster representing a road network to create a data frame
#' featuring adjacent, and therefore connected, pairs of cells and calculates
#' the transition cost for each pair
#'
#' @param rasterName character, filename/path of a road network raster
#' @param costType integer, 1 for travel calculations based on time, 2 for
#' travel calculations based on distance
#'
#' @details If \emph{costType} = 1, value cells of the raster are expected to
#' represent travel speed in miles per hour. Although integers, the cell IDs
#' in the output data frame are of numeric type, to prevent numerical overflow
#' issues with rasters comprising more than 2^31-1 cells. The output of this
#' function is used by \emph{createGraph}()
#'
#' @returns data frame with three columns. The first two are the IDs of cell
#' pairs, as inherited from the raster. The third column is the transition cost
#' between the centers of paired cells, either as travel time in seconds, if
#' \emph{costType} = 1, or the planar distance in meters, if \emph{costType} = 2
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' df <- prepareGraph( "road_net.tif", 1 )
#' head( df )
#' }
#'
#' @useDynLib tRee2MillCost, .registration = TRUE
#' @importFrom foreach foreach %dopar%
#' @importFrom Rcpp sourceCpp
#' @importFrom snow makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#'
#' @export
prepareGraph <- function( rasterName, costType ) {
  startTime <- Sys.time()
  cat( "\nprepareGraph(): processing ..." ); flush.console()
  r  <- terra::rast( rasterName )

  resolution <- terra::res(r)[1]
  unit       <- terra::linearUnits( r )
  nRows      <- nrow( r )
  nCols      <- ncol( r )
  chunkSize  <- 2000

  nChunks <- as.integer( ceiling(nRows/chunkSize) )

  if( nChunks == 1 ) {
    chunk.df <- data.frame( ROWSTART=1, ROWEND=nrow(r), CELLIDOFFSET=0 )
  } else {
    chunkRowsStart <- seq(1, nRows, chunkSize)
    chunkRowsEnd   <- chunkRowsStart + chunkSize - 1
    if( chunkRowsEnd[length(chunkRowsEnd)] > nRows )
      chunkRowsEnd[length(chunkRowsEnd)] <- nRows
    chunk.df <- data.frame( ROWSTART = chunkRowsStart, ROWEND = chunkRowsEnd )
    chunk.df$ROWSTART[2:nrow(chunk.df)] <- chunk.df$ROWSTART[2:nrow(chunk.df)] - 1
    chunk.df$CELLIDOFFSET <- ( chunk.df$ROWEND - chunkSize - 1 ) * nCols ####
    chunk.df$CELLIDOFFSET[nrow(chunk.df)] <- ( chunk.df$ROWSTART[nrow(chunk.df)] - 1 ) * nCols
    chunk.df$CELLIDOFFSET[1] <- 0
    chunk.df$CELLIDOFFSET[nrow(chunk.df)] <- ( chunk.df$ROWSTART[nrow(chunk.df)] - 1 ) * nCols
  }
  rm( r )
  invisible( gc(full=T) )

  cat( "\nprepareGraph(): calculating node-to-node cost ..." ); flush.console()

  cl <- snow::makeCluster( min(nChunks, 4, parallel::detectCores()) )
  doSNOW::registerDoSNOW( cl )

  chunk.result <- foreach::foreach( j=1:nrow(chunk.df), .packages=c("tRee2MillCost") ) %dopar% {
    r <- terra::rast( rasterName )
    chunk.ID.SW <- terra::cellFromRowCol( r, chunk.df$ROWEND[j], 1 )
    chunk.xy.SW <- terra::xyFromCell( r, chunk.ID.SW )
    chunk.ID.NE <- terra::cellFromRowCol( r, chunk.df$ROWSTART[j], nCols )
    chunk.xy.NE <- terra::xyFromCell( r,  chunk.ID.NE )
    chunk.ext   <- terra::ext( c(chunk.xy.SW[1], chunk.xy.NE[1], chunk.xy.SW[2], chunk.xy.NE[2]) ) + resolution/2
    tmp.r <- terra::crop( r, chunk.ext )
    chunk.name <-  paste0( "chunk_", j, ".tif" )
    terra::writeRaster( tmp.r, chunk.name, datatype="INT1U", overwrite=TRUE )
    mat <- tiff::readTIFF( chunk.name, as.is=T )
    unlink( chunk.name )
    df <- rcpp_buildNodeDF( mat, resolution, unit, costType )
    rm(mat, tmp.r)
    df$from <- df$from + chunk.df$CELLIDOFFSET[j]
    df$to <- df$to + chunk.df$CELLIDOFFSET[j]
    gc()
    df
  }

  snow::stopCluster( cl )
  df <- do.call( rbind, chunk.result )
  rm( chunk.result )
  invisible( gc() )

  endTime = Sys.time()
  cat( paste( "\nprepareGraph(): Completed in", reportTime( startTime, endTime ), "\n\n" ) )
  return( df )
}

#' @title Set the value of selected raster cells to NA
#'
#' @details Deprecated by the far more efficient \emph{terra::update()}
#' function. Will
#' be removed in future updates
#'
#' @param rasterName character, filename/path of the raster
#' @param IDs numeric, vector of cell IDs
#' @param tmpName character, filename/path of the output raster
#'
#' @export
maskCells <- function( rasterName, IDs, tmpName ) {
  r <- terra::rast( rasterName )
  p <- terra::vect( xyFromCell( r, IDs ), type="points", crs=crs(r) )
  r <- terra::mask( r, p, inverse=TRUE )
  terra::writeRaster( r, tmpName, datatype="INT1U" )
}

#' @title Detect road network disconnections
#'
#' @description Operates on a graph whose nodes represent cell IDs of a road
#' network raster and a data frame of connected nodes to determine network
#' segments that are disconnected from the rest of the network. The disconnected
#' segments, if any, are saved into a file named dc_filexxxxx.gpkg. The
#' GeoPackage can be examined to determine if editing of the vector
#' representation of the road network is warranted. The functions is called
#' internally by \emph{createGraph()}, it is not designed to be used
#' independently
#'
#' @param rasterName character, filename/path of the road network raster
#' @param graph graph object created by \emph{createGraph()}
#' @param node.df data frame created by \emph{prepareGraph()}
#' @param nIterations integer, the number of randomly selected node IDs used to
#' determine network connectivity. Default value is 10, determined empirically
#' @param nodeThreshold numeric, ratio of nodes compared to the total expected
#' to belong to the connected network
#'
#' @details nodeThreshold should be in (0,1) although a value lower than 0.5
#' would denote a poor quality vector road network representation with a lot of
#' disconnected segments. Default values for nIterations and nodeTrheshold,
#' set to 10 and 0.85, were determined empirically
#'
#' @seealso [prepareGraph] and [createGraph]
#'
#' @noRd
#'
#' @export
#'
disconnectedSegments <- function( rasterName, graph, node.df, ... ) {
  nIterations <- 10
  nodeThreshold <- 0.85
  startTime <- Sys.time()
  flag <- TRUE
  s <- node.df[sample(1:nrow(node.df), nIterations), "nodeIDs"]
  j <- 1
  while( flag ) {
    cat( "\nSearching for disconnected segments" )
    RcppParallel::setThreadOptions( parallel::detectCores() )
    iso <- cppRouting::get_isochrone( graph, from=s[j], lim=1E12, long=FALSE )
    if( length( iso[[as.character(s[j])]] ) > ( nrow(node.df) * nodeThreshold ) ) {
      flag <- FALSE
      disconnectedNodeIDs <- setdiff( node.df$nodeIDs, as.numeric(iso[[as.character(s[j])]]) )
      if( length( disconnectedNodeIDs ) > 0 ) {
        cat( "\nfound", length(disconnectedNodeIDs), "disconnected segment(s)"); flush.console()
        disconnectedNodes <- terra::xyFromCell( terra::rast(rasterName), disconnectedNodeIDs )
        disconnectedNodes <- terra::vect( matrix(disconnectedNodes[, c("x","y")], ncol=2), type="points", crs=terra::crs( terra::rast(rasterName )) )
        outName <- paste0("dc_", basename(tempfile()), ".gpkg")
        terra::writeVector( disconnectedNodes, outName )
        cat( "\nDisconnected segments saved as", outName, "\n" ); flush.console()
      } else {
        cat( "\nThere are no disconnected segments\n" )
        return()
      }
    }
    if( j == nIterations & flag ) {
      cat( "\n" )
      warning( "did not identify any disconnected road segments after ", nIterations, " iteration(s)", immediate.=TRUE, call.=FALSE )
      cat( "\nDisconnected segment search completed in", reportTime( startTime, Sys.time() ), "\n" )
    }
    j <- j + 1
  }
  if( !flag ) {
    cat( "\nRemoving disconnected segments from", rasterName ); flush.console()
    terra::update( terra::rast(rasterName), cells=disconnectedNodeIDs, values=NA )
    cat( "\n", rasterName, "has been updated\n"); flush.console()
  }
  cat( "\nDisconnected segment search completed in", reportTime( startTime, Sys.time() ), "\n" )
}

#' @title Create a routing graph from a road network raster
#'
#' @description Relies on a raster representation of a road network
#' \emph{(rasterName)} and embedded functions to create a network graph. Network
#' cells with all 8 immediate neighbors representing background (no road) are
#' labeled orphan, saved into a file named orphan_xxxx.gpkg, and removed from
#' \emph{rasterName}. Segments with more than one cell each that are
#' disconnected from the rest of the network are saved into a file named
#' dc_xxxx.pgkg and are also removed from \emph{rasterName}
#'
#' @details The frequency of orphan cells or disconnected cell groups (road
#' network segments) tends to increase as the resolution of the raster becomes
#' finer.
#'
#' @param rasterName character, filename/path of the road network raster
#' @param costType integer, 1 for travel calculations based on time, 2 for
#' travel calculations based on distance
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- createGraph( "road_net.tif", 1 )
#' }
#'
#' @seealso [cppRouting::makegraph] and [prepareGraph]
#'
#' @export
createGraph <- function( rasterName, costType ) {
  startTime <- Sys.time()
  graph.df    <- prepareGraph( rasterName, costType )
  cat( "\ncreateGraph(): arranging nodes ..." ); flush.console()
  nodeIDs     <- sort( unique( c( graph.df$from, graph.df$to ) ) )
  nIDs        <- length( nodeIDs )
  node.df     <- data.frame( cbind( nodeIDs, terra::xyFromCell(terra::rast(rasterName), nodeIDs) ) )
  cat( "\ncreateGraph(): assembling nodes onto a graph ..." ); flush.console()
  graph       <- cppRouting::makegraph( graph.df, directed = FALSE, coords = node.df )
  graph$attrib["costType"] <- list( NULL )
  graph$attrib["costType"][[1]] <- costType
  cellIDs     <- terra::cells( terra::rast(rasterName) )
  if( nrow(graph$coords) < length(cellIDs) ) {
    cat( "\ncreateGraph(): checking for orphan graph nodes ..." ); flush.console()
    diffIDs     <- setdiff( cellIDs, graph$coords$nodeIDs )
    tmpName     <- paste0( "orphan_", basename(tempfile()), ".gpkg" )
    orphanMat   <- terra::xyFromCell( terra::rast(rasterName), diffIDs )
    orphanP     <- terra::vect( orphanMat, type="points", crs=crs(rast(rasterName)) )
    terra::writeVector( orphanP, tmpName )
    cat( "\ncreateGraph(): Orphan nodes saved to", tmpName ); flush.console()
    cat( "\ncreateGraph(): removing", length(diffIDs), "orphan graph nodes ..." ); flush.console()
    terra::update( terra::rast(rasterName), cells=diffIDs, values=NA)
  }
  disconnectedSegments( rasterName, graph, node.df )

  invisible( gc() )
  cat( "\ncreateGraph(): Completed in", reportTime( startTime, Sys.time() ), "\n" )
  return( graph )
}

#' @title Move points in a vector file to road network cells
#'
#' @param rasterName character, filename/path of the road network raster
#' @param inName character, filename/path of the point vector layer
#' @param outName character, filename/path of the moved point vector layer
#'
#' @description Travel cost calculations are performed between locations (nodes)
#' of a graph representing the road network. Points that are not on the network
#' must first move from their original location. The new location is determined
#' as the one on the network closest (in 2D) to the original location.
#'
#' @details \emph{movePt2RdSegment()} must be run after \emph{createGraph()} to
#' ensure that any disconnected road network segments have been already removed
#' from \emph{rasterName}. Distribution moments of the move distances are shown
#' on the console
#'
#' @return A point vector file (\emph{outName}). Includes as attribute the
#' coordinates of the new locations and the Euclidean distance between the
#' original and moved location
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' fromPath <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- createGraph( "road_net.tif", 1 )
#' movePt2RdSegment( "road_net.tif", fromPath, "movedFromLocations.gpkg" )
#' p <- terra::vect( "movedFromLocations.gpkg" )
#' head( data.frame(p) )
#' }
#'
#' @importFrom foreach %dopar%
#'
#' @export
movePt2RdSegment <- function( rasterName, inName, outName ) {
  startTime <- Sys.time()
  cat( "\nmovePt2RdSegment(): Reading inputs\n\t", inName, "\n\t", rasterName ); flush.console()

  p <- terra::vect( inName )
  r <- terra::rast( rasterName )

  angle.rad.vec <- seq(0, 355, 5) * pi / 180

  circleFun <- function( buffer, rasterName ) {
    circle.v <- vect( cbind( cos(angle.rad.vec) * buffer,
                             sin(angle.rad.vec) * buffer ),
                      type="polygons", crs=crs(rast(rasterName)) )
    return( circle.v )
  }

  nP <- length( p )

  cat( "\nmovePt2RdSegment(): Calculating distances ...\n" ); flush.console()
  crds.mat <- terra::crds( p )
  res <- terra::res(terra::rast(rasterName))[1]

  cl <- snow::makeCluster( min(10, parallel::detectCores() / 2) )
  doSNOW::registerDoSNOW(cl)

  if( nP > 1000 ) {
    pb <- utils::txtProgressBar( max=nP, initial=1, style=3 )
    progress <- function( n ) {
      utils::setTxtProgressBar( pb, n )
    }
  } else {
    progress <- NULL
  }

  resList <- foreach::foreach( j = 1:nrow(crds.mat), .packages="terra", .options.snow=list(progress=progress) ) %dopar% {
    buffer  <- 10 * res
    circle.v <- terra::shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] )
    while( is.null( terra::intersect(terra::ext(terra::rast(rasterName)), terra::ext(circle.v)) ) ) {
      ## this while loop is necessary when the buffered point is outside the bounding
      ## box of the road raster. Example: fuzzed FIA plot locations well into the ocean
      buffer = buffer * 5
      circle.v <- terra::shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] )
    }
    sub.r <- terra::crop( terra::rast(rasterName), circle.v, mask=TRUE )
    flag <- TRUE
    while( flag ) {
      cellIDs <- terra::cells( sub.r )
      if( length( cellIDs ) > 0 ) {
        flag <- FALSE
        rdCellXY.mat <- terra::xyFromCell( sub.r, cellIDs )
        dist.mat <- terra::distance( matrix(crds.mat[j,], ncol=2), rdCellXY.mat, lonlat=FALSE )
        minDist <- min( dist.mat[1,] )
        w <- dist.mat[1,] == minDist
        newCoords <- matrix(rdCellXY.mat[w,], ncol=2)[1,]
      } else {
        buffer <- buffer * 5
        sub.r   <- terra::crop( terra::rast(rasterName),
                                terra::shift( circleFun(buffer, rasterName), dx=crds.mat[j,1], dy=crds.mat[j,2] ),
                                mask=TRUE )
      }
    }
    c( newCoords, minDist )
  }

  if( nP > 1000 ) rm( pb )
  snow::stopCluster(cl)

  res.mat <- do.call( rbind, resList )
  p$MOVEX    <- res.mat[,1]
  p$MOVEY    <- res.mat[,2]
  p$MOVEDIST <- res.mat[,3]
  p <- terra::vect( res.mat[,1:2], type="points", atts=data.frame(p), crs=terra::crs(p) )

  terra::writeVector( p, outName, overwrite=T )

  cat( "\n\nmovePt2RdSegment()" )
  cat( "\n\tmove distance metrics:" )
  cat( "\n\tMean :  ", formatC( mean( p$MOVEDIST ),   format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMedian: ", formatC( median( p$MOVEDIST ), format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMinimum:", formatC( min( p$MOVEDIST ),    format="f", digits=2, width=10 ), "units" )
  cat( "\n\tMaximum:", formatC( max( p$MOVEDIST ),    format="f", digits=2, width=10 ), "units" )
  cat( "\n\tStDev:  ", formatC( sd( p$MOVEDIST ),     format="f", digits=2, width=10 ), "units" )

  cat( "\n\nmovePt2RdSegment(): Completed in", reportTime( startTime, Sys.time() ), "\n\n" )
  flush.console()
}

#' @title Determine if points are on value cells of a raster
#'
#' @param rasterName character, filename/path of the network raster
#' @param pName character, filename/path of a point vector layer
#'
#' @return A console message with connectivity results
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' fromPath <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- createGraph( "road_net.tif", 1 )
#' movePt2RdSegment( "road_net.tif", fromPath, "movedFromLocations.gpkg" )
#' pOriginal <- terra::vect( fromPath )
#' pMoved <- terra::vect( "movedFromLocations.gpkg" )
#' connectivity( "road_net.tif", "movedFromLocations.gpkg" )
#' connectivity( "road_net.tif", fromPath )
#' }
#'
#' @export
connectivity <- function( rasterName, pName ) {
  r   <- terra::rast( rasterName )
  p   <- terra::vect( pName )
  v   <- terra::extract( r, p )
  w   <- is.na( v[, names(r)[1]] )
  nNA <- length( v[w, names(r)[1]] )
  if( nNA > 0 ) {
    warning( "\n", nNA, " out of ", length(p), " element(s) of ",
             basename(pName), " are on NA ", rasterName, " cells\n\n" )
  } else {
    cat( "\nAll elements of", basename(pName), "are on value", rasterName, "cells\n" )
  }
}

#' @title Compute the network cost matrix
#'
#' @param graph graph object generated by \emph{createGraph()}
#' @param rasterName character, filename/path of the raster network
#' @param fromName character, filename/path of a point vector layer with travel
#' start locations
#' @param fromField character, name of column in \emph{fromName} used as point
#' identifier
#' @param toName character, filename/path of a point vector layer with travel
#' end locations
#' @param toField character, name of column in \emph{toName} used as point
#' identifier
#'
#' @description Calculates the travel cost, either in hours or kilometers,
#' between points in \emph{fromName} and \emph{toName} respectively.
#'
#' @details \emph{fromName} and \emph{toName} locations should be on, or have
#' been moved to the network by applying \emph{movePt2RdSegement()}. The
#' \emph{costType} applied is read from the \emph{graph} object
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' fromPath <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#' toPath <- system.file( "extdata", "mills.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- createGraph( "road_net.tif", 1 )
#' movePt2RdSegment( "road_net.tif", fromPath, "movedFromLocations.gpkg" )
#' names( terra::vect( "movedFromLocations.gpkg" ) )
#' movePt2RdSegment( "road_net.tif", toPath, "movedToLocations.gpkg" )
#' names( terra::vect( "movedToLocations.gpkg" ) )
#' cost.df <- calculateCost( graph, "road_net.tif",
#'                           "movedFromLocations.gpkg", "sampleID",
#'                           "movedToLocations.gpkg", "COMPANY_NAME" )
#' write.csv( cost.df, "cost_matrix.csv", row.names=FALSE )
#' }
#'
#' @return data frame. The first column named \emph{toField} contains the IDs
#' of the destination points. Each of the remaining columns is named by the
#' values in \emph{fromField} and reports the pairwise cost either as travel
#' time in hours or travel distance in kilometers across \emph{fromName} and
#' \emph{toName} points. An NA value indicates no path exists between the
#' corresponding \emph{fromName} and \emph{toName} locations and suggests that
#' one or both points in the pair have not moved using \emph{movePt2RdSegment()}
#'
#' @export
calculateCost <- function( graph, rasterName, fromName, fromField, toName, toField ) {
  startTime <- Sys.time()
  cat( "\ncalculateCost(): calculating departure and destination nodes ..." ); flush.console()
  fromCellIDs   <- terra::cellFromXY( terra::rast(rasterName), terra::crds(terra::vect(fromName)) )
  fromIDs       <- data.frame( terra::vect(fromName) )[,fromField ]
  toCellIDs     <- terra::cellFromXY( terra::rast(rasterName), terra::crds(terra::vect(toName)) )
  toIDs         <- data.frame( terra::vect(toName) )[,toField ]
  cat( "\ncalculateCost(): optimizing graph ..." ); flush.console()
  costType      <- graph$attrib["costType"][[1]]
  #graph$attrib$costType will be removed from cppRouting::cpp_simplify()
  graph         <- cppRouting::cpp_simplify( graph, keep=c(fromCellIDs, toCellIDs) )
  cat( "\ncalculateCost(): calculating cost matrix ..." ); flush.console()
  RcppParallel::setThreadOptions( parallel::detectCores() )
  mat           <- cppRouting::get_distance_matrix( graph, from=fromCellIDs, to=toCellIDs )
  if( costType == 1 ) {
    mat <- t( round( mat/3600, 3 ) ) ## convert secs to hours and transpose
  } else {
    mat <- t( round( mat/1000, 3 ) ) ## convert meters to kilometers and transpose
  }
  df            <- data.frame( mat )
  df            <- cbind( as.character(toIDs), df )
  names( df )   <- c( toField, as.character(fromIDs) )
  cat( "\ncalculateCost(): Completed in", reportTime( startTime, Sys.time() ), "\n" )
  return( df )
}

#' @title Get the road network part accessible from a node within specified time
#' or distance
#'
#' @description Determines the road network components that are within user-
#' specified travel time (in hours) or distance (in kilometers) from a road
#' raster cell ID, saves it in .tif format, and, optionally, adds it to a bitmap
#' figure in PNG format
#'
#' @param graph graph object generated by \emph{createGraph()}
#' @param cellID numeric, road network raster cell where travel starts
#' @param threshold numeric, maximum travel time, in hours, or travel distance,
#' in kilometers, from the start node
#' @param label character, used in the figure title and the output PNG filename
#' @param PNG logical. If TRUE, a figure of the isochrone is created in PNG
#' format
#' @param colRamp character, either "RGB" or "508". The latter ensures colors
#' used in the figure are in compliance with Section 508 of the Rehabilitation
#' Act
#' @param trimPNG logical. If TRUE, extra white space is trimmed from the PNG
#' and a 10 pixel border is added
#'
#' @details cellID refers to a cell in the road network raster used to create
#' the graph. The cellID can be determined from coordinates or raster row and
#' column using \emph{terra::cellFromRowCol()} or \emph{terra::cellFromXY()}. It
#' can also be determined from the graph, if the corresponding graph node ID is
#' known, via \emph{cellFromGraphNode()}. Note that both terra package functions
#' will return a cellID even if the raster row and column or XY location are not
#' part of the road network. Or, they were initially but were removed as
#' belonging to a disconnected component of the road network. Specifying a
#' cellID that is not part of the network results in an error that states 'Some
#' nodes are not in the graph'.
#'
#' The resolution of the isochrone raster created is calculated from the graph
#' object and matches the resolution of the road raster used to create the graph
#'
#' The term [isochrone]{.underline} has been inherited from
#' \emph{cppRounting::isochrone()} and does imply \emph{threshold} is
#' referencing travel time. The type of cost calculation is determined by the
#' \emph{costType} used in \emph{createGraph()}
#'
#' If a PNG figure is requested, the pixel dimensionality of the plot region
#' (without the axes, legend, and title) will match the rows and columns in the
#' isochrone raster. This provision can create a very large bitmap but avoids
#' resampling which can be erroneously perceived as road network discontinuities
#' when the bitmap is examined. The location pertaining to the \emph{cellID} is
#' displayed by a white dot
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' toPath <- system.file( "extdata", "mills.gpkg", package="tRee2MillCost" )
#' # with rdField specified in rasterizeRoads(), the graph should be based on
#' #travel time
#' graphPath <- system.file( "extdata", "graph_20m_time.rds", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- readRDS( graphPath )
#' movePt2RdSegment( "road_net.tif", toPath, "movedToLocations.gpkg" )
#' mills.v <- terra::vect( "movedToLocations.gpkg" )
#' # pick as destination the third mill in toPath
#' mill.xy <- terra::crds( mills.v[3] )
#' mill.cellID <- terra::cellFromXY( terra::rast( "road_net.tif" ), mill.xy )
#' getIsochrone( graph, mill.cellID, 0.5, mills.v$COMPANY_NAME[3], TRUE, "RGB", TRUE )
#' }
#'
#' @seealso [cellFromRowCol], [graphNodeFromXY], [cellFromGraphNode], and
#' [cppRouting::get_isochrone]
#'
#' @return spatrast object saved as 'isochrone_<threshold><h|km>_<label>.tif'.
#' If PNG=TRUE, also a PNG figure named
#' 'isochrone_<threshold><h|km>_<label>.png'
#'
#' @export
getIsochrone <- function( graph, cellID, threshold, label, PNG=FALSE, colRamp, trimPNG=TRUE ) {
  if( !(PNG %in% c(TRUE, FALSE)) ) stop( "\ngetIsochrone(): PNG should be TRUE or FALSE\n\n" )
  if( !is.logical(PNG) )
    stop( "\ngetIsochrone(): PNG should be TRUE or FALSE\n\n" )
  if( PNG ) {
    colRamp = toupper(colRamp)
    if( colRamp != "RGB" & colRamp != "508" )
      stop( "\ngetIsochrone(): colRamp should be 'RGB' or '508'\n\n" )
    if( !(trimPNG %in% c(TRUE, FALSE)) )
      stop( "\ngetIsochrone(): trimPNG should be TRUE or FALSE\n\n" )
  }

  costType <- graph$attrib$costType

  # in ifelse(), convert threshold from hours to seconds or meters to kilometers
  breaks <- seq( 0, ifelse( costType == 1, threshold * 3600, threshold * 1000 ), length.out=2048 )
  RcppParallel::setThreadOptions( parallel::detectCores() )
  iso <- cppRouting::get_isochrone( graph, from=cellID, lim=breaks, setdif=T, long=T )
  names(iso)[2] <- "nodeIDs"
  iso <- merge(iso, graph$coords, by="nodeIDs" )
  iso$lim <- as.numeric(iso$lim)
  rangeX <- range( iso$x )
  rangeY <- range( iso$y )
  deltaX <- rangeX[2] - rangeX[1]
  deltaY <- rangeY[2] - rangeY[1]
  if( nrow(graph$coords) > 1E6 ) {
    s = sample( 1:nrow(graph$coords), size=1E6, replace=FALSE )
    res = min(RANN::nn2(graph$coords[s, 2:3], k=2)[[2]][,2])
  } else {
    res = min(RANN::nn2(graph$coords[, 2:3], k=2)[[2]][,2])
  }

  ext <- terra::ext( c( floor(rangeX[1]/res)*res, ceiling(rangeX[2]/res)*res,
                        floor(rangeY[1]/res)*res, ceiling(rangeY[2]/res)*res ) )
  r <- terra::rast( ext, res=res )
  r <- terra::rasterize( as.matrix(iso[, c("x", "y")]), r, values=iso$lim, fun="mean" ) / ifelse( costType == 1, 3600, 1000 )
  unitTxt <- ifelse( graph$attrib$costType == 1, "h", "km" )
  terra::writeRaster(r, paste0("isochrone_", threshold, unitTxt, "_", label, ".tif"), overwrite=TRUE )

  if( PNG ) {
    options(scipen=999)
    if( colRamp == "RGB" )
      pal <- grDevices::colorRampPalette( c("blue4", "blue", "aquamarine", "green", "darkgreen", "yellow", "orange", "purple", "red") )
    if( colRamp == "508" )
      pal <- grDevices::colorRampPalette( c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499", "#DDDDDD") )
      #pal <- grDevices::colorRampPalette( c("#444444", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") )

    ht <- nrow( r )
    wd <- ncol( r )
    maxDim <- max( ht, wd )
    leftMargin   <- max(30,  ceiling( maxDim * 0.045 ) )
    bottomMargin <- max(30,  ceiling( maxDim * 0.030 ) )
    topMargin    <- max(50, ceiling( maxDim * 0.100 ) )
    rightMargin  <- max(80, ceiling( maxDim * 0.120 ) )
    pointSize    <- 0.01 * maxDim + 10
    pngHeight <- ht + bottomMargin + topMargin
    pngWidth  <- wd + leftMargin + rightMargin
    pngName   <- paste0( "isochrone_", threshold, unitTxt, "_", gsub(" ", "_", label), ".png" )

    png( pngName,
         height=pngHeight, width=pngWidth, pointsize=pointSize, units="px", res=72 )
    par( mai=c(bottomMargin, leftMargin, topMargin, rightMargin) / 72 )
    terra::plot( r,
                 mar=NA,
                 main=paste0( "Isochrone ", threshold, unitTxt, " ", label ),
                 cex.main = sqrt( pointSize / 12 ),
                 plg=list( title=ifelse(costType == 1, "Hours", "km") ),
                 col=pal( 256 ), background="#202020", maxcell=Inf, smooth=FALSE, box=FALSE, buffer=FALSE )
    terra::points( graph$coords[graph$coords$nodeIDs == as.character(cellID), c("x", "y")], pch=20, col="white", cex=1.5 )
    dev.off()

    if( trimPNG ) {
      img <- magick::image_read( pngName )
      img <- magick::image_trim( img )
      img <- magick::image_border( img, color="white", geometry="10x10")
      magick::image_write(img, pngName, compression="LZMA")
    }
    options( scipen=0 )
  }
}

#' @title Get the graph node ID using coordinates
#'
#' @description Returns the graph node ID at specified coordinates
#' Graph note IDs shall not be confused with the cell IDs of the road network
#' raster used to create the graph
#'
#' @param graph object generated by \emph{createGraph()}
#' @param rasterName character, filename/path of the raster network used to
#' create the graph
#' @param X Numeric, X coordinate
#' @param Y Numeric, Y coordinate
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' graphPath <- system.file( "extdata", "graph_20m_time.rds", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' # Since rasterizeRoads() has the rdField populated the graph loaded should be
#' # based on travel time
#' graph <- readRDS( graphPath )
#' x <- 496350
#' y <- 4903210
#' graphNodeID <- graphNodeFromXY( graph, terra::rast("road_net.tif"), X=x, Y=y )
#' cat( graphNodeID )
#' y <- 4903230
#' graphNodeID <- graphNodeFromXY( graph, terra::rast("road_net.tif"), X=x, Y=y )
#' }
#'
#' @export
graphNodeFromXY <- function( graph, rasterName, X, Y ) {
  r <- terra::rast( rasterName )
  cellID <- terra::cellFromXY( r, matrix(c(X, Y), ncol=2) )
  if( is.na(cellID) ) {
    cat( "\nretrievGraphNodeIDbyCoordinates(): The coordinates provided are not on", rasterName, "\n\n" )
    return( NA )
  }
  w = as.numeric(graph$dict$ref) == cellID
  node = graph$dict$id[w]
  if( length(node) == 0 ) {
    cat( "\nretrievGraphNodeIDbyCoordinates(): There is no graph node at the coordinates provided\n\n" )
    return( NA )
  }
  return( node )
}

#' @title Get the road network cellID from a graph node
#'
#' @description Determines the cellID of the road network raster from a node on
#' the graph created from the raster. The node must exist
#'
#' @param graph object generated by \emph{createGraph()}
#' @param node numeric, the ID of a graph node
#'
#' @return numeric, the ID of a cell of the road network raster used to create
#' the graph. Or NA if the node does not exist or the graph
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' graphPath <- system.file( "extdata", "graph_20m_time.rds", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' #' # Since rasterizeRoads() has the rdField populated the graph loaded should be
#' # based on travel time
#' graph <- readRDS( graphPath )
#' node <- 18692
#' cellID <- cellFromGraphNode( graph, node )
#' node.xy <- terra::xyFromCell( terra::rast( "road_net.tif" ), cellID )
#' cat( paste0("Graph node=", node, ", cellID=", cellID, ", X=", node.xy[1], ", Y =", node.xy[2], "\n") )
#' }
#'
#' @seealso [graphNodeFromXY] and [getIsochrone]
#'
#' @export
cellFromGraphNode <- function ( graph, node ) {
  w <- graph$dict$id == node
  if( nrow(graph$dict[w,]) == 0 ) {
    warning( "\ncellFromGraphNode(): ", node, " is not part of the graph specified\n" )
    return( NA )
  } else {
    return( as.numeric(graph$dict$ref[w]) )
  }
}

#' @title Summarize travel cost
#'
#' @description Processes a cost matrix to generate a summary
#'
#' @param costDF data frame created by \emph{calculateCost}
#' @param fromName character, filename/path of the point vector layer
#' representing travel start location(s)
#' @param fromField character, the field in \emph{fromName} used with
#' \emph{calculateCost()} and representing travel start locations
#' @param maxMoveDistance optional numeric, the maximum 2D distance a
#' \emph{fromName} location is allowed to move to be on the road network and
#' still considered in the summary
#' @param travelThreshold optional numeric, maximum travel time, in hours, or
#' travel distance, in kilometers, to destination location(s) considered in the
#' summary
#'
#' @details \emph{maxMoveDistance} should be in the linear unit of the
#' projection of the spatial data used to calculate the cost matrix. If both
#' \emph{maxMoveDistance} and \emph{travelThreshold} are missing or set to "",
#' all locations in \emph{fromName} will be used to create the summary.
#' \emph{maxMoveDistance} is often referred to as 'yarding' distance. Smaller
#' values for \emph{maxMoveDistance} and \emph{travelThreshold} result in fewer
#' 'from' and 'to' locations included in the summary
#'
#' \emph{fromName} must represent locations moved to the rasterized road network
#' by \emph{movePt2RdSegment()}
#'
#' @return list with two elements; a data frame comprising 'from' locations IDs,
#' the closest, travel time or distance to 'to' location IDs, and the
#' corresponding travel time, in hours, or distance, in kilometers. The second
#' list element, also a data frame, with 'to' location IDs, the number of 'from'
#' locations for which the 'to' location is closest in travel time or distance,
#' and the mean and standard deviation for the travel cost (time) for those
#' 'from' locations
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' fromPath <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#' toPath <- system.file( "extdata", "mills.gpkg", package="tRee2MillCost" )
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- createGraph( "road_net.tif", 1 )
#' movePt2RdSegment( "road_net.tif", fromPath, "movedFromLocations.gpkg" )
#' movePt2RdSegment( "road_net.tif", toPath, "movedToLocations.gpkg" )
#' cost.df <- calculateCost( graph, "road_net.tif",
#'                           "movedFromLocations.gpkg", "sampleID",
#'                           "movedToLocations.gpkg", "COMPANY_NAME" )
#' summary.list <- costSummary( cost.df,
#'                              fromName="movedFromLocations.gpkg",
#'                              fromField="sampleID",
#'                              travelThreshold=0.5 ) #30 minutes
#' print( summary.list[[1]] )
#' print( summary.list[[2]] )
#' }
#'
#' @seealso [calculateCost] and [movePt2RdSegment]
#'
#' @export
costSummary <- function( costDF, fromName, fromField, maxMoveDistance="", travelThreshold="" ) {
  #df <- read.csv(costName, header=T, check.names=FALSE)
  toField <- names(costDF)[1]

  if( !file.exists(fromName) )
    stop( "\ncostSummary:", fromName, " does not exist\n\n" )

  s <- terra::vect( fromName ) # must contain fromField and MOVEDIST
  unit <- terra::linearUnits( s )
  if( unit == 1 ) {
    unit.txt <- "m"
  } else {
    unit.txt <- "ft"
  }

  if( fromField %in% names(s) == FALSE )
    stop( "\ncostSummary(): Field ", fromField, " is not present in ", fromName, "\n\n" )
  if( "MOVEDIST" %in% names(s) == FALSE )
    stop( "\ncostSummary(): MOVEDIST field required but missing from ", fromName, "\n\n" )

  if( !missing(maxMoveDistance) & maxMoveDistance != "" ) {
    if( !is.numeric(maxMoveDistance) )
      stop( "\ncostSummary(): maxMoveDistance must be numeric. ", maxMoveDistance, "was specified\n\n" )
    if( maxMoveDistance <= 0 )
      stop( "\ncostSummary(): maxMoveDistance must be positive. ", maxMoveDistance, " was specified\n\n" )
    from.df <- data.frame(s)
    fromID <- from.df[from.df$MOVEDIST > maxMoveDistance, fromField]
    fromRemoved <- length( fromID )
    if( fromRemoved == (ncol(costDF) - 1) ) {
      cat( "\ncostSummary(): All records in", fromName, "have MOVEDIST greater than", maxMoveDistance, "\n\n" )
      return( invisible(NULL) )
    }
    if( fromRemoved > 0 )
      cat("\n", fromRemoved, "'from' locations had move distance greater than", maxMoveDistance, unit.txt, "and were excluded\n\n")
    costDF[, fromID] <- NULL
  }

  if( !missing(travelThreshold) | travelThreshold != "") {
    w <- costDF[, 2:ncol(costDF)] > travelThreshold
    costDF[, 2:ncol(costDF)][w] <- NA
  }

  if( ncol(costDF) == 2 )
    return( list( costDF, NULL ) )

  minCostTo <- costDF[,1][apply(costDF[,2:ncol(costDF)], 2, function(x) { if(all(is.na(x))) NA else which.min(x) } )]
  minCost <- apply(costDF[,2:ncol(costDF)], 2, function(x) { if(all(is.na(x))) NA else min(x, na.rm=T) } )
  minCost.df <- data.frame( names(costDF)[2:ncol(costDF)], minCostTo, minCost )
  names( minCost.df ) <- c( fromField, toField, "MINCOST" )
  minCost.df <- minCost.df[complete.cases(minCost.df),]
  row.names( minCost.df ) <- NULL
  if( nrow( minCost.df ) == 0 ) {
    cat( "\nNo 'from'/'to' pairs much the summary criteria specified\n" )
    return( invisible(NULL) )
  }

  nFromPerTo <- tapply(minCost.df[, fromField], minCost.df[, names(costDF)[1]], length )
  meanCostPerTo <- round( tapply(minCost.df$MINCOST, minCost.df[, toField], mean, na.rm=T ), 3 )
  medianCostPerTo <- round( tapply(minCost.df$MINCOST, minCost.df[, toField], median, na.rm=T ), 3 )
  sdCostPerTo <- round( tapply(minCost.df$MINCOST, minCost.df[, toField], sd, na.rm=T ), 3 )
  costSummary.df <- data.frame( names(nFromPerTo), nFromPerTo, meanCostPerTo, medianCostPerTo, sdCostPerTo )
  row.names( costSummary.df ) <- NULL
  names( costSummary.df ) = c( toField, "FROMCOUNT", "COSTMEAN", "COSTMEDIAN", "COSTSD" )

  return( list( minCost.df, costSummary.df ) )
}

#' @title delineate routes between two sets of locations
#'
#' @description Uses a road raster and a graph created from the raster to
#' delineate the routes between two sets of locations present in the graph
#'
#' @param graph object generated by \emph{createGraph()}
#' @param roadRasterName character, filename/path of the road raster
#' @param fromName character, filename/path of a point vector layer
#' @param fromField character, the name of a field with point IDs present in
#' \emph{fromName}
#' @param fromIDs character, vector of selected point IDs, present in
#' \emph{fromField}, representing path origins
#' @param toName character, filename/path of a point vector layer
#' @param toField character, the name of a field with point IDs present in
#' \emph{toName}
#' @param toIDs character, vector of selected point IDs, present in
#' \emph{toField}, representing path destinations
#'
#' @details Originally designed to identify routes from a collection of points
#' to a single destination point, as from sample locations (forest inventory
#' plots) to a wood processing facility. It can also be used with a set of
#' destination points. The routes identified carry no attributes
#'
#' \emph{fromIDs} and \emph{toIDs} for locations of interest can be determined
#' by querying the road raster or the graph. All IDs must be present in the
#' graph. If \emph{cppRouting::cpp_simplify()} has been applied to the
#' \emph{graph} object, all \emph{fromIDs} and \emph{toIDs} must have been kept.
#' If not kept, the paths delineated will deviate from the original vector road
#' layer used to great the graph. To maintain path detail, a non-optimized
#' version of the \emph{graph} should be used at the cost of longer processing
#' time
#'
#' @examples
#' \dontrun{
#' rdPath <- system.file( "extdata", "roads.gpkg", package="tRee2MillCost" )
#' graphPath <- system.file( "extdata", "graph_20m_time.rds", package="tRee2MillCost" )
#' toPath <- system.file( "extdata", "mills.gpkg", package="tRee2MillCost" )
#' fromPath <- system.file( "extdata", "sample.gpkg", package="tRee2MillCost" )
#'
#' rasterizeRoads( rdPath, 20, "road_net.tif", "MPH" )
#' graph <- readRDS( graphPath )
#' movePt2RdSegment( "road_net.tif", toPath, "movedToLocations.gpkg" )
#' movePt2RdSegment( "road_net.tif", fromPath, "movedFromLocations.gpkg" )
#'
#' r <- terra::rast( "road_net.tif" )
#'
#' from.v <- terra::vect( "movedFromLocations.gpkg" )
#' #randomly select 5 'from' points
#' from.v <- from.v[sample( 1:length(from.v), 5, replace=FALSE )]
#' fromIDs <- from.v$sampleID
#'
#' to.v <- terra::vect( "movedToLocations.gpkg" )
#' #randomly select 1 'to' point
#' to.v <- to.v[sample( 1:length(to.v), 1 )]
#' toIDs <- to.v$COMPANY_NAME
#
#' paths <- fromToPaths( graph, "road_net.tif",
#'                       "movedFromLocations.gpkg", "sampleID", fromIDs,
#'                       "movedToLocations.gpkg", "COMPANY_NAME", toIDs )
#' terra::plot( paths )
#' terra::points( from.v, pch=16, col="red", cex=1.5 )
#' terra::points( to.v, pch=16, col="blue", cex=1.5 )
#' }
#'
#' @return spatVect line object
#'
#' @seealso [costSummary], [cppRouting::cpp_simplify], and [calculateCost]
#'
#' @export
fromToPaths <- function( graph, roadRasterName, fromName, fromField, fromIDs, toName, toField, toIDs ) {
  to.v <- terra::vect( toName )
  to.df <- cbind( data.frame(to.v), xcoord=terra::crds(to.v)[,1], ycoord=terra::crds(to.v)[,2] )
  ids.df <- data.frame( toIDs )
  names( ids.df ) <- toField
  to.df <- merge( to.df, ids.df, by=toField )
  to.df$cellIDs <- terra::cellFromXY( terra::rast(roadRasterName),
                                      matrix(c(to.df$xcoord, to.df$ycoord), ncol=2) )
  from.v <- terra::vect( fromName )
  from.df <- cbind( data.frame(from.v), xcoord=terra::crds(from.v)[,1], ycoord=terra::crds(from.v)[,2] )
  ids.df <- data.frame( fromIDs )
  names( ids.df ) <- fromField
  from.df <- merge( from.df, ids.df, by=fromField )
  from.df$cellIDs <- terra::cellFromXY( terra::rast(roadRasterName),
                                        matrix(c(from.df$xcoord, from.df$ycoord), ncol=2) )
  RcppParallel::setThreadOptions( parallel::detectCores() )
  paths <- cppRouting::get_multi_paths( graph,
                                        from=from.df$cellIDs,
                                        to=to.df$cellIDs,
                                        long=TRUE )
  paths[, c("x", "y")] <- terra::xyFromCell( terra::rast(roadRasterName), as.numeric(paths$node) )

  rangeX <- range( paths$x )
  rangeY <- range( paths$y )
  deltaX <- rangeX[2] - rangeX[1]
  deltaY <- rangeY[2] - rangeY[1]
  if( nrow(graph$coords) > 1E6 ) {
    s = sample( 1:nrow(graph$coords), size=1E6, replace=FALSE )
    res = min(RANN::nn2(graph$coords[s, 2:3], k=2)[[2]][,2])
  } else {
    res = min(RANN::nn2(graph$coords[, 2:3], k=2)[[2]][,2])
  }

  ext <- terra::ext( c( floor(rangeX[1]/res)*res, ceiling(rangeX[2]/res)*res,
                        floor(rangeY[1]/res)*res, ceiling(rangeY[2]/res)*res ) )
  r <- terra::rast( ext, res=res, crs=terra::crs(to.v) )
  r <- terra::rasterize( as.matrix(paths[, c("x", "y")]), r, values=1 )

  return( terra::as.lines( r, na.rm=TRUE ) )
}
