#' predict to map
#'
#' This function predict a model fit and save result as a map raster file
#'
#'
#' @param model_list list of fit models by function run_models
#' @param path_rastesr path of rasters with co-variates
#' @param raster_type extension of raster files with co-variates
#' @param path_result path to store maps with results of prediction
#' @param namefile sufix of name of file with results of prediction
#' @param result_type type of file (extension) to be saved after prediction
#' @keywords predict models maps raster
#' @details  details
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' predict_to_map(fit.models,
#'               path_raster = './covar_asc/',
#'               raster_type = ".asc",
#'               path_result = './result/',
#'               namefile = 'uso_',
#'               result_type = ".tif")
#' }
#' @export



predict_to_map <- function(model_list, path_raster, raster_type = ".asc",
                           path_result, namefile, result_type= ".tif") {
  nm = length(model_list)
  i = 1
  for (i in 1:nm) {
    inicio = Sys.time()
    var_file_raster = paste0(path_raster,vsel,raster_type)
    st <- raster::stack(var_file_raster)
    vachei = !(names(st) %in% fit[[i]]$coefnames)
    if (sum(vachei) > 0) {
      print(paste(names(st)[vachei]))
      stop("covariate no found")
    }
    name_model = model_list[[i]]$modelInfo$label
    filename = gsub(" ", "_",paste0(path_result, namefile, name_model,typefile))
    print(paste("model : ", name_model,"file name", filename))
    raster::predict(object = st, model = model_list[[i]], filename = filename, overwrite=TRUE)
    print(paste( "time prediction" , hms_span(inicio, Sys.time())))
  }
}
