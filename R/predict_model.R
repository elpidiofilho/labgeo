#' predict to map
#'
#' This function predict a model fit and save result as a map raster file
#'
#'
#' @param model_list list of fit models by function run_models
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
  .outcome <- NULL
  nm <- length(model_list)

  for (i in 1:nm) {
    inicio <- Sys.time()
    df_entrada <- model_list[[i]]$trainingData %>% select( -(.outcome))

    vsel <- names(df_entrada)
    var_file_raster <- paste0(path_raster, vsel, result_type)
    st <- raster::stack(var_file_raster)
    vachei <- !(names(st) %in% vsel)
    if (sum(vachei) > 0) {
      print(paste(names(st)[vachei]))
      stop("covariate not found")
    }
    name_model <- model_list[[i]]$method
    filename <- gsub(" ", "_", paste0(
      path_result, namefile,
      name_model, result_type
    ))
    print(paste("model : ", name_model, "file name", filename))
    raster::predict(
      object = st, model = model_list[[i]],
      filename = filename, overwrite = TRUE
    )
    print(paste("time prediction", hms_span(inicio, Sys.time())))
  }
}



#' predict stack to map
#'
#' This function predict a model fit and save result as a map raster file
#'
#'
#' @param model_list list of fit models by function run_models
#' @param path_raster path of rasters with co-variates
#' @param raster_type extension of raster files with co-variates
#' @param path_result path to store maps with results of prediction
#' @param namefile sufix of name of file with results of prediction
#' @param result_type type of file (extension) to be saved after prediction
#' @keywords predict models maps raster
#' @details  details
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' predict_stack_to_map(fit.models,
#'               path_raster = './raster/',
#'               result_type = '.asc',
#'               path_result = './result/',
#'               namefile = 'uso_',
#'               result_type = ".tif")
#' }
#' @export

predict_stack_to_map <- function(model_list, path_raster, raster_type = ".asc",
                                 path_result, namefile, result_type= ".tif") {
  .outcome <- NULL
  nm <- length(model_list)
  for (i in 1:nm) {
    inicio <- Sys.time()
    df_entrada <- model_list[[i]]$trainingData %>% select( -(.outcome))
    vsel <- names(df_entrada)
    var_file_raster <- paste0(path_raster, vsel, result_type)
    stack_var <- raster::stack(var_file_raster)
    vachei <- !(vsel %in% names(stack_var))
    if (sum(vachei) > 0) {
      print(paste(names(stack_var)[vachei]))
      stop("covariate not found")
    }
    name_model <- model_list[[i]]$modelInfo$label
    filename <- gsub(" ", "_", paste0(
      path_result, namefile,
      name_model, result_type
    ))
    print(paste("model : ", name_model, "file name", filename))
    raster::predict(
      object = stack_var, model = model_list[[i]], na.rm = TRUE,
      progress = "text",
      filename = filename, overwrite = TRUE
    )
    print(paste("time prediction", hms_span(inicio, Sys.time())))
    raster::endCluster()
  }
}




#' predict stack to map parallel
#'
#' This function predict a model fit and save result as a map raster file in paralllel processing
#'
#'
#' @param model_list list of fit models by function run_models
#' @param cou_cores number of cpu_cores
#' @param stack_var Stack with all vars used to fit them model
#' @param path_result path to store maps with results of prediction
#' @param namefile sufix of name of file with results of prediction
#' @param result_type type of file (extension) to be saved after prediction
#' @keywords predict models maps raster
#' @details  details
#' @author Elpidio Filho, \email{elpidio@ufv.br}
#' @examples
#' \dontrun{
#' predict_stack_to_map_paralllel(fit.models,
#'               cpu_cores = 2,
#'               stack_var = predicao,
#'               path_result = './result/',
#'               namefile = 'uso_',
#'               result_type = ".tif")
#' }
#' @export

predict_stack_to_map_parallel <- function(model_list, cpu_cores = 2, stack_var,
                                 path_result, namefile, result_type= ".tif") {
  .outcome <- NULL
  nm <- length(model_list)
  for (i in 1:nm) {
    inicio <- Sys.time()
    df_entrada <- model_list[[i]]$trainingData %>% select(-.outcome)

    vsel <- names(df_entrada)
    vachei <- !(vsel %in% names(stack_var))
    if (sum(vachei) > 0) {
      print(paste(names(stack_var)[vachei]))
      stop("covariate not found")
    }
    name_model <- model_list[[i]]$modelInfo$label
    filename <- gsub(" ", "_", paste0(
      path_result, namefile,
      name_model, result_type
    ))
    print(paste("model : ", name_model, "file name", filename))
    raster::beginCluster(cpu_cores)
    raster::clusterR(stack_var, raster::predict,
      args = list(model_list[[i]]), na.rm = TRUE,
      progress = TRUE,
      filename = filename, overwrite = TRUE
    )
    raster::endCluster()
    print(paste("time prediction", hms_span(inicio, Sys.time())))
  }
}
