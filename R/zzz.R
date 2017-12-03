
.onLoad <- function(libname, pkgname) {
  version  <- utils::packageDescription("labgeo")$Version
  v <- utils::packageDescription("labgeo")$Built
  data_compilacao <- unlist(strsplit(as.character(v), ";"))
  #data_compilacao = unlist(base::strsplit(v, "[;]"))
  dc <- data_compilacao[3]
  base::packageStartupMessage(paste('library labgeo', ' - version ',version ))
  base::packageStartupMessage(paste('Built:', dc))
}
