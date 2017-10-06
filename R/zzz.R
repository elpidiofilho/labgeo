
.onLoad <- function(libname, pkgname) {
  version  = packageDescription("labgeo")$Version
  v = packageDescription("labgeo")$Built
  data_compilacao = unlist(strsplit(as.character(v), ";"))
  #data_compilacao = unlist(base::strsplit(v, "[;]"))
  dc = data_compilacao[3]
  packageStartupMessage(paste('library labgeo', ' - version ',version ))
  packageStartupMessage(paste('Built:', dc))
}
