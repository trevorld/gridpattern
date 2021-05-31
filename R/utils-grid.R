# c() and append() don't directly work with grid::gList() lists
append_gList <- function(gl, grob) {
    gl[[length(gl) + 1L]] <- grob
    gl
}
