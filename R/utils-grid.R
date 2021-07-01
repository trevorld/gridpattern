# c() and append() don't directly work with grid::gList() lists
append_gList <- function(gl, grob) {
    gl[[length(gl) + 1L]] <- grob
    gl
}

# get width, height, length, and center cooordinates of the viewport in `units` units
get_vp_measurements <- function(units = "bigpts") {
    width <- convertWidth(unit(1, "npc"), units, valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), units, valueOnly = TRUE)
    length <- max(width, height)
    x <- convertX(unit(0.5, "npc"), units, valueOnly = TRUE)
    y <- convertY(unit(0.5, "npc"), units, valueOnly = TRUE)
    list(width = width, height = height, length = length, x = x, y = y)
}
