# c() and append() don't directly work with grid::gList() lists
append_gList <- function(gl, grob) {
    gl[[length(gl) + 1L]] <- grob
    gl
}

# get width, height, length, and center cooordinates of the viewport in `units` units
get_vp_measurements <- function(units = "bigpts") {
    width <- as.numeric(convertWidth(unit(1, "npc"), units))
    height <- as.numeric(convertHeight(unit(1, "npc"), units))
    length <- max(width, height)
    x <- as.numeric(convertX(unit(0.5, "npc"), units))
    y <- as.numeric(convertY(unit(0.5, "npc"), units))
    list(width = width, height = height, length = length, x = x, y = y)
}
