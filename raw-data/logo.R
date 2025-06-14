library("grid")
library("gridpattern")
library("piecepackr")
library("polyclip")

draw_logo <- function(bleed = FALSE, cut = FALSE) {
x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))

hex_xy <- list(x = x_hex, y = y_hex)
bot_xy <- list(x = c(0, 0, 1, 1), y = 0/4 + c(0, 1/4, 1/4, 0))
low_xy <- list(x = c(0, 0, 1, 1), y = 1/4 + c(0, 1/4, 1/4, 0))
hih_xy <- list(x = c(0, 0, 1, 1), y = 2/4 + c(0, 1/4, 1/4, 0))
top_xy <- list(x = c(0, 0, 1, 1), y = 3/4 + c(0, 1/4, 1/4, 0))
if (bleed) {
    bot_xy$y <- bot_xy$y + c(0, 1, 1, 0) / 8
    top_xy$y <- top_xy$y - c(1, 0, 0, 1) / 8
}

bd_bot <- polyclip(hex_xy, bot_xy, "intersection")[[1]]
bd_low <- polyclip(hex_xy, low_xy, "intersection")[[1]]
bd_hih <- polyclip(hex_xy, hih_xy, "intersection")[[1]]
bd_top <- polyclip(hex_xy, top_xy, "intersection")[[1]]

# colorblind accessible scheme https://jfly.uni-koeln.de/color/
blue <- grDevices::rgb(0.35, 0.70, 0.90)
yellow <- grDevices::rgb(0.95, 0.90, 0.25)
red <- grDevices::rgb(0.80, 0.40, 0.00)
green <- grDevices::rgb(0.00, 0.60, 0.50)
orange <- grDevices::rgb(0.90, 0.60, 0.00)

w <- 4.5

grid.newpage()
gp <- gpar(fill = yellow, col = "black")
# grid.polygon(bd_bot$x, bd_bot$y, gp = gpar(fill = "white", col = NA))
grid.pattern_weave(bd_bot$x, bd_bot$y, fill2 = blue,
                   type = "satin", density=0.3, angle = 45, gp=gp)
gp <- gpar(fill = c(yellow, orange, red), col = "black")
grid.pattern_regular_polygon(bd_top$x, bd_top$y, shape = "convex3", density = 1.33,
                             grid = "hex_circle", gp = gp,
                             spacing = 0.05, rot = 30, angle = 0)
pushViewport(viewport(height = unit(w, "inches")))
gp <- gpar(fill = c(yellow, blue), col = "black")
grid.pattern_regular_polygon(bd_low$x, bd_low$y, shape = "square",
                             density = 1, angle = 0, spacing=0.125, gp = gp)
gp <- gpar(fill = c(yellow, orange, red), col = "black")
grid.pattern_regular_polygon(bd_hih$x, bd_hih$y, shape = "convex6",
                             density = 1, angle = 0, grid = "hex", spacing=0.175, gp = gp,
                             yoffset = -0.03, xoffset = -0.01)
# grid.polygon(bd_top$x, bd_top$y, gp = gpar(fill = "white", col = NA))
popViewport()

pushViewport(viewport(width=unit(w, "inches"), height = unit(w, "inches")))
gp = gpar(col = "black", fontsize = 50, fontfamily = "sans", fontface = "bold")
yoffset <- 0.002
grid.text("g", x=0.23, y=0.625 + yoffset, gp = gp)
grid.text("r", x=0.40, y=0.625 + yoffset, gp = gp)
grid.text("i", x=0.58, y=0.625 + yoffset, gp = gp)
grid.text("d", x=0.75, y=0.625 + yoffset, gp = gp)

xr <- range(x_hex)
step <- (xr[2] - xr[1]) / 7
x <- seq(xr[1] + step / 2, by = step, length.out = 7)
yoffset <- -0.001
gp = gpar(col = "black", fontsize = 48, fontfamily = "sans", fontface = "bold")
grid.text("p", x=x[1], y=0.375 + yoffset, gp = gp)
grid.text("a", x=x[2], y=0.375 + yoffset, gp = gp)
grid.text("t", x=x[3], y=0.375 + yoffset, gp = gp)
grid.text("t", x=x[4], y=0.375 + yoffset, gp = gp)
grid.text("e", x=x[5], y=0.375 + yoffset, gp = gp)
grid.text("r", x=x[6], y=0.375 + yoffset, gp = gp)
grid.text("n", x=x[7], y=0.375 + yoffset, gp = gp)
popViewport()

if (!isTRUE(bleed) || isTRUE(cut)) { 
    pushViewport(viewport(width=unit(w, "inches"), height = unit(w, "inches")))
    hex <- pp_shape("convex6")
    grid.draw(hex$shape(gp = gpar(fill = NA, col = "white", lwd=4)))
    if(!isTRUE(bleed)) {
        grid.draw(hex$mat(mat_width = 0.01, gp = gpar(fill = "black", col = NA)))
    }
    popViewport()
    if (isTRUE(bleed)) {
        pushViewport(viewport(width=unit(5/6, "npc"), height=unit(5/6, "npc")))
        grid.draw(hex$shape(gp = gpar(fill="transparent", col="orange")))
        popViewport()
    }
}
}

# svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
# draw_logo()
# dev.off()
# 
# png("man/figures/logo.png", width = w, height = w, units = "in", res = 72, bg = "transparent")
# draw_logo()
# dev.off()

png("raw-data/sticker_with_cutline.png", width = 5.125, height = 5.125, units = "in", res = 150, bg = "white")
draw_logo(bleed = TRUE, cut = TRUE)
dev.off()

png("raw-data/sticker.png", width = 5.125, height = 5.125, units = "in", res = 150, bg = "white")
draw_logo(bleed = TRUE, cut = FALSE)
dev.off()
