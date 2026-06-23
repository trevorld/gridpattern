#' Heraldic color hatching patterned grobs
#'
#' `grid.pattern_hatch()` draws a heraldic color hatching patterns onto the graphic device.
#' `names_hatch()` returns a character vector of supported `type` values.
#'
#' @inheritParams grid.pattern_line
#' @param type A tincture or color name.  `names_hatch()` lists supported values.
#'   Both traditional tincture names (e.g. `"gules"`) and modern color equivalents
#'   (e.g. `"red"`) are accepted.  Matching is case-insensitive and ignores hyphens
#'   and spaces.
#' @param subtype A string with one of
#'
#'  * `"combinatorial"` (default): an extension of the seven standard Petra Sancta hatchings with systematically derived mixed-color hatchings.
#'  * `"fox-davies"`: the hatchings in Fox-Davies' *A Complete Guide to Heraldry*.
#'  * `"goodman"`: the hatchings in David Goodman's *Heraldic Tincture* reference.
#'  * `"unicode"`: the hatchings used in Unicode character charts.
#'
#'   The string is case-insensitive and hyphens and spaces are ignored.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_hatch(x_hex, y_hex, type = "azure", colour = "blue")
#' }
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_hatch(x_hex, y_hex, type = "cendree", colour = "grey", spacing = 0.1)
#' }
#' print(names_hatch())
#' print(names_hatch("fox-davies"))
#' print(names_hatch("goodman"))
#' print(names_hatch("unicode"))
#' @seealso [grid.pattern_line()] for single-direction lines,
#'   [grid.pattern_crosshatch()] for perpendicular lines.
#'   `vignette("hatching", package = "gridpattern")` for a visual overview of all subtypes.
#'   <https://en.wikisource.org/wiki/A_Complete_Guide_to_Heraldry/Chapter_7#74> for Fox-Davies'
#'   _A Complete Guide to Heraldry_.
#'   <https://david.goodman.graphics/portfolio/item/crests-heraldry-and-coats-of-arms/> for
#'   David Goodman's Heraldic Tincture reference.
#' @export
grid.pattern_hatch <- function(
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	type = "gules",
	subtype = "combinatorial",
	colour = gp$col %||% "grey20",
	spacing = 0.05,
	xoffset = 0,
	yoffset = 0,
	units = "snpc",
	alpha = gp$alpha %||% NA_real_,
	lineend = gp$lineend %||% "round",
	linewidth = size %||% gp$lwd %||% 1,
	size = NULL,
	use_R4.1_masks = getOption(
		"ggpattern_use_R4.1_masks",
		getOption("ggpattern_use_R4.1_features")
	),
	png_device = NULL,
	res = getOption("ggpattern_res", 72),
	default.units = "npc",
	name = NULL,
	gp = gpar(),
	draw = TRUE,
	vp = NULL
) {
	if (missing(colour) && hasName(l <- list(...), "color")) {
		colour <- l$color
	}
	grid.pattern(
		"hatch",
		x,
		y,
		id,
		colour = colour,
		type = type,
		subtype = subtype,
		spacing = spacing,
		xoffset = xoffset,
		yoffset = yoffset,
		units = units,
		alpha = alpha,
		lineend = lineend,
		linewidth = linewidth,
		use_R4.1_masks = use_R4.1_masks,
		png_device = png_device,
		res = res,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

#' @rdname grid.pattern_hatch
#' @param accent If `TRUE`, return tincture names using their traditional
#'   accented spellings where applicable (e.g. `"tenn\u00e9"`, `"brun\u00e2tre"`).
#'   Defaults to `FALSE`.
#' @export
names_hatch <- function(
	subtype = c("combinatorial", "fox-davies", "goodman", "unicode"),
	accent = FALSE
) {
	subtype <- tolower(subtype)
	subtype <- match.arg(subtype)
	nms <- if (subtype == "fox-davies") {
		sort(names(HATCH_FOX_DAVIES))
	} else if (subtype == "unicode") {
		sort(unname(HATCH_COLORS[names(HATCH_UNICODE)]))
	} else if (subtype == "goodman") {
		sort(names(HATCH_GOODMAN))
	} else {
		sort(unname(HATCH_COLORS[names(HATCH_COMBINATORIAL)]))
	}
	if (accent) {
		accented <- HATCH_ACCENTS[nms]
		nms[!is.na(accented)] <- accented[!is.na(accented)]
	}
	nms
}

# Accented spellings for tincture names that have them.
HATCH_ACCENTS <- c(
	`bleu celeste` = "bleu c\u00e9leste",
	cendree = "cendr\u00e9e",
	tenne = "tenn\u00e9",
	brunatre = "brun\u00e2tre"
)

# Internal: hatching specs per subtype.
# Each entry is one of:
#   list(special = "plain")   -- no pattern (argent)
#   list(special = "solid")   -- solid colour fill (unicode sable)
#   list(angles, linetypes)   -- one or two sets of stroked lines

# Combinatorial Petra Sancta: systematically extends the seven standard tinctures.
# Three derivation rules:
#   white + color  → dashed lines at that color's angle     (lightened tints)
#   yellow + color → dotdash lines at that color's angle    (Munsell YR and GY secondaries)
#   color + color  → crossing solid lines at both angles    (Munsell BG, PB, RP secondaries + others)
HATCH_COMBINATORIAL <- list(
	# === Base seven tinctures (standard Petra Sancta) ===
	argent = list(special = "plain"),
	or = list(special = "circle"),
	azure = list(angles = 0, linetypes = "solid"),
	gules = list(angles = 90, linetypes = "solid"),
	sable = list(angles = c(0, 90), linetypes = c("solid", "solid")),
	vert = list(angles = 135, linetypes = "solid"),
	purpure = list(angles = 45, linetypes = "solid"),
	# === White + color: dashed lines ===
	`bleu celeste` = list(angles = 0, linetypes = "dashed"), # argent + azure
	carnation = list(angles = 90, linetypes = "dashed"), # argent + gules
	cendree = list(special = "cendree"), # argent + sable
	mint = list(angles = 135, linetypes = "dashed"), # argent + vert (new)
	lavender = list(angles = 45, linetypes = "dashed"), # argent + purpure (new)
	# === Yellow + color: dotdash lines ===
	orange = list(angles = 90, linetypes = "dotdash"), # or + gules (YR)
	lime = list(angles = 135, linetypes = "dotdash"), # or + vert (GY)
	olive = list(special = "olive"), # or + sable: alternating circles and plus signs
	rose = list(angles = 45, linetypes = "dotdash"), # or + purpure (YP)
	# === Color × color: crossing lines ===
	brunatre = list(
		angles = c(90, 0, 135),
		linetypes = c("solid", "solid", "solid"),
		spacings = c(1, 1, 1 / sqrt(2))
	), # gules × azure × vert
	eisenfarbe = list(angles = c(45, 135), linetypes = c("solid", "solid")), # purpure × vert
	sanguine = list(angles = c(90, 45), linetypes = c("solid", "solid")), # gules × purpure (RP)
	tenne = list(angles = c(90, 135), linetypes = c("solid", "solid")), # gules × vert
	teal = list(angles = c(0, 135), linetypes = c("solid", "solid")), # azure × vert (BG, new)
	violet = list(angles = c(0, 45), linetypes = c("solid", "solid")) # azure × purpure (PB, new)
)

# Fox-Davies is the standard Petra Sancta tinctures plus the German heraldry extensions,
# all of which are defined in HATCH_COMBINATORIAL.
HATCH_FOX_DAVIES <- c(
	HATCH_COMBINATORIAL[c(
		"argent",
		"or",
		"azure",
		"gules",
		"sable",
		"vert",
		"purpure",
		"eisenfarbe",
		"sanguine",
		"tenne",
		"brunatre",
		"carnation",
		"cendree",
		"orange",
		"bleu celeste"
	)],
	list(proper = list(special = "proper"))
)

# Goodman system (david.goodman.graphics v2.0 2024-02-14).
# Shares most specs with fox-davies; key differences:
#   murrey   = eisenfarbe spec (45° + 135°) — not fox-davies sanguine
#   sanguine = teal spec (0° + 135°)        — different from fox-davies sanguine (90° + 45°)
#   rose     = carnation spec (90° dashed)  — rose and carnation share one hatching
# Rare metals (steel, copper, bronze, lead) are unique to this system.
HATCH_GOODMAN <- c(
	HATCH_COMBINATORIAL[c(
		"argent",
		"or",
		"azure",
		"gules",
		"sable",
		"vert",
		"purpure",
		"cendree",
		"brunatre",
		"bleu celeste",
		"carnation",
		"orange",
		"tenne"
	)],
	list(
		murrey = HATCH_COMBINATORIAL[["eisenfarbe"]], # 45° + 135° (dark red-purple)
		sanguine = HATCH_COMBINATORIAL[["teal"]], # 0° + 135° (blood red)
		rose = HATCH_COMBINATORIAL[["carnation"]], # 90° dashed (rose/carnation share hatching)
		steel = list(special = "steel"), # + signs in square grid
		copper = list(special = "c_hex"), # c in hex grid
		bronze = list(special = "c_hex"), # c in hex grid
		lead = list(special = "c_hex") # c in hex grid
	)
)

HATCH_UNICODE <- c(
	HATCH_COMBINATORIAL[c("argent", "or", "azure", "gules", "vert", "purpure", "tenne")],
	list(
		`bleu celeste` = list(angles = 0, linetypes = "dotdash"),
		carnation = HATCH_COMBINATORIAL[["rose"]], # unicode "pink" heart uses 45° dotdash
		cendree = list(special = "checker"),
		orange = list(angles = c(0, 45), linetypes = c("solid", "solid")),
		sable = list(special = "solid")
	)
)

# Modern color equivalents for tincture names.
# For the unicode subtype, values match the canonical Unicode emoji color names.
HATCH_COLORS <- c(
	`bleu celeste` = "light blue",
	argent = "white",
	azure = "blue",
	bronze = "bronze",
	brunatre = "umbre",
	carnation = "pink",
	cendree = "grey",
	copper = "copper",
	eisenfarbe = "slate",
	gules = "red",
	lavender = "lavender",
	lead = "lead grey",
	lime = "lime green",
	mint = "mint green",
	murrey = "mulberry",
	olive = "olive",
	or = "yellow",
	orange = "orange",
	proper = "color of nature",
	purpure = "purple",
	rose = "rose",
	sable = "black",
	sanguine = "magenta",
	steel = "steel grey",
	teal = "teal",
	tenne = "brown",
	vert = "green",
	violet = "violet"
)


# Map from normalized user input to canonical tincture name.
# Normalization: tolower() + remove hyphens and spaces.
HATCH_NAME_MAP <- c(
	argent = "argent",
	white = "argent",
	silver = "argent",
	azure = "azure",
	blue = "azure",
	cobalt = "azure",
	bleuceleste = "bleu celeste",
	"bleuc\u00e9leste" = "bleu celeste",
	skyblue = "bleu celeste",
	lightblue = "bleu celeste",
	celeste = "bleu celeste",
	"c\u00e9leste" = "bleu celeste",
	ciel = "bleu celeste",
	bronze = "bronze",
	brunatre = "brunatre",
	"brun\u00e2tre" = "brunatre",
	darkbrown = "brunatre",
	umbre = "brunatre",
	umber = "brunatre",
	carnation = "carnation",
	flesh = "carnation",
	pink = "carnation",
	cendree = "cendree",
	"cendr\u00e9e" = "cendree",
	grey = "cendree",
	gray = "cendree",
	ash = "cendree",
	ashgray = "cendree",
	ashgrey = "cendree",
	copper = "copper",
	slate = "eisenfarbe",
	irongrey = "eisenfarbe",
	irongray = "eisenfarbe",
	iron = "eisenfarbe",
	eisenfarbe = "eisenfarbe",
	gules = "gules",
	red = "gules",
	crimson = "gules",
	lavender = "lavender",
	lilac = "lavender",
	lead = "lead",
	leadgrey = "lead",
	leadgray = "lead",
	lime = "lime",
	chartreuse = "lime",
	limegreen = "lime",
	yellowgreen = "lime",
	mint = "mint",
	mintgreen = "mint",
	seafoam = "mint",
	murrey = "murrey",
	mulberry = "murrey",
	olive = "olive",
	or = "or",
	yellow = "or",
	gold = "or",
	orange = "orange",
	colorofnature = "proper",
	proper = "proper",
	natural = "proper",
	purpure = "purpure",
	purple = "purpure",
	rose = "rose",
	sable = "sable",
	black = "sable",
	sanguine = "sanguine",
	bloodred = "sanguine",
	magenta = "sanguine",
	steel = "steel",
	steelgrey = "steel",
	steelgray = "steel",
	teal = "teal",
	cyan = "teal",
	aqua = "teal",
	aquamarine = "teal",
	turquoise = "teal",
	bluegreen = "teal",
	tenne = "tenne",
	"tenn\u00e9" = "tenne",
	tawny = "tenne",
	brown = "tenne",
	vert = "vert",
	green = "vert",
	violet = "violet",
	bluepurple = "violet",
	violetblue = "violet"
)

create_pattern_hatch <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	type_norm <- gsub("[- ]", "", tolower(params$pattern_type))
	subtype <- params$pattern_subtype
	if (is.null(subtype) || is.na(subtype)) {
		subtype <- "combinatorial"
	}
	subtype_norm <- gsub("[- ]", "", tolower(subtype))
	is_unicode <- subtype_norm == "unicode"
	is_fox_davies <- subtype_norm == "foxdavies"
	is_goodman <- subtype_norm == "goodman"
	canonical <- HATCH_NAME_MAP[type_norm]
	if (is.na(canonical)) {
		stop("Unknown hatching type '", params$pattern_type, "'.")
	}

	spec <- if (is_unicode) {
		HATCH_UNICODE[[canonical]]
	} else if (is_fox_davies) {
		HATCH_FOX_DAVIES[[canonical]]
	} else if (is_goodman) {
		HATCH_GOODMAN[[canonical]]
	} else {
		HATCH_COMBINATORIAL[[canonical]]
	}

	if (is.null(spec)) {
		stop(
			"Hatching type '",
			params$pattern_type,
			"' is not supported ",
			"by the '",
			subtype,
			"' subtype."
		)
	}

	if (!is.null(spec$special)) {
		if (spec$special == "plain") {
			return(grid::nullGrob())
		}
		if (spec$special == "solid") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			default.units <- "bigpts"
			bd <- convert_polygon_df_units(boundary_df, default.units)
			return(convert_polygon_df_to_polygon_grob(
				bd,
				default.units = default.units,
				gp = gpar(fill = col, col = NA)
			))
		}
		if (spec$special == "circle") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			spacing_bigpts <- convertX(
				unit(params$pattern_spacing, params$pattern_units),
				"bigpts",
				valueOnly = TRUE
			)
			density <- params$pattern_linewidth * .pt / spacing_bigpts
			return(patternGrob(
				"circle",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				colour = NA,
				fill = col,
				spacing = params$pattern_spacing,
				density = density,
				grid = "hex_circle"
			))
		}
		if (spec$special == "olive") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			spacing_bigpts <- convertX(
				unit(params$pattern_spacing, params$pattern_units),
				"bigpts",
				valueOnly = TRUE
			)
			lwd_bigpts <- params$pattern_linewidth * .pt
			density_dot <- lwd_bigpts / spacing_bigpts * 0.6
			density_plus <- min(lwd_bigpts / spacing_bigpts * 2, 0.9)
			return(patternGrob(
				"pch",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				colour = col,
				fill = col,
				shape = c(16, 3),
				spacing = params$pattern_spacing,
				density = c(density_dot, density_plus),
				angle = 0,
				type = "diagonal",
				subtype = 2L,
				grid = "square"
			))
		}
		if (spec$special == "steel") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			spacing_bigpts <- convertX(
				unit(params$pattern_spacing, params$pattern_units),
				"bigpts",
				valueOnly = TRUE
			)
			lwd_bigpts <- params$pattern_linewidth * .pt
			density <- min(lwd_bigpts / spacing_bigpts * 2.5, 0.9)
			return(patternGrob(
				"pch",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				colour = col,
				fill = col,
				shape = 3,
				spacing = params$pattern_spacing,
				density = density,
				angle = 0,
				grid = "square"
			))
		}
		if (spec$special == "c_hex") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			return(patternGrob(
				"text",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				colour = col,
				shape = "c",
				spacing = params$pattern_spacing,
				angle = 0,
				grid = "hex_circle"
			))
		}
		if (spec$special == "checker") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			return(patternGrob(
				"polygon_tiling",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				type = "square",
				spacing = params$pattern_spacing,
				angle = 0,
				colour = NA,
				fill = c(col, "transparent")
			))
		}
		if (spec$special == "cendree") {
			# Build segments directly at exact grid intersections (no lty approximation).
			# At each (m, j) grid point, draw an H dash if (m+j) is even, V dash if odd.
			# Dashes have length exactly S so centers are precisely at grid positions.
			default.units <- "bigpts"
			boundary_df_pts <- convert_polygon_df_units(boundary_df, default.units)
			params_pts <- convert_params_units(params, default.units)
			vpm <- get_vp_measurements(default.units)
			col <- update_alpha(params_pts$pattern_colour, params_pts$pattern_alpha)
			lwd <- params_pts$pattern_linewidth * .pt
			gp_seg <- gpar(
				col = col,
				lwd = lwd,
				lty = "solid",
				lineend = params_pts$pattern_lineend
			)
			S <- params_pts$pattern_spacing
			xo <- params_pts$pattern_xoffset
			yo <- params_pts$pattern_yoffset
			half <- S / 2
			# Grid indices covering viewport + margin (same extent as get_xy_grid)
			idx_seq <- seq_robust(from = 0, to = vpm$length, by = S)
			idx <- round(c(rev(-idx_seq[-1L]), idx_seq) / S) # ..., -2, -1, 0, 1, 2, ...
			x_grid <- xo + vpm$x + idx * S
			y_grid <- yo + vpm$y + idx * S
			ng <- length(idx)
			m_mat <- matrix(idx, nrow = ng, ncol = ng, byrow = FALSE)
			j_mat <- matrix(idx, nrow = ng, ncol = ng, byrow = TRUE)
			x_mat <- matrix(x_grid, nrow = ng, ncol = ng, byrow = FALSE)
			y_mat <- matrix(y_grid, nrow = ng, ncol = ng, byrow = TRUE)
			is_H <- (m_mat + j_mat) %% 2L == 0L
			x_H <- as.vector(x_mat[is_H])
			y_H <- as.vector(y_mat[is_H])
			x_V <- as.vector(x_mat[!is_H])
			y_V <- as.vector(y_mat[!is_H])
			maskee <- grid::grobTree(
				segmentsGrob(
					x_H - half,
					y_H,
					x_H + half,
					y_H,
					default.units = default.units,
					gp = gp_seg
				),
				segmentsGrob(
					x_V,
					y_V - half,
					x_V,
					y_V + half,
					default.units = default.units,
					gp = gp_seg
				)
			)
			masker <- convert_polygon_df_to_polygon_grob(
				boundary_df_pts,
				default.units = default.units,
				gp = gpar(fill = "white", col = NA, lwd = 0)
			)
			return(alphaMaskGrob(
				maskee,
				masker,
				use_R4.1_masks = params$pattern_use_R4.1_masks,
				png_device = params$pattern_png_device,
				res = params$pattern_res
			))
		}
		if (spec$special == "proper") {
			col <- update_alpha(params$pattern_colour, params$pattern_alpha)
			spacing_bigpts <- convertX(
				unit(params$pattern_spacing, params$pattern_units),
				"bigpts",
				valueOnly = TRUE
			)
			density <- params$pattern_linewidth * .pt / spacing_bigpts
			return(patternGrob(
				"wave",
				x = boundary_df$x,
				y = boundary_df$y,
				id = boundary_df$id,
				colour = NA,
				fill = c(col, "transparent"),
				angle = 135,
				spacing = params$pattern_spacing,
				density = density,
				type = "triangle"
			))
		}
	}

	# Build all line-segment grobs and apply a single mask.  Using separate
	# alphaMaskGrobs in a grobTree breaks compositing (full-viewport rasters
	# overwrite each other), so we combine the maskees first.
	default.units <- "bigpts"
	boundary_df_pts <- convert_polygon_df_units(boundary_df, default.units)
	params_pts <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	spacings <- spec$spacings %||% rep(1, length(spec$angles))
	maskees <- mapply(
		function(angle, lty, spacing_mult) {
			p <- params_pts
			p$pattern_angle <- angle
			p$pattern_linetype <- lty
			p$pattern_stagger <- (lty != "solid")
			p$pattern_spacing <- p$pattern_spacing * spacing_mult
			create_line_maskee(p, vpm, default.units)
		},
		spec$angles,
		spec$linetypes,
		spacings,
		SIMPLIFY = FALSE
	)
	maskee <- if (length(maskees) == 1L) maskees[[1L]] else do.call(grid::grobTree, maskees)
	masker <- convert_polygon_df_to_polygon_grob(
		boundary_df_pts,
		default.units = default.units,
		gp = gpar(fill = "white", col = NA, lwd = 0)
	)
	alphaMaskGrob(
		maskee,
		masker,
		use_R4.1_masks = params$pattern_use_R4.1_masks,
		png_device = params$pattern_png_device,
		res = params$pattern_res
	)
}
