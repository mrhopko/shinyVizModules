#'Contains a table of geom mappings
#'
#'@param geom_name character (default = "") if not supplied returns whole table
#'@returns list of geom(s) and mapping requirements/allowances
geom_mapping <- function(geom_name = "") {
  
  position_list <- list(
      type = "character",
      choices = c("stack","dodge","fill"),
      selected = "stack")
  
  colour_choices <- c("black","red","green","blue")
  
  shape_choices = c(0:127)
  
  p_list <- function(type, choices, selected) {
    return(
      list(type = type, choices = choices, selected = selected)
    )
  }
  
  mappings <- 
    list(
      geom_abline = 
        list(
          aes_required = character(),
          aes_allowed = c("alpha", "colour", "linetype", "size"),
          params = 
            list(slope = p_list("numeric",numeric(),NULL),
                intercept = p_list("numeric",numeric(),NULL)
                )
          ),
      geom_hline = 
        list(
          aes_required =  character(),
          aes_allowed = c("alpha", "colour", "linetype", "size"),
          params = list(xintercept = p_list("numeric",numeric(),NULL))
          ),
      geom_vline = 
        list(
          aes_required =  character(),
          aes_allowed = c("alpha", "colour", "linetype", "size"),
          params = list(yintercept = p_list("numeric",numeric(),NULL))
          ),
      geom_bar = 
        list(
          aes_required = "x",
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size"),
          params = list(
            stat = p_list("character",c("count","bin","identity"),"count"),
            position = position_list,
            binwidth = p_list("numeric",numeric(),NULL)
          )
        ),
      geom_bin2d = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","fill"),
          params = 
            list(
              position = position_list
            )
        ),
      geom_boxplot = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","shape","size","weight"),
          params = 
            list(
              outlier.colour = p_list("character", colour_choices, NULL),
              outlier.shape = p_list("integer", shape_choices, 19),
              outlier.size = p_list("numeric", numeric(), 1.5),
              outlier.stroke = p_list("numeric",numeric(),0.5),
              notch = p_list("logical", c(TRUE,FALSE), FALSE),
              notchwidth = p_list("numeric",numeric(),0.5),
              varwidth = p_list("logical", c(TRUE,FALSE), FALSE)
            )
          ),
      geom_contour = 
        list(
          aes_required = c("x","y"),
          aes_allowed = c("x","y","alpha","colour","linetype","size","weight"),
          params = 
            list(
              lineend = p_list("character",c("round","butt","square"), "butt"),
              linejoin = p_list("character",c("round","mitre","bevel"),"round")
            )
        ),
      geom_count = 
        list(
          aes_required = c("x","y") ,
          aes_allowed =  c("x","y","alpha","colour", "fill", "shape","size","stroke"),
          params = list()),
      geom_crossbar = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin", "alpha","colour","linetype","size"),
          params = list(
            fatten = p_list("numeric",numeric(),2.5)
          )),
      geom_errorbar = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin", "alpha","colour","linetype","size"),
          params = list()),
      geom_linerange = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin", "alpha","colour","linetype","size"),
          params = list()),
      geom_pointrange = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin", "alpha","colour","linetype","size"),
          params = list(
            fatten = p_list("numeric",numeric(),4)
          )),
      geom_density = 
        list(
          aes_required =  c("x"),
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size","weight"),
          params = list(
            stat = p_list("character", c("density", "count"), "density")
          )),
      geom_density_2d = 
        list(
          aes_required = c("x","y"),
          aes_allowed = c("x","y","alpha","colour","linetype","size"),
          params = list(
            lineend = p_list("character",c("round","butt","square"), "butt"),
            linejoin = p_list("character",c("round","mitre","bevel"),"round"),
            linemitre = p_list("numeric", numeric(), 1)
          )),
      geom_dotplot = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill"),
          params = list(
            binwidth = p_list("numeric", numeric(), NULL),
            binaxis = p_list("character",c("x","y"),"x"),
            method = p_list("character",c("dotdensity", "histodot"),"dotdensity"),
            binpositions = p_list("character",c("bygroup", "all"),"bygroup"),
            stackdir = p_list("character",c("up", "down","center","centerwhole"),"up"),
            stackratio = p_list("numeric", c(0,1), 1),
            dotsize = p_list("numeric", c(0,1), 1),
            stackgroups = p_list("logical", c(TRUE, FALSE), FALSE)
          )
        ),
      geom_errorbarh = 
        list(
          aes_required = c("x","xmax","xmin", "y") ,
          aes_allowed = c("x","xmax","xmin", "y", "alpha","colour","height","linetype","size"),
          params = list(position = position_list)),
      geom_freqpoly = 
        list(
          aes_required = "x",
          aes_allowed = c("x","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_histogram = 
        list(
          aes_required = "x",
          aes_allowed = c("x","alpha","colour","fill","linetype","size"),
          params = list(
            binwidth = p_list("numeric", numeric(), NULL),
            bins = p_list("integer", integer(), NULL)
          )),
      geom_hex = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","size"),
          params = list()),
      geom_jitter = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","shape","size","stroke"),
          params = list(
            width = p_list("numeric",numeric(), NULL),
            height = p_list("numeric",numeric(), NULL)
          )),
      geom_label = 
        list(
          aes_required = c("label","x","y") ,
          aes_allowed = c("label","x","y", "alpha","angle","colour","family","fontface","hjust","lineheight","size","vjust"),
          params = list(
            parse = p_list("logical", c(TRUE,FALSE), FALSE),
            nudge_x = p_list("numeric", numeric(), 0),
            nudge_y = p_list("numeric", numeric(), 0),
            label.padding = p_list("numeric", numeric(), unit(0.25, "lines")),
            label.r = p_list("numeric", numeric(), unit(0.15, "lines")),
            label.size = p_list("numeric", numeric(), 0.25)
          )
        ),
      geom_text = 
        list(
          aes_required =  c("label","x","y"),
          aes_allowed = c("label","x","y", "alpha","angle","colour","family","fontface","hjust","lineheight","size","vjust"),
          params = list(
            parse = p_list("logical", c(TRUE,FALSE), FALSE),
            nudge_x = p_list("numeric", numeric(), 0),
            nudge_y = p_list("numeric", numeric(), 0),
            check_overlap = p_list("logical", c(TRUE,FALSE), FALSE)
            )
          ),
      geom_map = 
        list(
          aes_required = c("map_id") ,
          aes_allowed = c("map_id","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_path = 
        list(
          aes_required = c("x", "y") ,
          aes_allowed = c("x","y", "alpha","colour","linetype","size"),
          params = list(
            lineend = p_list("character",c("round","butt","square"), "butt"),
            linejoin = p_list("character",c("round","mitre","bevel"),"round"),
            linemitre = p_list("numeric", numeric(), 1)
          )
        ),
      geom_line = 
        list(
          aes_required = c("x", "y") ,
          aes_allowed = c("x","y", "alpha","colour","linetype","size"),
          params = list()),
      geom_step = 
        list(
          aes_required = c("x", "y") ,
          aes_allowed = c("x","y", "alpha","colour","linetype","size"),
          params = list(
            direction = p_list("character", c("vh","hv"), "hv")
          )),
      geom_point = 
        list(
          aes_required = c("x","y"),
          aes_allowed = c("x","y","alpha","colour","fill","shape","size","stroke"),
          params = list(
            alpha = p_list("numeric", numeric(), NULL),
            colour = p_list("character", colour_choices, NULL),
            fill = p_list("character", colour_choices, NULL),
            shape = p_list("numeric", shape_choices, NULL),
            size = p_list("numeric", numeric(), NULL),
            stroke = p_list("character", colour_choices, NULL)
          )),
      geom_polygon = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_quantile = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","linetype", "size","weight"),
          params = list(
            lineend = p_list("character",c("round","butt","square"), "butt"),
            linejoin = p_list("character",c("round","mitre","bevel"),"round"),
            linemitre = p_list("numeric", numeric(), 1)
          )),
      geom_raster = 
        list(
          aes_required =  c("x","y"),
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size"),
          params = list(
            hjust = p_list("numeric", numeric(), 0.5),
            vjust = p_list("numeric", numeric(), 0.5),
            interpolate = p_list("logical", c(TRUE,FALSE), FALSE)
          )
        ),
      geom_rect = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_tile = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_ribbon = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_area = 
        list(
          aes_required = c("x","ymax","ymin") ,
          aes_allowed = c("x","ymax","ymin","alpha","colour","fill","linetype","size"),
          params = list()),
      geom_segment = 
        list(
          aes_required = c("x","xend","y","yend") ,
          aes_allowed = c("x","xend","y","yend", "alpha","colour","linetype","size"),
          params = list(lineend = p_list("character",c("line","butt","square"), "butt"))
          ),
      geom_curve = 
        list(
          aes_required = c("x","xend","y","yend") ,
          aes_allowed = c("x","xend","y","yend", "alpha","colour","linetype","size"),
          params = list(
            curvature = p_list("numeric", numeric(), 0.5),
            angle = p_list("numeric", numeric(), 90),
            ncp = p_list("numeric", numeric(), 5),
            lineend = p_list("character",c("line","butt","square"), "butt")
            )
          ),
      geom_smooth = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size","weight"),
          params = list(
            formula = p_list("formula", character(), "y ~ x"),
            method = p_list("character", c("lm", "glm", "gam", "loess", "rlm", "auto"), "auto"),
            se = p_list("logical", c(TRUE, FALSE), TRUE)
          )
        ),
      geom_violin = 
        list(
          aes_required = c("x","y") ,
          aes_allowed = c("x","y","alpha","colour","fill","linetype","size","weight"),
          params = list(
            trim = p_list("logical", c(TRUE, FALSE), TRUE),
            scale = p_list("character",c("area","count","width"), "area")
          )
        )
    )
  
  if(geom_name %in% names(mappings)) {
    mappings[[geom_name]]
  } else {
    mappings
  }
}


geom_default_mapping <- function(geom_name, current_map_attr) {
  if(is_null_empty_na(geom_name)) return(list())
  aes_required <- geom_mapping(geom_name)$aes_required
  if(is_null_empty_na(aes_required)) return(list())
  length_diff <- length(aes_required) - length(current_map_attr)
  attr_required <- 
    if(length_diff > 0) {
      c(current_map_attr, rep(current_map_attr[1], length_diff))  
    } else if(length_diff == 0) {
      current_map_attr
    } else {
      current_map_attr[1:(-length_diff)]
    }
  list("aes" = aes_required, "attr" = attr_required, "map_count" = length(aes_required), "map_deleted" = numeric())
}


geom_params_to_list <- function(geom_name, current_param_list, layer_id) {
  
  if(is_null_empty_na(geom_name)) return(list())
  if(is_null_empty_na(current_param_list)) return(list())
  
  required_params <- paste0(names(geom_mapping(geom_name)$params), layer_id)
  
  geom_params <- current_param_list[required_params]
  names(geom_params) <- names(geom_mapping(geom_name)$params)
  
  geom_params_no_empty <- rlist::list.clean(geom_params, function(x) is_null_empty_na(x,test_blank = TRUE))
  
  geom_params_no_empty
}
