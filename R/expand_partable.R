# requires group_out, inst_template, rm_mustache

#' Remove variable from lavaan partable.
#'
#' @param d data.frame specifying lavaan partable
#' @param varname variables to be removed
#' @param expand should results be passed to \code{\link{expand_mod}}?
#' @export
remove_var <- function(d, varname, expand=FALSE){
  mod_block <- subset(d, (!d$rhs %in% varname) & (!d$lhs %in% varname) )

  if (expand) expand_mod(mod_block)
  else mod_block
}

#' Expand lavaan partable into different formats.
#'
#'  * args: array with value of each parameter
#'  * mod_block: original lavaan partable
#'  * template: lavaan syntax with labels in handlebars (e.g. "{{label_name}}")
#'  * mod_inst: lavaan syntax with labels replaced by their value
#'  * mod: lavaan syntax with unaltered labels
#'
#' @param mod_block d data.frame specifying lavaan partable
#' @export
#' @return list with named entry for each format
expand_mod <- function(mod_block) {
  label <- mod_block$label
  value <- mod_block$value
  args <- sapply(unique(label[label != ""]),
                 function(k) as.numeric(mod_block[match(k,label), 'value']))

  mod_pars <- mod_block
  mod_pars$label <- ifelse(label == "", as.character(value), label)

  list(
    args = args,                                # array w/ param_name: value
    mod_block = mod_block,                      # data.frame specifying model
    template = tmp <- group_out(mod_block),     # lav syntax w/ {{param}}
    mod_inst = inst_template(tmp, args),        # lav syntax w/ params fixed
    mod = rm_mustache(group_out(mod_pars))      # lav syntax w/ free params
  )
}
