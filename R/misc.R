write_model <- function(bundle, model_file = "", inst_file = "") {
  # takes bundle from expand_model
  # write lavaan model
  if (length(model_file))
    cat(bundle$mod, file=model_file)
  # write lavaan model with pars filled in
  if (length(inst_file))
    args_comment <- sapply(unique(names(bundle$args)),
                           function(k, args) paste('#', k, ':', args[k]),
                           args=bundle$args)

  cat(file=inst_file, sep='\n',
      '# Model with following args set.. ', args_comment, bundle$mod_inst)
}
