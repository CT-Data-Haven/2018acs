weight_regions <- function(.data, 
                           lookup, 
                           ...,
                           join_cols = NULL, estimate = estimate, weight = weight, moe = NULL) {
  group_vars <- quos(...)
  est_var <- enquo(estimate)
  weight_var <- enquo(weight)
  
  grouped <- .data %>%
    dplyr::inner_join(lookup, by = join_cols) %>%
    dplyr::group_by(!!!group_vars)
  # testing for if moe is null
  if (!rlang::quo_is_null(rlang::enquo(moe))) {
    moe_var <- enquo(moe)
    
    out <- grouped %>%
      dplyr::mutate_at(dplyr::vars(!!est_var, !!moe_var), dplyr::funs(. * !!weight_var)) %>%
      dplyr::summarise(!!quo_name(est_var) := sum(!!est_var),
                !!quo_name(moe_var) := tidycensus::moe_sum(!!moe_var, !!est_var)) %>%
      dplyr::mutate_at(dplyr::vars(!!est_var, !!moe_var), round)
  } else {
    out <- grouped %>%
      dplyr::mutate_at(dplyr::vars(!!est_var), dplyr::funs(. * !!weight_var)) %>%
      dplyr::summarise(!!quo_name(est_var) := sum(!!est_var)) %>%
      dplyr::mutate_at(vars(!!est_var), round)
  }
  out %>%
    dplyr::ungroup()
}

