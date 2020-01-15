spread_with_multiple_values <- function(data, key, value, fill = NA,
                                        convert = FALSE, drop = TRUE,
                                        sep = NULL, aggfunc = NULL, ...) {
  args <- list(...)
  key_var <- vars_pull(names(data), !!enquo(key))
  value_var <- vars_pull(names(data), !!enquo(value))
  by <- colnames(data)[which(!colnames(data) %in% c(key_var, value_var))]
  col <- data %>%
    pull(key_var) %>%
    unique() %>%
    as.character()
  
  data <- map(
    col,
    function(x) data %>%
      filter(!!sym(key_var) == x)
  ) %>%
    map2(col, ~change_colname(.x, .y, value_var, key_var)) %>%
    map2(col, ~apply_aggfunc(.x, .y,
                             group_by_col = by,
                             aggfunc = aggfunc,
                             args
    )) %>%
    map2(col, ~apply_convert(.x, .y, convert)) %>%
    map2(col, ~apply_sep(.x, .y, key_var, sep)) %>%
    reduce(full_join, by = by)
  
  if (!drop) {
    data <- data %>% complete_(by)
  }
  
  if (!is.na(fill)) {
    key_value_cols <- colnames(data)[which(!colnames(data) %in% by)]
    data <- data %>% mutate_at(
      vars(one_of(key_value_cols)),
      funs(replace(., is.na(.), fill))
    )
  }
  
  return(data)
}

change_colname <- function(data, new_col, value, old_col) {
  data %>%
    rename(!!as.character(new_col) := !!value) %>%
    select(-one_of(old_col))
}


apply_aggfunc <- function(data, col_name, group_by_col, aggfunc, args) {
  if (is.function(aggfunc)) {
    data <- data %>%
      group_by(!!!syms(group_by_col)) %>%
      summarize(
        !!col_name := do.call(
          aggfunc,
          args = c(list(!!sym(col_name)), args) %>% compact()
        )
      ) %>%
      ungroup()
  } else {
    data
  }
}

apply_convert <- function(data, col_name, convert) {
  values <- data[[col_name]]
  if (convert & !is_character(values)) {
    values <- as.character(values)
    values <- type.convert(values, as.is = TRUE)
  }
  data <- data %>% mutate(!!col_name := values)
}

apply_sep <- function(data, new_col, old_col, sep) {
  if (!is.null(sep)) {
    data %>%
      rename(!!str_c(as.character(old_col), as.character(new_col),
                     sep = sep
      ) := !!as.character(new_col))
  } else {
    data
  }
}

