# [FUNCTIONS] --------------------------------------------------------------
# - fun_econ_taxa ---------------------------------------------------------
fun_econ_taxa <- function(
    mtx_hireability,
    df_taxa
){

  # arguments validation
  stopifnot(
    "'mtx_hireability' must be a square matrix of hireability scores between 0 and 1." =
      all(
        is.matrix(mtx_hireability),
        is.numeric(mtx_hireability),
        all(diag(mtx_hireability) == 1),
        all(mtx_hireability >= 0),
        all(mtx_hireability <= 1)
      )
  )

  stopifnot(
    "'df_taxa' must be a data frame with the 'df_taxa' subclass." =
      all(
        is.data.frame(df_taxa)
        , any(class(df_taxa) == 'df_taxa')
      )
  )

  # data wrangling
  mtx_hireability %>%
    as_tibble(
      rownames =
        'comparison_set'
    ) -> df_hireability

  rm(mtx_hireability)

  # hireability data frame
  # filter by hireability to reduce join
  df_hireability %>%
    pivot_longer(
      cols = -1
      , names_to = 'competing_set'
      , values_to = 'hireability'
    ) %>%
    filter(
      hireability > 0
    ) -> df_hireability

  # taxo-economic competition data frame
  df_taxa %>%
    unnest(set) %>%
    rename(
      comparison_set = set
    ) %>%
    left_join(
      df_hireability
      , multiple = 'all'
      , relationship = 'many-to-many'
    ) %>%
    relocate(any_of(
      names(df_taxa)
    )) %>%
    group_by(
      taxon,
      taxon_id
    ) %>%
    mutate(
      .after = n
      , ncomp =
        unique(competing_set) %>%
        length()
    ) -> df_econ

  rm(df_hireability)
  rm(df_taxa)

  # data frame subclass
  new_data_frame(
    df_econ
    , class = c(
      class(df_econ)
      , 'df_econ'
    )
  ) -> df_econ

  # output
  return(df_econ)

}
