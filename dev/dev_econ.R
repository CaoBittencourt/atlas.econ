# [FUNCTIONS] --------------------------------------------------------------
# # - fun_econ_taxa ---------------------------------------------------------
# fun_econ_taxa <- function(
    #     mtx_hireability,
#     df_taxa
# ){
#
#   # arguments validation
#   stopifnot(
#     "'mtx_hireability' must be a square matrix of hireability scores between 0 and 1." =
#       all(
#         is.matrix(mtx_hireability),
#         is.numeric(mtx_hireability),
#         all(diag(mtx_hireability) == 1),
#         all(mtx_hireability >= 0),
#         all(mtx_hireability <= 1)
#       )
#   )
#
#   stopifnot(
#     "'df_taxa' must be a data frame with the 'df_taxa' subclass." =
#       all(
#         is.data.frame(df_taxa)
#         , any(class(df_taxa) == 'df_taxa')
#       )
#   )
#
#   # data wrangling
#   mtx_hireability %>%
#     as_tibble(
#       rownames =
#         'set'
#     ) -> df_hireability
#
#   rm(mtx_hireability)
#
#   # hireability data frame
#   # filter by hireability to reduce join
#   df_hireability %>%
#     pivot_longer(
#       cols = -1
#       , names_to = 'competing_set'
#       , values_to = 'hireability'
#     ) %>%
#     filter(
#       hireability > 0
#     ) -> df_hireability
#
#   # taxo-economic competition data frame
#   df_taxa %>%
#     unnest(set) %>%
#     rename(
#       set = set
#     ) %>%
#     left_join(
#       df_hireability
#       , multiple = 'all'
#       , relationship = 'many-to-many'
#     ) %>%
#     relocate(any_of(
#       names(df_taxa)
#     )) %>%
#     group_by(
#       taxon,
#       taxon_id
#     ) %>%
#     mutate(
#       .after = n
#       , ncomp =
#         unique(competing_set) %>%
#         length()
#     ) %>%
#     ungroup() ->
#     df_econ
#
#   rm(df_hireability)
#
#   rm(df_taxa)
#
#   # data frame subclass
#   new_data_frame(
#     df_econ
#     , class = c(
#       class(df_econ)
#       , 'df_econ'
#     )
#   ) -> df_econ
#
#   # output
#   return(df_econ)
#
# }

# - fun_econ_taxa ---------------------------------------------------------
fun_econ_taxa <- function(
    mtx_hireability
    , df_taxa
    , dbl_employment = NULL
    , dbl_wages = NULL
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

  stopifnot(
    "'dbl_employment' must be either NULL or a numeric vector the same length as the number of rows in 'mtx_hireability'." =
      any(
        is.null(dbl_employment)
        , all(
          is.numeric(dbl_employment)
          , length(dbl_employment) ==
            nrow(mtx_hireability)
        )
      )
  )

  stopifnot(
    "'dbl_wages' must be either NULL or a numeric vector the same length as the number of rows in 'mtx_hireability'." =
      any(
        is.null(dbl_wages)
        , all(
          is.numeric(dbl_wages)
          , length(dbl_wages) ==
            nrow(mtx_hireability)
        )
      )
  )

  # data wrangling
  mtx_hireability %>%
    as_tibble(
      rownames =
        'set'
    ) -> df_hireability

  rm(mtx_hireability)

  if(is.null(dbl_employment)){

    dbl_employment <- 1

  }

  if(is.null(dbl_wages)){

    dbl_wages <- 0

  }

  # add weights and wages
  dbl_employment ->
    df_hireability$
    employment

  dbl_wages ->
    df_hireability$
    wage

  # hireability data frame
  # filter by hireability to reduce join
  df_hireability %>%
    pivot_longer(
      cols = -c(
        'set',
        'employment',
        'wage'
      )
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
      set = set
    ) %>%
    left_join(
      df_hireability
      , multiple = 'all'
      , relationship = 'many-to-many'
    ) -> df_hireability

  rm(df_taxa)

  df_hireability %>%
    filter(
      set ==
        competing_set
    ) %>%
    select(
      competing_set
      , employment
      , wage
    ) %>%
    rename(
      competing_employment =
        employment
      , competing_wage =
        wage
    ) %>%
    right_join(
      df_hireability
      , multiple = 'all'
      , relationship = 'many-to-many'
    ) %>%
    relocate(any_of(
      names(df_hireability)
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
    ) %>%
    ungroup() ->
    df_econ

  rm(df_hireability)

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

# - fun_econ_employability ------------------------------------------------
fun_econ_employability <- function(df_econ, lgc_taxon = F){

  # arguments validation
  stopifnot(
    "'df_econ' must be a data frame with the 'df_econ' subclass." =
      all(
        is.data.frame(df_econ),
        any(class(df_econ) == 'df_econ')
      )
  )

  stopifnot(
    "'lgc_taxon' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_taxon)
        , !is.na(lgc_taxon)
      )
  )

  # calculate employability
  # for each occupation
  df_econ %>%
    group_by(
      taxon,
      taxon_id,
      competing_set
    ) %>%
    mutate(
      employability =
        weighted.mean(
          hireability
          , employment
        )
    ) %>%
    ungroup() %>%
    mutate(
      employment_potential =
        hireability *
        employment
    ) -> df_employability

  # for each taxon
  if(lgc_taxon){

    df_employability %>%
      right_join(
        df_employability %>%
          filter(
            set ==
              competing_set
          ) %>%
          select(
            competing_set
            , employment
          ) %>%
          rename(
            competing_employment =
              employment
          )
        , multiple = 'all'
        , relationship = 'many-to-many'
      ) %>%
      group_by(
        taxon,
        taxon_id
      ) %>%
      reframe(
        employability =
          weighted.mean(
            employability
            , competing_employment
          )
      ) -> df_employability

  }

  # reframe(
  #   employment_potential = sum(
  #     hireability *
  #       employment
  #   )
  #   , employment =
  #     first(employment)
  #   , employability =
  #     weighted.mean(
  #       hireability
  #       , employment
  #     )
  # ) -> df_employability

  rm(df_econ)

  # output
  return(df_employability)

}

# - fun_econ_versus ----------------------------------------------




# - fun_econ_value --------------------------------------------------------


