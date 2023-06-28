#' Process SEM R2 for point figure for manuscript 6/2/2023
#'
#' @param R2out_merge Merged observed R2 and max R2 output dataframes to modify
#'
#' @return Processed dataframe with percentage of maximum R2 that was explained in the model and relabled variables
#' @export
#'
#' @examples
#' R2_proc_all <- R2_calc(merged_R2_df) # Apply function on merged dataframe to calculate %max R2 explained in the model


R2_calc = function(R2out_merge){
  R2out_proc <- R2out_merge %>%
    mutate(R2_perc = R2/MaxR2*100)%>% # Calculate % maxR2
    mutate(across(R2_perc, round,1))%>%
    mutate(across(c(R2, MaxR2), round,2))%>%
    mutate(Variable = recode_factor(Variable, # Relabel response variables
                               Lpt01_XCMGW="Site-riparian cover index",L_STRM_POWER="Specific stream power",
                               LQLow_kmcl="Summer flow/km2",LQbkf_kmcl="Bankfull flow/km2",
                               evap_index_sc="Evaporation indicator",LRBS_use="Relative bed stability",
                               L_NTL="TN",L_SULF="Sulfate",
                               OE_SCORE="Biotic response",
                               MMI_BENT_sc="Biotic response",
                               EPT_RICH_sc= "Biotic response"))%>%
    mutate(Category=Variable)%>% # Create a Category variable
    mutate(Category = recode_factor(Category,
                                    "Site-riparian cover index" = "Riparian cover",
                                    "Specific stream power"="Hydromorphology",
                                    "Summer flow/km2"="Hydromorphology","Bankfull flow/km2"="Hydromorphology",
                                    "Evaporation indicator"="Hydromorphology",
                                    "Relative bed stability"="Substrate",
                                    "TN"="Chemistry","Sulfate"="Chemistry"))

}
