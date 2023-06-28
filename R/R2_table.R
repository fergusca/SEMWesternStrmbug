#' Process SEM R2 output for OE,MMI, EPT relabel variable names to make into a table
#' Revised script to accomodated updated lavaan model output - lavInspect
#'
#' @param sem_output SEM estimated effects output saved as .csv file
#'
#' @return Processed dataframe that selected direct, indirect, and total effects
#' @export
#'
#' @examples
#'west_oe_m1_full<- r2_tab("model1") # name R2 dataframe processed from SEM output
r2_tab_v2 = function(R2_output){
  R2_out_proc <- R2_output %>%
    select(Variable,R2)%>%
    rename(Response = Variable, R2 = R2)%>%
    mutate(across(where(is.numeric), round,2))%>%
    mutate(Response = recode_factor(Response,
                                    L_STRM_POWER="Specific stream power",
                                    Lpt01_XCMGW="Site-riparian cover index",
                                    LQLow_kmcl="Summer flow/km2",LQbkf_kmcl="Bankfull flow/km2",
                                    LOE_QLow_cl="O/E Summer flow",LOE_Qbkf_cl="OE Bankfull flow",
                                    evap_index_sc= "Evaporation indicator",
                                    Lpt01_XFC_NAT="Instream cover",LRBS_use="Relative bed stability",
                                    L_NTL="TN", L_SULF="Sulfate",
                                    OE_SCORE="OE", MMI_BENT_sc="MMI", EPT_RICH_sc="EPT"))
}

