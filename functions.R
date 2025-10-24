output_ipa_table <- function(
  font = "serif"
) {
  ipa_symbols <- ipa_symbols |> 
  mutate(
    glyph = IPA
  ) |> 
    select(IPA, glyph, uni_name, ipa_name, unicode)

  ipa_symbols |> datatable() |> 
  formatStyle(
    "glyph",
    fontFamily = font
  )
}