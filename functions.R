library(phonetisr)
library(tidyverse)
library(DT)
library(htmltools)
library(knitr)
library(glue)

output_ipa_chart <- function(
  font = "serif"
) {

  style_tag <- tags$style(HTML(glue("
  table {{
    font-family: {font}
  }}
")))
  
### 1. Pulmonic consonants (from before)
ipa_consonants <- data.frame(
  Manner = c("Plosive","Nasal","Trill","Tap or flap",
             "Fricative","Lateral fricative",
             "Approximant","Lateral approximant"),
  Bilabial     = c("p b","m","","","ɸ β","","ʋ",""),
  Labiodental  = c("","ɱ","","","f v","","",""),
  Dental       = c("","n̪","","","θ ð","","",""),
  Alveolar     = c("t d","n","r","ɾ","s z","","ɹ","l"),
  Postalveolar = c("","","","","ʃ ʒ","","",""),
  Retroflex    = c("ʈ ɖ","ɳ","ɽ","","ʂ ʐ","","ɻ","ɭ"),
  Palatal      = c("c ɟ","ɲ","","","ç ʝ","","j","ʎ"),
  Velar        = c("k ɡ","ŋ","","","x ɣ","","ɰ","ʟ"),
  Uvular       = c("q ɢ","ɴ","ʀ","","χ ʁ","","",""),
  Pharyngeal   = c("","","","","ħ ʕ","","",""),
  Glottal      = c("ʔ","","","","h ɦ","","",""),
  stringsAsFactors = FALSE
)

### 2. Non-pulmonic consonants
ipa_nonpulmonic <- data.frame(
  Type = c("Clicks","Ejectives","Implosives"),
  Symbols = c(
    "! ǀ ǁ ǃ ǂ ʘ",
    "pʼ tʼ kʼ sʼ",
    "ɓ ɗ ʄ ɠ ʛ"
  ),
  stringsAsFactors = FALSE
)

### 3. Vowels (IPA trapezium layout)
ipa_vowels <- data.frame(
  Height = c("Close","Near-close","Close-mid","Mid","Open-mid","Near-open","Open"),
  Front = c("i y","ɪ ʏ","e ø","ə","ɛ œ","æ","a ɶ"),
  Central = c("ɨ ʉ","","ɘ ɵ","","ɜ ɞ","ɐ","ä"),
  Back = c("ɯ u","ʊ","o ɤ","","ɔ","ʌ","ɑ ɒ"),
  stringsAsFactors = FALSE
)

### 4. Diacritics
ipa_diacritics <- data.frame(
  Function = c(
    "Voicing","Aspirated","Breathy voice","Creaky voice","Nasalized",
    "Velarized or pharyngealized","Labialized","Palatalized","Dental","Advanced","Retracted","Raised","Lowered"
  ),
  Diacritic = c(
    " ̬","ʰ"," ̤"," ̰"," ̃",
    " ˠ","ʷ","ʲ","̪","̟","̠","̝","̞"
  ),
  Example = c(
    "s̬","pʰ","b̤","a̰","ã",
    "ɫ","tʷ","tʲ","t̪","e̟","e̠","e̝","e̞"
  ),
  stringsAsFactors = FALSE
)

### 5. Suprasegmentals
ipa_suprasegmentals <- data.frame(
  Feature = c("Primary stress","Secondary stress","Length","Half-long","Extra-short",
              "Minor group (foot)","Major group (intonation phrase)","Linking (tie bar)"),
  Symbol  = c("ˈ","ˌ","ː","ˑ","̆",".","‖","͜"),
  Example = c("ˈkan","ˌkan","aː","eˑ","ŏ","pros.od.y","intonation‖phrase","k͜p"),
  stringsAsFactors = FALSE
)

### 6. Extended IPA (selected)
ipa_extended <- data.frame(
  Category = c(
    "Airstream mechanisms","Voice quality","Sound source","Timing/tempo","Additional articulations"
  ),
  Examples = c(
    "↓ ↑ ↗ ↘ (tone arrows); ʘ͜ for double articulation",
    "ʱ (murmur), ˤ (pharyngealized), ͈ (strong articulation)",
    "ʩ (fricated bilabial stop), ʭ (nasal snort)",
    "| |̟ (length markers for segments)",
    "t͈ p͈ k͈ (tense stops), ʩ ʪ ʫ (fricatives for disordered speech)"
  ),
  stringsAsFactors = FALSE
)


  # Convert to HTML tables
  tbl_consonants <- HTML(knitr::kable(ipa_consonants, format = "html"))
  tbl_nonpulmonic <- HTML(knitr::kable(ipa_nonpulmonic, format = "html"))
  tbl_vowels <- HTML(knitr::kable(ipa_vowels, format = "html"))
  tbl_diacritics <- HTML(knitr::kable(ipa_diacritics, format = "html"))
  tbl_suprasegmentals <- HTML(knitr::kable(ipa_suprasegmentals, format = "html"))
  
  # Assemble as HTML tags
  html_output <- tagList(
    style_tag,
    tags$h3("Consonants"), tbl_consonants,
    tags$h3("Non-pulmonic"), tbl_nonpulmonic,
    tags$h3("Vowels"), tbl_vowels,
    tags$h3("Diacritics"), tbl_diacritics,
    tags$h3("Suprasegmentals"), tbl_suprasegmentals,
  )
  
  # Return the assembled HTML as a single object
  return(html_output)
}


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