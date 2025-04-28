# function to create table of text analysis results in study 6
create_table_text_analysis_study6 <- function(butter_data) {
  butter_data %>%
    filter(ngram %in% c("lazy", "caring", "thoughtful", 
                        "genuine", "loves", "romantic")) %>%
    transmute(
      Word                     = str_to_title(ngram),
      `Control\nFreq.`         = round(Control_Freq),
      `Tool\nFreq.`            = round(Tool_Freq),
      `Full\nFreq.`            = round(Full_Freq),
      `%DIFF\nFull vs Control` = round(`Full vs Control: %DIFF`, 2),
      `LL\nFull vs Control`    = round(`Full vs Control: LL`, 2),
      `%DIFF\nTool vs Control` = round(`Tool vs Control: %DIFF`, 2),
      `LL\nTool vs Control`    = round(`Tool vs Control: LL`, 2),
      `%DIFF\nFull vs Tool`    = round(`Full vs Tool: %DIFF`, 2),
      `LL\nFull vs Tool`       = round(`Full vs Tool: LL`, 2)
    )
}
