new_assignment_linter = function(source_file) {
  left_assign_lints = lapply(lintr:::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
    parsed = lintr:::with_id(source_file, id)

    lintr::Lint(filename = source_file$filename, line_number = parsed$line1,
                column_number = parsed$col1, type = "style", message = "Use =, not <-, for assignment.",
                line = source_file$lines[as.character(parsed$line1)],
                linter = "assignment_linter")
  })

  right_assign_lints = lapply(lintr:::ids_with_token(source_file, "RIGHT_ASSIGN"), function(id) {
    parsed = lintr:::with_id(source_file, id)

    lintr::Lint(filename = source_file$filename, line_number = parsed$line1,
                column_number = parsed$col1, type = "style", message = "Use =, not ->, for assignment.",
                line = source_file$lines[as.character(parsed$line1)],
                linter = "assignment_linter")
  })

  return(c(left_assign_lints, right_assign_lints))
}
