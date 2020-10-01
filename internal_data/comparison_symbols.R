COMP_EXACTLY_EQUAL = "=="
COMP_LTE = "<="
COMP_LT = "<"
COMP_GTE = ">="
COMP_GT = ">"
COMP_COMPATIBLE = "~="
COMP_NOT_EQUAL = "!="

COMP_UNCOMBINABLE = c(COMP_EXACTLY_EQUAL,
                      COMP_COMPATIBLE)

COMP_EQUALITY = c(COMP_EXACTLY_EQUAL,
                  COMP_LTE,
                  COMP_GTE,
                  COMP_COMPATIBLE)

COMPS = c(COMP_EXACTLY_EQUAL,
          COMP_LTE,
          COMP_GTE,
          COMP_COMPATIBLE,
          COMP_NOT_EQUAL,
          COMP_LT,
          COMP_GT)
