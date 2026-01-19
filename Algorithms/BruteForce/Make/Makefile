# Sudoku Solver in GNU Make
# Pure Make implementation of the backtracking algorithm

# Precompute box coordinates
box_rows_0 := 0 1 2
box_rows_1 := 0 1 2
box_rows_2 := 0 1 2
box_rows_3 := 3 4 5
box_rows_4 := 3 4 5
box_rows_5 := 3 4 5
box_rows_6 := 6 7 8
box_rows_7 := 6 7 8
box_rows_8 := 6 7 8

box_cols_0 := 0 1 2
box_cols_1 := 0 1 2
box_cols_2 := 0 1 2
box_cols_3 := 3 4 5
box_cols_4 := 3 4 5
box_cols_5 := 3 4 5
box_cols_6 := 6 7 8
box_cols_7 := 6 7 8
box_cols_8 := 6 7 8

# Iteration counter
ITER_LIST := 

# Validation check
is_valid = $(strip $(if $(filter $(3),$(foreach i,0 1 2 3 4 5 6 7 8,$(cell_$(1)_$(i)))),,$(if $(filter $(3),$(foreach i,0 1 2 3 4 5 6 7 8,$(cell_$(i)_$(2)))),,$(if $(filter $(3),$(foreach i,$(box_rows_$(1)),$(foreach j,$(box_cols_$(2)),$(cell_$(i)_$(j))))),,1))))

# Backtracking solver
# $(call solve,remaining_empty_cells)
define solve
$(strip \
  $(if $(1), \
    $(foreach pos,$(word 1,$(1)), \
      $(foreach r,$(word 1,$(subst _, ,$(pos))), \
        $(foreach c,$(word 2,$(subst _, ,$(pos))), \
          $(word 1,$(foreach v,1 2 3 4 5 6 7 8 9, \
            $(if $(filter 0,$(cell_$(r)_$(c))), \
              $(eval ITER_LIST += x) \
              $(if $(call is_valid,$(r),$(c),$(v)), \
                $(eval cell_$(r)_$(c) := $(v)) \
                $(if $(wordlist 2,999,$(1)), \
                  $(if $(call solve,$(wordlist 2,999,$(1))), \
                    1, \
                    $(eval cell_$(r)_$(c) := 0) \
                  ), \
                  1 \
                ) \
              ) \
            ) \
          ))
        ) \
      ) \
    ) \
  ,1) \
)
endef

# Include the matrix data
-include matrix_data.mk

# After including matrix_data.mk, find all empty cells
EMPTY_CELLS := $(strip $(foreach r,0 1 2 3 4 5 6 7 8,$(foreach c,0 1 2 3 4 5 6 7 8,$(if $(filter 0,$(cell_$(r)_$(c))),$(r)_$(c)))))

# Run the solver
ifeq ($(MAKECMDGOALS),solve)
  $(eval FINAL_STATUS := $(call solve,$(EMPTY_CELLS)))
endif

.PHONY: all solve print_result

all:
	@echo "Usage: make solve INPUT=file.matrix"

solve: print_result

print_result:
	@echo "$(INPUT)"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_0_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_1_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_2_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_3_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_4_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_5_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_6_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_7_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_8_$(c)) )"
	@echo ""
	@echo "Puzzle:"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_0_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_1_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_2_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_3_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_4_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_5_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_6_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_7_$(c)) )"
	@echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_8_$(c)) )"
	@if [ "$(FINAL_STATUS)" = "1" ] || [ "$(FINAL_STATUS)" = "1 1" ] || [ "$(word 1,$(FINAL_STATUS))" = "1" ]; then \
		echo "" ; \
		echo "Puzzle:" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_0_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_1_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_2_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_3_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_4_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_5_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_6_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_7_$(c)) )" ; \
		echo "$(foreach c,0 1 2 3 4 5 6 7 8,$(cell_8_$(c)) )" ; \
		echo "" ; \
		echo "Solved in Iterations=$(words $(ITER_LIST))" ; \
		echo "" ; \
	else \
		echo "No solution found" ; \
	fi

