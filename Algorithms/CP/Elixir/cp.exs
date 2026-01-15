#!/usr/bin/env elixir
# Elixir Constraint Propagation (CP) Sudoku Solver
# Port of OCaml/C CP implementation using Agent for mutable state

defmodule CP do
  import Bitwise

  def main(args) do
    start_time = System.monotonic_time(:millisecond)

    args
    |> Enum.filter(&String.ends_with?(&1, ".matrix"))
    |> Enum.each(&solve_puzzle/1)

    end_time = System.monotonic_time(:millisecond)
    elapsed = (end_time - start_time) / 1000.0
    IO.puts("Seconds to process #{:erlang.float_to_binary(elapsed, decimals: 3)}")
  end

  defp solve_puzzle(filename) do
    IO.puts(filename)

    case read_matrix_file(filename) do
      {:ok, puzzle} ->
        print_raw_board(puzzle)
        print_puzzle(puzzle)

        # Create Agents for mutable state
        {:ok, grid_agent} = Agent.start_link(fn -> init_grid(puzzle) end)
        {:ok, cand_agent} = Agent.start_link(fn -> init_candidates(puzzle) end)
        {:ok, counter} = Agent.start_link(fn -> 0 end)

        # Propagate until fixpoint - finds naked/hidden singles (WITH counting)
        # Note: We don't pre-eliminate clue digits from peers - C reference doesn't either
        # The propagate loop will discover hidden singles and assign will eliminate from peers
        case propagate(grid_agent, cand_agent, counter) do
          :error ->
            IO.puts("No solution found (contradiction during propagation)")
          :ok ->
            # Solve
            case search(grid_agent, cand_agent, counter) do
              :ok ->
                solved = Agent.get(grid_agent, & &1)
                print_puzzle(solved)
                iterations = Agent.get(counter, & &1)
                IO.puts("\nSolved in Iterations=#{iterations}\n")

              :error ->
                IO.puts("No solution found")
            end
        end

        Agent.stop(counter)
        Agent.stop(cand_agent)
        Agent.stop(grid_agent)

      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end

  defp read_matrix_file(filename) do
    case File.read(filename) do
      {:ok, content} ->
        lines =
          content
          |> String.split("\n", trim: true)
          |> Enum.reject(&String.starts_with?(&1, "#"))
          |> Enum.take(9)

        puzzle =
          lines
          |> Enum.map(fn line ->
            line
            |> String.split(~r/\s+/, trim: true)
            |> Enum.map(&String.to_integer/1)
          end)

        {:ok, puzzle}

      error ->
        error
    end
  end

  defp print_raw_board(puzzle) do
    Enum.each(puzzle, fn row ->
      IO.puts(Enum.join(row, " ") <> " ")
    end)
  end

  defp print_puzzle(puzzle) do
    IO.puts("\nPuzzle:")

    Enum.each(puzzle, fn row ->
      IO.puts(Enum.join(row, " ") <> " ")
    end)
  end

  # Initialize grid with clue values (9x9 nested lists)
  defp init_grid(puzzle) do
    for row <- puzzle do
      for digit <- row do
        digit
      end
    end
  end

  # Initialize candidates with clue bits (bitsets, bits 1-9)
  defp init_candidates(puzzle) do
    # 0x3FE = 0011 1111 1110 (bits 1-9 set)
    for row <- puzzle do
      for digit <- row do
        if digit == 0, do: 0x3FE, else: 1 <<< digit
      end
    end
  end

  # Bitset operations
  defp has_candidate?(set, digit), do: (set &&& (1 <<< digit)) != 0
  defp remove_candidate(set, digit), do: set &&& bnot(1 <<< digit)

  # Count set bits (popcount)
  defp count_candidates(0), do: 0

  defp count_candidates(n) do
    count_candidates(n &&& (n - 1)) + 1
  end

  # Get first candidate digit from bitset (1-9)
  defp get_first_candidate(cs), do: get_first_candidate(cs, 1)
  defp get_first_candidate(_cs, 10), do: 0

  defp get_first_candidate(cs, digit) do
    if has_candidate?(cs, digit), do: digit, else: get_first_candidate(cs, digit + 1)
  end

  # Get all 20 peers for a cell (row, col, box)
  defp get_peers(row, col) do
    # Same row (8 cells)
    row_peers =
      for c <- 0..8, c != col do
        {row, c}
      end

    # Same column (8 cells)
    col_peers =
      for r <- 0..8, r != row do
        {r, col}
      end

    # Same 3x3 box (4 cells not already counted)
    box_row = div(row, 3) * 3
    box_col = div(col, 3) * 3

    box_peers =
      for r <- box_row..(box_row + 2),
          c <- box_col..(box_col + 2),
          r != row and c != col do
        {r, c}
      end

    row_peers ++ col_peers ++ box_peers
  end

  # Eliminate a digit from a cell's candidates
  defp eliminate(cand_agent, grid_agent, counter, row, col, digit) do
    cand = Agent.get(cand_agent, fn candidates ->
      Enum.at(Enum.at(candidates, row), col)
    end)

    # Check if digit is already eliminated
    if not has_candidate?(cand, digit) do
      :ok
    else
      # Remove digit from candidates
      Agent.update(cand_agent, fn candidates ->
        List.update_at(candidates, row, fn r ->
          List.update_at(r, col, fn c -> remove_candidate(c, digit) end)
        end)
      end)

      # Check for contradiction (no candidates left)
      new_cand = Agent.get(cand_agent, fn candidates ->
        Enum.at(Enum.at(candidates, row), col)
      end)

      remaining = count_candidates(new_cand)

      cond do
        remaining == 0 ->
          :error

        remaining == 1 ->
          grid_val = Agent.get(grid_agent, fn grid -> Enum.at(Enum.at(grid, row), col) end)

          if grid_val == 0 do
            # Only one candidate left - assign it (singleton elimination)
            last_digit = get_first_candidate(new_cand)
            assign(grid_agent, cand_agent, counter, row, col, last_digit)
          else
            :ok
          end

        true ->
          :ok
      end
    end
  end

  # Assign a digit to a cell
  defp assign(grid_agent, cand_agent, counter, row, col, digit) do
    # Increment iteration counter
    Agent.update(counter, &(&1 + 1))

    # Set value
    Agent.update(grid_agent, fn grid ->
      List.update_at(grid, row, fn r ->
        List.update_at(r, col, fn _ -> digit end)
      end)
    end)

    Agent.update(cand_agent, fn candidates ->
      List.update_at(candidates, row, fn r ->
        List.update_at(r, col, fn _ -> 1 <<< digit end)
      end)
    end)

    # Eliminate digit from all peers
    peers = get_peers(row, col)

    Enum.reduce_while(peers, :ok, fn {peer_row, peer_col}, _acc ->
      case eliminate(cand_agent, grid_agent, counter, peer_row, peer_col, digit) do
        :ok -> {:cont, :ok}
        :error -> {:halt, :error}
      end
    end)
  end

  # Propagate constraints until fixpoint (finds naked/hidden singles)
  defp propagate(grid_agent, cand_agent, counter) do
    case propagate_loop(grid_agent, cand_agent, counter) do
      {:error, _} -> :error
      {:ok, _} -> :ok
    end
  end

  defp propagate_loop(grid_agent, cand_agent, counter) do
    grid = Agent.get(grid_agent, & &1)
    candidates = Agent.get(cand_agent, & &1)

    # Strategy 1: Naked singles - cells with only one candidate
    naked_result = find_and_assign_naked_singles(grid, candidates, grid_agent, cand_agent, counter)
    case naked_result do
      {:error, _} -> {:error, :contradiction}
      {:changed, :ok} -> propagate_loop(grid_agent, cand_agent, counter)
      {:changed, :error} -> {:error, :contradiction}
      {:unchanged, _} ->
        # Strategy 2: Hidden singles in rows
        grid2 = Agent.get(grid_agent, & &1)
        candidates2 = Agent.get(cand_agent, & &1)
        row_result = find_and_assign_hidden_singles_rows(grid2, candidates2, grid_agent, cand_agent, counter)
        case row_result do
          {:error, _} -> {:error, :contradiction}
          {:changed, :ok} -> propagate_loop(grid_agent, cand_agent, counter)
          {:changed, :error} -> {:error, :contradiction}
          {:unchanged, _} ->
            # Strategy 3: Hidden singles in columns
            grid3 = Agent.get(grid_agent, & &1)
            candidates3 = Agent.get(cand_agent, & &1)
            col_result = find_and_assign_hidden_singles_cols(grid3, candidates3, grid_agent, cand_agent, counter)
            case col_result do
              {:error, _} -> {:error, :contradiction}
              {:changed, :ok} -> propagate_loop(grid_agent, cand_agent, counter)
              {:changed, :error} -> {:error, :contradiction}
              {:unchanged, _} ->
                # Strategy 4: Hidden singles in boxes
                grid4 = Agent.get(grid_agent, & &1)
                candidates4 = Agent.get(cand_agent, & &1)
                box_result = find_and_assign_hidden_singles_boxes(grid4, candidates4, grid_agent, cand_agent, counter)
                case box_result do
                  {:error, _} -> {:error, :contradiction}
                  {:changed, :ok} -> propagate_loop(grid_agent, cand_agent, counter)
                  {:changed, :error} -> {:error, :contradiction}
                  {:unchanged, _} -> {:ok, :fixpoint}
                end
            end
        end
    end
  end

  defp find_and_assign_naked_singles(grid, candidates, grid_agent, cand_agent, counter) do
    Enum.with_index(grid)
    |> Enum.reduce_while({:unchanged, :ok}, fn {row, r}, acc ->
      result = Enum.with_index(row)
      |> Enum.reduce_while(acc, fn {val, c}, inner_acc ->
        if val == 0 do
          cands = Enum.at(Enum.at(candidates, r), c)
          count = count_candidates(cands)
          cond do
            count == 0 -> {:halt, {:error, :contradiction}}
            count == 1 ->
              digit = get_first_candidate(cands)
              case assign(grid_agent, cand_agent, counter, r, c, digit) do
                :ok -> {:halt, {:changed, :ok}}
                :error -> {:halt, {:changed, :error}}
              end
            true -> {:cont, inner_acc}
          end
        else
          {:cont, inner_acc}
        end
      end)
      case result do
        {:unchanged, _} -> {:cont, result}
        {:changed, _} -> {:halt, result}
        {:error, _} -> {:halt, result}
      end
    end)
  end

  defp find_and_assign_hidden_singles_rows(grid, candidates, grid_agent, cand_agent, counter) do
    Enum.reduce_while(0..8, {:unchanged, :ok}, fn row, acc ->
      result = Enum.reduce_while(1..9, acc, fn digit, inner_acc ->
        # Check if digit is already assigned in this row
        already_assigned = Enum.any?(Enum.at(grid, row), fn val -> val == digit end)
        if already_assigned do
          {:cont, inner_acc}
        else
          # Count cells that can have this digit
          {count, last_col} = Enum.reduce(0..8, {0, -1}, fn col, {cnt, lcol} ->
            cands = Enum.at(Enum.at(candidates, row), col)
            if has_candidate?(cands, digit), do: {cnt + 1, col}, else: {cnt, lcol}
          end)
          cond do
            count == 0 -> {:halt, {:error, :contradiction}}
            count == 1 ->
              case assign(grid_agent, cand_agent, counter, row, last_col, digit) do
                :ok -> {:halt, {:changed, :ok}}
                :error -> {:halt, {:changed, :error}}
              end
            true -> {:cont, inner_acc}
          end
        end
      end)
      case result do
        {:unchanged, _} -> {:cont, result}
        {:changed, _} -> {:halt, result}
        {:error, _} -> {:halt, result}
      end
    end)
  end

  defp find_and_assign_hidden_singles_cols(grid, candidates, grid_agent, cand_agent, counter) do
    Enum.reduce_while(0..8, {:unchanged, :ok}, fn col, acc ->
      result = Enum.reduce_while(1..9, acc, fn digit, inner_acc ->
        # Check if digit is already assigned in this column
        already_assigned = Enum.any?(0..8, fn row -> Enum.at(Enum.at(grid, row), col) == digit end)
        if already_assigned do
          {:cont, inner_acc}
        else
          # Count cells that can have this digit
          {count, last_row} = Enum.reduce(0..8, {0, -1}, fn row, {cnt, lrow} ->
            cands = Enum.at(Enum.at(candidates, row), col)
            if has_candidate?(cands, digit), do: {cnt + 1, row}, else: {cnt, lrow}
          end)
          cond do
            count == 0 -> {:halt, {:error, :contradiction}}
            count == 1 ->
              case assign(grid_agent, cand_agent, counter, last_row, col, digit) do
                :ok -> {:halt, {:changed, :ok}}
                :error -> {:halt, {:changed, :error}}
              end
            true -> {:cont, inner_acc}
          end
        end
      end)
      case result do
        {:unchanged, _} -> {:cont, result}
        {:changed, _} -> {:halt, result}
        {:error, _} -> {:halt, result}
      end
    end)
  end

  defp find_and_assign_hidden_singles_boxes(grid, candidates, grid_agent, cand_agent, counter) do
    Enum.reduce_while(0..8, {:unchanged, :ok}, fn box, acc ->
      box_row = div(box, 3) * 3
      box_col = rem(box, 3) * 3

      result = Enum.reduce_while(1..9, acc, fn digit, inner_acc ->
        # Check if digit is already assigned in this box
        already_assigned = Enum.any?(box_row..(box_row + 2), fn r ->
          Enum.any?(box_col..(box_col + 2), fn c ->
            Enum.at(Enum.at(grid, r), c) == digit
          end)
        end)
        if already_assigned do
          {:cont, inner_acc}
        else
          # Count cells that can have this digit
          {count, last_r, last_c} = Enum.reduce(box_row..(box_row + 2), {0, -1, -1}, fn r, {cnt, lr, lc} ->
            Enum.reduce(box_col..(box_col + 2), {cnt, lr, lc}, fn c, {cnt2, lr2, lc2} ->
              cands = Enum.at(Enum.at(candidates, r), c)
              if has_candidate?(cands, digit), do: {cnt2 + 1, r, c}, else: {cnt2, lr2, lc2}
            end)
          end)
          cond do
            count == 0 -> {:halt, {:error, :contradiction}}
            count == 1 ->
              case assign(grid_agent, cand_agent, counter, last_r, last_c, digit) do
                :ok -> {:halt, {:changed, :ok}}
                :error -> {:halt, {:changed, :error}}
              end
            true -> {:cont, inner_acc}
          end
        end
      end)
      case result do
        {:unchanged, _} -> {:cont, result}
        {:changed, _} -> {:halt, result}
        {:error, _} -> {:halt, result}
      end
    end)
  end

  # Search with backtracking
  defp search(grid_agent, cand_agent, counter) do
    # Find cell with minimum remaining values (MRV heuristic)
    case find_mrv_cell(grid_agent, cand_agent) do
      nil ->
        # Grid is complete
        :ok

      {row, col, cands} ->
        # Try each candidate
        try_candidates(grid_agent, cand_agent, counter, row, col, cands, 1)
    end
  end

  defp try_candidates(_grid_agent, _cand_agent, _counter, _row, _col, _cands, 10), do: :error

  defp try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit) do
    if has_candidate?(cands, digit) do
      # Save state before attempting (but NOT the counter - iterations always count forward)
      saved_grid = Agent.get(grid_agent, & &1)
      saved_cands = Agent.get(cand_agent, & &1)

      case assign(grid_agent, cand_agent, counter, row, col, digit) do
        :ok ->
          # After assignment, run propagate to find more singles (like C does)
          case propagate(grid_agent, cand_agent, counter) do
            :ok ->
              case search(grid_agent, cand_agent, counter) do
                :ok ->
                  :ok

                :error ->
                  # Restore grid state and try next digit (counter NOT restored)
                  Agent.update(grid_agent, fn _ -> saved_grid end)
                  Agent.update(cand_agent, fn _ -> saved_cands end)
                  try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit + 1)
              end

            :error ->
              # Propagation failed, restore state and try next digit
              Agent.update(grid_agent, fn _ -> saved_grid end)
              Agent.update(cand_agent, fn _ -> saved_cands end)
              try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit + 1)
          end

        :error ->
          # Assignment failed, restore state and try next digit
          Agent.update(grid_agent, fn _ -> saved_grid end)
          Agent.update(cand_agent, fn _ -> saved_cands end)
          try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit + 1)
      end
    else
      try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit + 1)
    end
  end

  # Find cell with minimum remaining values (MRV heuristic)
  defp find_mrv_cell(grid_agent, cand_agent) do
    grid = Agent.get(grid_agent, & &1)
    candidates = Agent.get(cand_agent, & &1)

    Enum.with_index(grid)
    |> Enum.reduce(nil, fn {row, r}, best ->
      Enum.with_index(row)
      |> Enum.reduce(best, fn {val, c}, acc ->
        if val == 0 do
          cands = Enum.at(Enum.at(candidates, r), c)
          count = count_candidates(cands)

          case acc do
            nil -> {r, c, cands, count}
            {_br, _bc, _bcands, best_count} when count < best_count -> {r, c, cands, count}
            _ -> acc
          end
        else
          acc
        end
      end)
    end)
    |> case do
      nil -> nil
      {r, c, cands, _count} -> {r, c, cands}
    end
  end
end

System.argv() |> CP.main()
