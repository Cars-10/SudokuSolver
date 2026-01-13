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

        # Assign given clues
        assign_clues(puzzle, grid_agent, cand_agent, counter)

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

  # Initialize grid (9x9 nested lists)
  defp init_grid(_puzzle) do
    List.duplicate(List.duplicate(0, 9), 9)
  end

  # Initialize candidates (bitsets, bits 1-9)
  defp init_candidates(_puzzle) do
    # 0x3FE = 0011 1111 1110 (bits 1-9 set)
    List.duplicate(List.duplicate(0x3FE, 9), 9)
  end

  # Assign given clues
  defp assign_clues(puzzle, grid_agent, cand_agent, counter) do
    Enum.with_index(puzzle)
    |> Enum.each(fn {row, r} ->
      Enum.with_index(row)
      |> Enum.each(fn {digit, c} ->
        if digit != 0 do
          case assign(grid_agent, cand_agent, counter, r, c, digit) do
            :ok -> :ok
            :error -> raise "Failed to assign clue #{digit} at (#{r}, #{c})"
          end
        end
      end)
    end)
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
      # Save state before attempting
      saved_grid = Agent.get(grid_agent, & &1)
      saved_cands = Agent.get(cand_agent, & &1)
      saved_counter = Agent.get(counter, & &1)

      case assign(grid_agent, cand_agent, counter, row, col, digit) do
        :ok ->
          case search(grid_agent, cand_agent, counter) do
            :ok ->
              :ok

            :error ->
              # Restore state and try next digit
              Agent.update(grid_agent, fn _ -> saved_grid end)
              Agent.update(cand_agent, fn _ -> saved_cands end)
              Agent.update(counter, fn _ -> saved_counter end)
              try_candidates(grid_agent, cand_agent, counter, row, col, cands, digit + 1)
          end

        :error ->
          # Assignment failed, restore state and try next digit
          Agent.update(grid_agent, fn _ -> saved_grid end)
          Agent.update(cand_agent, fn _ -> saved_cands end)
          Agent.update(counter, fn _ -> saved_counter end)
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
