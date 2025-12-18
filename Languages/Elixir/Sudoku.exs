# Sudoku Solver - Elixir Implementation
# Brute-force backtracking algorithm matching C reference exactly.
#
# Algorithm:
# - Row-major search for empty cells (top-to-bottom, left-to-right)
# - Try values 1-9 in ascending order
# - Count EVERY placement attempt (algorithm fingerprint)
#
# Uses Agent for mutable iteration counter (Elixir is immutable)

defmodule Sudoku do
  def main(args) do
    start_time = System.monotonic_time(:millisecond)

    args
    |> Enum.filter(&String.ends_with?(&1, ".matrix"))
    |> Enum.each(&process_file/1)

    end_time = System.monotonic_time(:millisecond)
    elapsed = (end_time - start_time) / 1000.0
    IO.puts("Seconds to process #{:erlang.float_to_binary(elapsed, decimals: 3)}")
  end

  defp process_file(filename) do
    # Normalize path for output (match C format)
    display_path =
      if String.starts_with?(filename, "/app/Matrices/") do
        "../" <> String.slice(filename, 5..-1//1)
      else
        filename
      end

    IO.puts(display_path)

    case read_board(filename) do
      {:ok, board} ->
        # Start iteration counter agent
        {:ok, counter} = Agent.start_link(fn -> 0 end)

        print_board(board)

        case solve(board, counter) do
          {:ok, solved_board} ->
            print_board(solved_board)
            iterations = Agent.get(counter, & &1)
            Agent.stop(counter)
            IO.puts("\nSolved in Iterations=#{iterations}\n")

          :error ->
            Agent.stop(counter)
            IO.puts("No solution found")
        end

      {:error, reason} ->
        IO.puts("Error reading file: #{reason}")
    end
  end

  defp read_board(filename) do
    case File.read(filename) do
      {:ok, content} ->
        lines =
          content
          |> String.split("\n", trim: true)
          |> Enum.reject(&String.starts_with?(&1, "#"))
          |> Enum.take(9)

        # Parse and print each line (match C format - prints while reading)
        board =
          lines
          |> Enum.map(fn line ->
            values =
              line
              |> String.split(~r/\s+/, trim: true)
              |> Enum.map(&String.to_integer/1)
              |> List.to_tuple()

            # Print row as we read it
            row_str = values |> Tuple.to_list() |> Enum.join(" ")
            IO.puts("#{row_str} ")
            values
          end)
          |> List.to_tuple()

        {:ok, board}

      error ->
        error
    end
  end

  defp print_board(board) do
    IO.puts("\nPuzzle:")

    Enum.each(0..8, fn r ->
      row_str =
        Enum.map(0..8, fn c ->
          Integer.to_string(elem(elem(board, r), c))
        end)
        |> Enum.join(" ")

      IO.puts("#{row_str} ")
    end)
  end

  defp solve(board, counter) do
    case find_empty(board) do
      nil ->
        {:ok, board}

      {r, c} ->
        try_values(board, r, c, 1, counter)
    end
  end

  defp try_values(_board, _r, _c, 10, _counter), do: :error

  defp try_values(board, r, c, num, counter) do
    # COUNT EVERY ATTEMPT - algorithm fingerprint
    Agent.update(counter, &(&1 + 1))

    if is_valid?(board, r, c, num) do
      new_board = set_cell(board, r, c, num)

      case solve(new_board, counter) do
        {:ok, solved_board} -> {:ok, solved_board}
        :error -> try_values(board, r, c, num + 1, counter)
      end
    else
      try_values(board, r, c, num + 1, counter)
    end
  end

  defp find_empty(board) do
    Enum.find_value(0..8, fn r ->
      Enum.find_value(0..8, fn c ->
        if elem(elem(board, r), c) == 0, do: {r, c}
      end)
    end)
  end

  defp is_valid?(board, r, c, num) do
    # Check row
    row_valid = Enum.all?(0..8, fn i -> elem(elem(board, r), i) != num end)

    # Check column
    col_valid = row_valid && Enum.all?(0..8, fn i -> elem(elem(board, i), c) != num end)

    # Check 3x3 box
    if col_valid do
      start_r = div(r, 3) * 3
      start_c = div(c, 3) * 3

      Enum.all?(0..2, fn i ->
        Enum.all?(0..2, fn j ->
          elem(elem(board, start_r + i), start_c + j) != num
        end)
      end)
    else
      false
    end
  end

  defp set_cell(board, r, c, num) do
    new_row = put_elem(elem(board, r), c, num)
    put_elem(board, r, new_row)
  end
end

System.argv() |> Sudoku.main()
