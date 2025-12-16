defmodule Sudoku do
  def main(args) do
    if Enum.empty?(args) do
      IO.puts "Usage: elixir Sudoku.exs <file1> <file2> ..."
    else
      Enum.each(args, &process_file/1)
    end
  end

  def process_file(filename) do
    IO.puts "\nProcessing #{filename}"
    case read_board(filename) do
      {:ok, board} ->
        print_board(board)
        {:ok, _} = Agent.start_link(fn -> 0 end, name: :iteration_counter)
        case solve(board, 0, 0) do
          {:ok, solved_board} ->
            print_board(solved_board)
            iterations = Agent.get(:iteration_counter, fn state -> state end)
            Agent.stop(:iteration_counter)
            IO.puts "\nSolved in Iterations=#{iterations}"
          :error ->
            Agent.stop(:iteration_counter)
            IO.puts "No solution found"
        end
      {:error, reason} ->
        IO.puts "Error reading file #{filename}: #{reason}"
    end
  end

  def read_board(filename) do
    case File.read(filename) do
      {:ok, content} ->
        board = content
                |> String.split("\n", trim: true)
                |> Enum.reject(&String.starts_with?(&1, "#"))
                |> Enum.map(fn line ->
                  line
                  |> String.split(~r/\s+/, trim: true)
                  |> Enum.map(&String.to_integer/1)
                  |> List.to_tuple()
                end)
                |> List.to_tuple()
        {:ok, board}
      error -> error
    end
  end

  def print_board(board) do
    IO.puts "Puzzle:"
    0..8 |> Enum.each(fn r ->
      0..8 |> Enum.each(fn c ->
        IO.write "#{elem(elem(board, r), c)} "
      end)
      IO.puts ""
    end)
  end

  def solve(board, 9, _), do: {:ok, board}
  def solve(board, r, 9), do: solve(board, r + 1, 0)
  def solve(board, r, c) do
    if elem(elem(board, r), c) != 0 do
      solve(board, r, c + 1)
    else
      try_numbers(board, r, c, 1)
    end
  end

  def try_numbers(_board, _r, _c, 10), do: :error
  def try_numbers(board, r, c, num) do
    Agent.update(:iteration_counter, fn state -> state + 1 end)
    if is_possible(board, r, c, num) do
      new_row = put_elem(elem(board, r), c, num)
      new_board = put_elem(board, r, new_row)
      case solve(new_board, r, c + 1) do
        {:ok, solved_board} -> {:ok, solved_board}
        :error -> try_numbers(board, r, c, num + 1)
      end
    else
      try_numbers(board, r, c, num + 1)
    end
  end

  def is_possible(board, r, c, num) do
    row_valid = 0..8 |> Enum.all?(fn i -> elem(elem(board, r), i) != num end)
    col_valid = 0..8 |> Enum.all?(fn i -> elem(elem(board, i), c) != num end)
    
    start_r = div(r, 3) * 3
    start_c = div(c, 3) * 3
    box_valid = 0..2 |> Enum.all?(fn i ->
      0..2 |> Enum.all?(fn j ->
        elem(elem(board, start_r + i), start_c + j) != num
      end)
    end)

    row_valid and col_valid and box_valid
  end
end

System.argv() |> Sudoku.main()
