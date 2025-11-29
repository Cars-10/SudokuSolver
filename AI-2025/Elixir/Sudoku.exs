defmodule Sudoku do
  def main(args) do
    start_time = System.monotonic_time(:millisecond)
    Enum.each(args, fn arg ->
      if String.ends_with?(arg, ".matrix") do
        process_file(arg)
      end
    end)
    end_time = System.monotonic_time(:millisecond)
    IO.puts("Seconds to process #{(end_time - start_time) / 1000.0}")
  end

  def process_file(filename) do
    IO.puts(filename)
    {:ok, content} = File.read(filename)
    board = parse_board(content)
    print_board(board)
    {solved_board, count} = solve(board)
    print_board(solved_board)
    IO.puts("Solved in Iterations=#{count}\n")
  end

  def parse_board(content) do
    content
    |> String.split("\n", trim: true)
    |> Enum.reject(&String.starts_with?(&1, "#"))
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.filter(&(length(&1) == 9))
    |> Enum.take(9)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {row_vals, row_idx}, acc ->
      row_vals
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {val, col_idx}, acc2 ->
        Map.put(acc2, {row_idx, col_idx}, val)
      end)
    end)
  end

  def print_board(board) do
    IO.puts("\nPuzzle:")
    for row <- 0..8 do
      for col <- 0..8 do
        IO.write("#{Map.get(board, {row, col})} ")
      end
      IO.puts("")
    end
  end

  def solve(board), do: solve(board, 0)

  def solve(board, count) do
    case find_empty(board) do
      nil -> {board, count}
      {row, col} -> try_values(board, row, col, 1, count)
    end
  end

  def try_values(_board, _row, _col, 10, count), do: {nil, count}
  def try_values(board, row, col, val, count) do
    new_count = count + 1
    if is_possible(board, row, col, val) do
      new_board = Map.put(board, {row, col}, val)
      case solve(new_board, new_count) do
        {nil, final_count} -> try_values(board, row, col, val + 1, final_count)
        {solved, final_count} -> {solved, final_count}
      end
    else
      try_values(board, row, col, val + 1, new_count)
    end
  end

  def find_empty(board) do
    Enum.find_value(0..8, fn row ->
      Enum.find_value(0..8, fn col ->
        if Map.get(board, {row, col}) == 0, do: {row, col}, else: nil
      end)
    end)
  end

  def is_possible(board, row, col, val) do
    not (in_row(board, row, val) or in_col(board, col, val) or in_box(board, row, col, val))
  end

  def in_row(board, row, val) do
    Enum.any?(0..8, fn c -> Map.get(board, {row, c}) == val end)
  end

  def in_col(board, col, val) do
    Enum.any?(0..8, fn r -> Map.get(board, {r, col}) == val end)
  end

  def in_box(board, row, col, val) do
    r0 = div(row, 3) * 3
    c0 = div(col, 3) * 3
    Enum.any?(r0..(r0 + 2), fn r ->
      Enum.any?(c0..(c0 + 2), fn c ->
        Map.get(board, {r, c}) == val
      end)
    end)
  end
end

Sudoku.main(System.argv())
