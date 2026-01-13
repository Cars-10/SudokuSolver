#!/usr/bin/env elixir
# Elixir Dancing Links (DLX) Sudoku Solver
# Port of OCaml/C DLX implementation using ETS for mutable node storage

defmodule DLX do
  # Node structure stored in ETS: {id, left, right, up, down, column, size, row_id, col_id}

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

        # Create ETS table for nodes: {id, {left, right, up, down, column, size, row_id, col_id}}
        nodes = :ets.new(:dlx_nodes, [:set, :public])

        # Create iteration counter
        {:ok, counter} = Agent.start_link(fn -> 0 end)

        # Create solution array
        {:ok, solution_agent} = Agent.start_link(fn -> List.duplicate(-1, 81) end)

        # Build DLX matrix
        {root_id, row_metadata, clue_rows} = build_dlx_matrix(nodes, puzzle)

        # Cover clue rows before search
        cover_clue_rows(nodes, clue_rows)

        # Solve
        if dlx_search(nodes, root_id, 0, solution_agent, counter) do
          solution = Agent.get(solution_agent, & &1)
          solved = decode_solution(solution, puzzle, row_metadata)
          print_puzzle(solved)
          iterations = Agent.get(counter, & &1)
          IO.puts("\nSolved in Iterations=#{iterations}\n")
        else
          IO.puts("No solution found")
        end

        Agent.stop(counter)
        Agent.stop(solution_agent)
        :ets.delete(nodes)

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

  # ETS node operations
  defp make_node(nodes, id, row_id, col_id) do
    # {left, right, up, down, column, size, row_id, col_id}
    node = {id, id, id, id, id, 0, row_id, col_id}
    :ets.insert(nodes, {id, node})
    id
  end

  defp get_node(nodes, id) do
    case :ets.lookup(nodes, id) do
      [{^id, node}] -> node
      [] -> raise "Node #{id} not found"
    end
  end

  defp update_node(nodes, id, node) do
    :ets.insert(nodes, {id, node})
  end

  defp node_left({left, _, _, _, _, _, _, _}), do: left
  defp node_right({_, right, _, _, _, _, _, _}), do: right
  defp node_up({_, _, up, _, _, _, _, _}), do: up
  defp node_down({_, _, _, down, _, _, _, _}), do: down
  defp node_column({_, _, _, _, column, _, _, _}), do: column
  defp node_size({_, _, _, _, _, size, _, _}), do: size
  defp node_row_id({_, _, _, _, _, _, row_id, _}), do: row_id

  defp set_left(node, left), do: {left, elem(node, 1), elem(node, 2), elem(node, 3), elem(node, 4), elem(node, 5), elem(node, 6), elem(node, 7)}
  defp set_right(node, right), do: {elem(node, 0), right, elem(node, 2), elem(node, 3), elem(node, 4), elem(node, 5), elem(node, 6), elem(node, 7)}
  defp set_up(node, up), do: {elem(node, 0), elem(node, 1), up, elem(node, 3), elem(node, 4), elem(node, 5), elem(node, 6), elem(node, 7)}
  defp set_down(node, down), do: {elem(node, 0), elem(node, 1), elem(node, 2), down, elem(node, 4), elem(node, 5), elem(node, 6), elem(node, 7)}
  defp set_column(node, col), do: {elem(node, 0), elem(node, 1), elem(node, 2), elem(node, 3), col, elem(node, 5), elem(node, 6), elem(node, 7)}
  defp set_size(node, size), do: {elem(node, 0), elem(node, 1), elem(node, 2), elem(node, 3), elem(node, 4), size, elem(node, 6), elem(node, 7)}

  # Cover a column in the DLX matrix
  defp cover_column(nodes, col_id) do
    col = get_node(nodes, col_id)
    right_id = node_right(col)
    left_id = node_left(col)

    # Remove column header from the header list
    right = get_node(nodes, right_id)
    left = get_node(nodes, left_id)
    update_node(nodes, right_id, set_left(right, left_id))
    update_node(nodes, left_id, set_right(left, right_id))

    # Cover all rows in this column
    cover_rows(nodes, node_down(col), col_id)
  end

  defp cover_rows(_nodes, row_id, col_id) when row_id == col_id, do: :ok

  defp cover_rows(nodes, row_id, col_id) do
    row_node = get_node(nodes, row_id)
    cover_row_nodes(nodes, node_right(row_node), row_id)
    cover_rows(nodes, node_down(row_node), col_id)
  end

  defp cover_row_nodes(_nodes, node_id, row_id) when node_id == row_id, do: :ok

  defp cover_row_nodes(nodes, node_id, row_id) do
    node = get_node(nodes, node_id)
    up_id = node_up(node)
    down_id = node_down(node)
    col_id = node_column(node)

    # Remove node from column
    down = get_node(nodes, down_id)
    up = get_node(nodes, up_id)
    col = get_node(nodes, col_id)

    update_node(nodes, down_id, set_up(down, up_id))
    update_node(nodes, up_id, set_down(up, down_id))
    update_node(nodes, col_id, set_size(col, node_size(col) - 1))

    cover_row_nodes(nodes, node_right(node), row_id)
  end

  # Uncover a column (exact reverse of cover)
  defp uncover_column(nodes, col_id) do
    col = get_node(nodes, col_id)

    # Uncover all rows in this column (in reverse order)
    uncover_rows(nodes, node_up(col), col_id)

    # Restore column header to the header list
    right_id = node_right(col)
    left_id = node_left(col)
    right = get_node(nodes, right_id)
    left = get_node(nodes, left_id)
    update_node(nodes, right_id, set_left(right, col_id))
    update_node(nodes, left_id, set_right(left, col_id))
  end

  defp uncover_rows(_nodes, row_id, col_id) when row_id == col_id, do: :ok

  defp uncover_rows(nodes, row_id, col_id) do
    row_node = get_node(nodes, row_id)
    uncover_row_nodes(nodes, node_left(row_node), row_id)
    uncover_rows(nodes, node_up(row_node), col_id)
  end

  defp uncover_row_nodes(_nodes, node_id, row_id) when node_id == row_id, do: :ok

  defp uncover_row_nodes(nodes, node_id, row_id) do
    node = get_node(nodes, node_id)
    up_id = node_up(node)
    down_id = node_down(node)
    col_id = node_column(node)

    # Restore node to column
    col = get_node(nodes, col_id)
    down = get_node(nodes, down_id)
    up = get_node(nodes, up_id)

    update_node(nodes, col_id, set_size(col, node_size(col) + 1))
    update_node(nodes, down_id, set_up(down, node_id))
    update_node(nodes, up_id, set_down(up, node_id))

    uncover_row_nodes(nodes, node_left(node), row_id)
  end

  # Choose column with minimum size (Knuth's S heuristic)
  defp choose_column(nodes, root_id) do
    root = get_node(nodes, root_id)
    choose_column_scan(nodes, node_right(root), root_id, nil, :infinity)
  end

  defp choose_column_scan(_nodes, col_id, root_id, best, _best_size) when col_id == root_id do
    best
  end

  defp choose_column_scan(nodes, col_id, root_id, best, best_size) do
    col = get_node(nodes, col_id)
    size = node_size(col)

    if size < best_size do
      choose_column_scan(nodes, node_right(col), root_id, col_id, size)
    else
      choose_column_scan(nodes, node_right(col), root_id, best, best_size)
    end
  end

  # DLX Search - Algorithm X with Dancing Links
  defp dlx_search(nodes, root_id, k, solution_agent, counter) do
    Agent.update(counter, &(&1 + 1))

    root = get_node(nodes, root_id)

    # If matrix is empty, we found a solution
    if node_right(root) == root_id do
      true
    else
      # Choose column with minimum size
      case choose_column(nodes, root_id) do
        nil ->
          false

        col_id ->
          col = get_node(nodes, col_id)

          # If column has no rows, no solution possible
          if node_size(col) == 0 do
            false
          else
            # Cover this column
            cover_column(nodes, col_id)

            # Try each row in this column
            result = try_rows(nodes, node_down(col), col_id, root_id, k, solution_agent, counter)

            # Uncover column
            uncover_column(nodes, col_id)

            result
          end
      end
    end
  end

  defp try_rows(_nodes, row_id, col_id, _root_id, _k, _solution_agent, _counter)
       when row_id == col_id do
    false
  end

  defp try_rows(nodes, row_id, col_id, root_id, k, solution_agent, counter) do
    row_node = get_node(nodes, row_id)

    # Add row to partial solution
    Agent.update(solution_agent, fn solution ->
      List.replace_at(solution, k, node_row_id(row_node))
    end)

    # Cover all other columns in this row
    cover_row_columns(nodes, node_right(row_node), row_id)

    # Recurse
    if dlx_search(nodes, root_id, k + 1, solution_agent, counter) do
      true
    else
      # Backtrack: uncover all columns in this row
      uncover_row_columns(nodes, node_left(row_node), row_id)

      # Try next row
      try_rows(nodes, node_down(row_node), col_id, root_id, k, solution_agent, counter)
    end
  end

  defp cover_row_columns(_nodes, node_id, row_id) when node_id == row_id, do: :ok

  defp cover_row_columns(nodes, node_id, row_id) do
    node = get_node(nodes, node_id)
    cover_column(nodes, node_column(node))
    cover_row_columns(nodes, node_right(node), row_id)
  end

  defp uncover_row_columns(_nodes, node_id, row_id) when node_id == row_id, do: :ok

  defp uncover_row_columns(nodes, node_id, row_id) do
    node = get_node(nodes, node_id)
    uncover_column(nodes, node_column(node))
    uncover_row_columns(nodes, node_left(node), row_id)
  end

  # Cover all columns in clue rows before search
  defp cover_clue_rows(nodes, clue_rows) do
    Enum.each(clue_rows, fn {_row_id, first_node_id} ->
      cover_clue_row(nodes, first_node_id, first_node_id)
    end)
  end

  defp cover_clue_row(nodes, node_id, first_node_id) do
    node = get_node(nodes, node_id)
    cover_column(nodes, node_column(node))

    next_id = node_right(node)
    if next_id != first_node_id do
      cover_clue_row(nodes, next_id, first_node_id)
    end
  end

  # Build DLX matrix for Sudoku
  defp build_dlx_matrix(nodes, puzzle) do
    # Create root node (id 0)
    root_id = 0
    make_node(nodes, root_id, -1, -1)

    # Create 324 column headers for Sudoku constraints
    num_cols = 324
    col_base = 1

    # Create column headers
    Enum.each(0..(num_cols - 1), fn i ->
      col_id = col_base + i
      make_node(nodes, col_id, -1, i)

      # Set column as its own column header
      col = get_node(nodes, col_id)
      update_node(nodes, col_id, set_column(col, col_id))
    end)

    # Link column headers horizontally
    Enum.each(0..(num_cols - 1), fn i ->
      col_id = col_base + i
      left_id = if i == 0, do: root_id, else: col_base + i - 1
      right_id = if i == num_cols - 1, do: root_id, else: col_base + i + 1

      col = get_node(nodes, col_id)
      col = set_left(col, left_id)
      col = set_right(col, right_id)
      update_node(nodes, col_id, col)

      if i == 0 do
        root = get_node(nodes, root_id)
        update_node(nodes, root_id, set_right(root, col_id))
      end

      if i == num_cols - 1 do
        root = get_node(nodes, root_id)
        update_node(nodes, root_id, set_left(root, col_id))
      end
    end)

    # Build rows
    node_id_counter = col_base + num_cols
    {_final_node_id, _row_count, row_metadata, clue_rows} =
      Enum.reduce(0..8, {node_id_counter, 0, %{}, []}, fn r, acc1 ->
        Enum.reduce(0..8, acc1, fn c, {node_id, row_count, meta, clues} ->
          cell_val = Enum.at(Enum.at(puzzle, r), c)

          if cell_val != 0 do
            # Cell has a clue - create only one row for that value
            d = cell_val
            {new_node_id, new_meta, first_node_id} = create_row(nodes, node_id, row_count, r, c, d, col_base, meta)
            {new_node_id, row_count + 1, new_meta, [{row_count, first_node_id} | clues]}
          else
            # Cell is empty - create rows for all possible values 1-9
            {final_node_id, final_count, final_meta} =
              Enum.reduce(1..9, {node_id, row_count, meta}, fn d, {nid, rc, m} ->
                {new_nid, new_m, _first} = create_row(nodes, nid, rc, r, c, d, col_base, m)
                {new_nid, rc + 1, new_m}
              end)

            {final_node_id, final_count, final_meta, clues}
          end
        end)
      end)

    {root_id, row_metadata, Enum.reverse(clue_rows)}
  end

  defp create_row(nodes, node_id, row_id, r, c, d, col_base, row_metadata) do
    # Calculate constraint column indices
    cell_col = col_base + r * 9 + c
    row_col = col_base + 81 + r * 9 + (d - 1)
    col_col = col_base + 162 + c * 9 + (d - 1)
    box_col = col_base + 243 + (div(r, 3) * 3 + div(c, 3)) * 9 + (d - 1)

    # Create 4 nodes
    node1_id = node_id
    node2_id = node_id + 1
    node3_id = node_id + 2
    node4_id = node_id + 3

    make_node(nodes, node1_id, row_id, -1)
    make_node(nodes, node2_id, row_id, -1)
    make_node(nodes, node3_id, row_id, -1)
    make_node(nodes, node4_id, row_id, -1)

    # Link nodes horizontally in a circular list
    node1 = get_node(nodes, node1_id)
    node1 = set_right(node1, node2_id)
    node1 = set_left(node1, node4_id)
    update_node(nodes, node1_id, node1)

    node2 = get_node(nodes, node2_id)
    node2 = set_right(node2, node3_id)
    node2 = set_left(node2, node1_id)
    update_node(nodes, node2_id, node2)

    node3 = get_node(nodes, node3_id)
    node3 = set_right(node3, node4_id)
    node3 = set_left(node3, node2_id)
    update_node(nodes, node3_id, node3)

    node4 = get_node(nodes, node4_id)
    node4 = set_right(node4, node1_id)
    node4 = set_left(node4, node3_id)
    update_node(nodes, node4_id, node4)

    # Insert each node into its column
    insert_into_column(nodes, node1_id, cell_col)
    insert_into_column(nodes, node2_id, row_col)
    insert_into_column(nodes, node3_id, col_col)
    insert_into_column(nodes, node4_id, box_col)

    new_metadata = Map.put(row_metadata, row_id, {r, c, d})

    {node_id + 4, new_metadata, node1_id}
  end

  defp insert_into_column(nodes, node_id, col_id) do
    node = get_node(nodes, node_id)
    col = get_node(nodes, col_id)

    # Set column reference
    node = set_column(node, col_id)

    # Insert at top of column (before col, after col.up)
    col_up_id = node_up(col)
    node = set_up(node, col_up_id)
    node = set_down(node, col_id)
    update_node(nodes, node_id, node)

    up = get_node(nodes, col_up_id)
    update_node(nodes, col_up_id, set_down(up, node_id))

    col = set_up(col, node_id)
    col = set_size(col, node_size(col) + 1)
    update_node(nodes, col_id, col)
  end

  # Decode solution from row IDs to Sudoku grid
  defp decode_solution(solution, puzzle, row_metadata) do
    # Start with given clues
    grid =
      Enum.map(puzzle, fn row ->
        Enum.map(row, & &1)
      end)

    # Decode each solution row using metadata
    Enum.reduce(solution, grid, fn row_id, acc ->
      case Map.get(row_metadata, row_id) do
        {r, c, d} when row_id >= 0 ->
          List.update_at(acc, r, fn row ->
            List.update_at(row, c, fn _ -> d end)
          end)

        _ ->
          acc
      end
    end)
  end
end

System.argv() |> DLX.main()
