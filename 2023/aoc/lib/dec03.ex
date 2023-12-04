defmodule Dec03 do
  @doc """
  iex> Dec03.parse_part_locations("467..114..\\n...*......\\n..35..633.\\n......#...\\n617*......\\n.....+.58.\\n..592.....\\n......755.\\n...$.*....\\n.664.598..\\n")
  %{{3,1} => "*", {6,3} => "#", {3,4} => "*", {5,5} => "+", {3,8} => "$", {5,8} => "*"}
  """
  def parse_part_locations(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> !String.contains?(".0123456789", char) end)
      |> Enum.map(fn {char, x} -> {{x, y}, char} end)
    end)
    |> List.flatten()
    |> Enum.into(%{})
  end

  @doc """
  iex> Dec03.parse_number_locations_line([{{0, 0}, 4}, {{1, 0}, 6}, {{2, 0}, 7}, {{5, 0}, 1}, {{6, 0}, 1}, {{7, 0}, 4}])
  [{467, [{0, 0}, {1, 0}, {2, 0}]}, {114, [{5,0},{6,0},{7,0}]}]
  """
  def parse_number_locations_line(data) do
    data
    |> Enum.reduce([], fn
      {{x, y}, n}, acc ->
        cond do
          acc
          |> Enum.map(fn {_, cs} -> cs end)
          |> Enum.any?(fn cs ->
            cs |> Enum.any?(fn {u, _} -> u == x - 1 end)
          end) ->
            {m, cs} =
              Enum.find(acc, fn {_, cs} -> cs |> Enum.any?(fn {u, _} -> u == x - 1 end) end)

            (acc -- [{m, cs}]) ++ [{10 * m + n, (cs ++ [{x, y}]) |> Enum.sort()}]

          true ->
            acc ++ [{n, [{x, y}]}]
        end
    end)
  end

  @doc """
  iex> Dec03.parse_part_number_locations("467..114..\\n...*......\\n..35..633.\\n......#...\\n617*......\\n.....+.58.\\n..592.....\\n......755.\\n...$.*....\\n.664.598..\\n")
  [
    {467, [{0,0},{1,0},{2,0}]},
    {114, [{5,0},{6,0},{7,0}]},
    {35, [{2,2},{3,2}]},
    {633, [{6,2},{7,2},{8,2}]},
    {617, [{0,4},{1,4},{2,4}]},
    {58, [{7,5},{8,5}]},
    {592, [{2,6},{3,6},{4,6}]},
    {755, [{6,7},{7,7},{8,7}]},
    {664, [{1,9},{2,9},{3,9}]},
    {598, [{5,9},{6,9},{7,9}]}
  ]
  """
  def parse_part_number_locations(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.map(fn {line, y} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.filter(fn {char, _} -> String.contains?("0123456789", char) end)
      |> Enum.map(fn {char, x} -> {{x, y}, String.to_integer(char)} end)
    end)
    |> Enum.flat_map(&parse_number_locations_line/1)
  end

  @doc """
  iex> Dec03.part_numbers({3,1}, [{467, [{0,0},{1,0},{2,0}]}, {114, [{5,0},{6,0},{7,0}]}, {35, [{2,2},{3,2}]}, {633, [{6,2},{7,2},{8,2}]}, {617, [{0,4},{1,4},{2,4}]}, {58, [{7,5},{8,5}]}, {592, [{2,6},{3,6},{4,6}]}, {755, [{6,7},{7,7},{8,7}]}, {664, [{1,9},{2,9},{3,9}]}, {598, [{5,9},{6,9},{7,9}]}])
  [467, 35]
  """
  def part_numbers({x, y}, numbers) do
    overlaps =
      [
        {x - 1, y - 1},
        {x, y - 1},
        {x + 1, y - 1},
        {x - 1, y},
        {x + 1, y},
        {x - 1, y + 1},
        {x, y + 1},
        {x + 1, y + 1}
      ]
      |> MapSet.new()

    numbers
    |> Enum.filter(fn {_, coords} -> !MapSet.disjoint?(overlaps, coords |> MapSet.new()) end)
    |> Enum.map(fn {n, _} -> n end)
  end

  @doc """
  iex> Dec03.a("467..114..\\n...*......\\n..35..633.\\n......#...\\n617*......\\n.....+.58.\\n..592.....\\n......755.\\n...$.*....\\n.664.598..\\n")
  4361
  """
  def a(input) do
    parts = parse_part_locations(input)
    numbers = parse_part_number_locations(input)

    parts
    |> Enum.flat_map(fn {p, _} -> part_numbers(p, numbers) end)
    |> Enum.sum()
  end

  @doc """
  iex> Dec03.b("467..114..\\n...*......\\n..35..633.\\n......#...\\n617*......\\n.....+.58.\\n..592.....\\n......755.\\n...$.*....\\n.664.598..\\n")
  467835
  """
  def b(input) do
    parts = parse_part_locations(input)
    numbers = parse_part_number_locations(input)

    parts
    |> Enum.filter(fn {_, char} -> char == "*" end)
    |> Enum.map(fn {coords, _} -> part_numbers(coords, numbers) end)
    |> Enum.filter(fn ns -> length(ns) == 2 end)
    |> Enum.map(fn ns -> Enum.product(ns) end)
    |> Enum.sum()
  end
end
