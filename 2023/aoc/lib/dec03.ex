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
end
