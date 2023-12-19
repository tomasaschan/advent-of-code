defmodule Dec12 do
  @doc """
  iex> Dec12.parse("???.### 1,1,3
  ...>.??..??...?##. 1,1,3
  ...>?#?#?#?#?#?#?#? 1,3,1,6
  ...>????.#...#... 4,1,1
  ...>????.######..#####. 1,6,5
  ...>?###???????? 3,2,1")
  [
    {["?", "?", "?", ".", "#", "#", "#"], [1, :space, 1, :space, 3]},
    {[".", "?", "?", ".", ".", "?", "?", ".", ".", ".", "?", "#", "#", "."], [1, :space, 1, :space, 3]},
    {["?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?"], [1, :space, 3, :space, 1, :space, 6]},
    {["?", "?", "?", "?", ".", "#", ".", ".", ".", "#", ".", ".", "."], [4, :space, 1, :space, 1]},
    {["?", "?", "?", "?", ".", "#", "#", "#", "#", "#", "#", ".", ".", "#", "#", "#", "#", "#", "."], [1, :space, 6, :space, 5]},
    {["?", "#", "#", "#", "?", "?", "?", "?", "?", "?", "?", "?"], [3, :space, 2, :space, 1]}
  ]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(" ", trim: true)
      |> Enum.map(fn part -> part |> String.graphemes() end)
      |> List.to_tuple()
      |> then(fn {conditions, checksum} ->
        {conditions,
         checksum
         |> Enum.map(fn
           "," -> :space
           c -> String.to_integer(c)
         end)}
      end)
    end)
  end
end
