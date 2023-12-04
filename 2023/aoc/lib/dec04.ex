defmodule Dec04 do
  @doc """
  iex> Dec04.parse("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
  {1, [41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]}
  iex> Dec04.parse("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")
  {2, [13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19]}
  iex> Dec04.parse("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1")
  {3, [1, 21, 53, 59, 44], [69, 82, 63, 72, 16, 21, 14, 1]}
  iex> Dec04.parse("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83")
  {4, [41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83]}
  iex> Dec04.parse("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36")
  {5, [87, 83, 26, 28, 32], [88, 30, 70, 12, 93, 22, 82, 36]}
  iex> Dec04.parse("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
  {6, [31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11]}
  """
  def parse(line) do
    [_, id, numbers] = Regex.run(~r/Card +(\d+): (.*)/, line)

    [winning, have] =
      numbers
      |> String.split(" | ")
      |> Enum.map(fn list ->
        list |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)
      end)

    {String.to_integer(id), winning, have}
  end

  @doc """
  iex> Dec04.points({1, [41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]})
  8
  iex> Dec04.points({2, [13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19]})
  2
  iex> Dec04.points({3, [1, 21, 53, 59, 44], [69, 82, 63, 72, 16, 21, 14, 1]})
  2
  iex> Dec04.points({4, [41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83]})
  1
  iex> Dec04.points({5, [87, 83, 26, 28, 32], [88, 30, 70, 12, 93, 22, 82, 36]})
  0
  iex> Dec04.points({6, [31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11]})
  0
  """
  def points({_, winning, have}) do
    winning_count = MapSet.new(winning) |> MapSet.intersection(MapSet.new(have)) |> MapSet.size()

    if winning_count > 0 do
      2 ** (winning_count - 1)
    else
      0
    end
  end

  @doc """
  iex> Dec04.a("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
  13
  """
  def a(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse/1)
    |> Enum.map(&points/1)
    |> Enum.sum()
  end
end
