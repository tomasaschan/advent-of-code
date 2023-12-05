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
  iex> Dec04.winning_count({1, [41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53]})
  4
  iex> Dec04.winning_count({2, [13, 32, 20, 16, 61], [61, 30, 68, 82, 17, 32, 24, 19]})
  2
  iex> Dec04.winning_count({3, [1, 21, 53, 59, 44], [69, 82, 63, 72, 16, 21, 14, 1]})
  2
  iex> Dec04.winning_count({4, [41, 92, 73, 84, 69], [59, 84, 76, 51, 58, 5, 54, 83]})
  1
  iex> Dec04.winning_count({5, [87, 83, 26, 28, 32], [88, 30, 70, 12, 93, 22, 82, 36]})
  0
  iex> Dec04.winning_count({6, [31, 18, 13, 56, 72], [74, 77, 10, 23, 35, 67, 36, 11]})
  0
  """
  def winning_count({_, winning, have}) do
    MapSet.new(winning) |> MapSet.intersection(MapSet.new(have)) |> MapSet.size()
  end

  def points(w) do
    if w > 0 do
      2 ** (w - 1)
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
    |> Enum.map(&winning_count/1)
    |> Enum.map(&points/1)
    |> Enum.sum()
  end

  @doc """
  iex> Dec04.b("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
  30
  """
  def b(input) do
    deck =
      input
      |> String.split("\n", trim: true)
      |> Enum.map(&parse/1)
      |> Enum.map(fn card -> {1, card} end)

    step_b(deck, 0)
  end

  def step_b(deck, index) do
    cond do
      index >= length(deck) ->
        deck |> Enum.map(fn {c, _} -> c end) |> Enum.sum()

      true ->
        {n, c} = deck |> Enum.at(index)
        step_b(add_cards(deck, index, winning_count(c), n), index + 1)
    end
  end

  def add_cards(deck, from, n_steps, n_to_add) do
    deck
    |> Enum.with_index()
    |> Enum.map(fn {{n, c}, i} ->
      cond do
        i <= from -> {n, c}
        i > from + n_steps -> {n, c}
        true -> {n + n_to_add, c}
      end
    end)
  end
end
