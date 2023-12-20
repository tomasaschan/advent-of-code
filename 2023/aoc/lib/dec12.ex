defmodule Dec12 do
  @doc """
  iex> Dec12.parse("???.### 1,1,3
  ...>.??..??...?##. 1,1,3
  ...>?#?#?#?#?#?#?#? 1,3,1,6
  ...>????.#...#... 4,1,1
  ...>????.######..#####. 1,6,5
  ...>?###???????? 3,2,1
  ...>")
  [
    {["?", "?", "?", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3]},
    {[".", "?", "?", ".", ".", "?", "?", ".", ".", ".", "?", "#", "#", "."], [:optional_space, 1, :space, 1, :space, 3]},
    {["?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?"], [:optional_space, 1, :space, 3, :space, 1, :space, 6]},
    {["?", "?", "?", "?", ".", "#", ".", ".", ".", "#", ".", ".", "."], [:optional_space, 4, :space, 1, :space, 1]},
    {["?", "?", "?", "?", ".", "#", "#", "#", "#", "#", "#", ".", ".", "#", "#", "#", "#", "#", "."], [:optional_space, 1, :space, 6, :space, 5]},
    {["?", "#", "#", "#", "?", "?", "?", "?", "?", "?", "?", "?"], [:optional_space, 3, :space, 2, :space, 1]}
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
         [
           :optional_space
           | checksum
             |> Enum.map(fn
               "," -> :space
               c -> String.to_integer(c)
             end)
         ]}
      end)
    end)
  end

  @doc """
  iex> Dec12.expand(["?", "?", "?", ".", "#", "#", "#"])
  [
    ["#", "#", "#", ".", "#", "#", "#"],
    [".", "#", "#", ".", "#", "#", "#"],
    ["#", ".", "#", ".", "#", "#", "#"],
    [".", ".", "#", ".", "#", "#", "#"],
    ["#", "#", ".", ".", "#", "#", "#"],
    [".", "#", ".", ".", "#", "#", "#"],
    ["#", ".", ".", ".", "#", "#", "#"],
    [".", ".", ".", ".", "#", "#", "#"],
  ]
  """
  def expand([]), do: [[]]
  def expand(["#" | rest]), do: expand(rest) |> Enum.map(fn x -> ["#" | x] end)
  def expand(["." | rest]), do: expand(rest) |> Enum.map(fn x -> ["." | x] end)
  def expand(["?" | rest]), do: expand(rest) |> Enum.flat_map(fn x -> [["#" | x], ["." | x]] end)

  @doc """
  iex> Dec12.valid?(["#", "#", "#", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?([".", "#", "#", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?(["#", ".", "#", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  true
  iex> Dec12.valid?([".", ".", "#", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?(["#", "#", ".", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?([".", "#", ".", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?(["#", ".", ".", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false
  iex> Dec12.valid?([".", ".", ".", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  false

  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", ".", ".", "#", "#", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", ".", "#", ".", "#", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", ".", ".", "#", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", ".", ".", "#", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", ".", "#", "#", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", ".", "#", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", ".", "#", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", "#", ".", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", "#", ".", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", "#", ".", ".", ".", ".", "#"], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", ".", "#", "#", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", ".", "#", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", ".", "#", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", "#", ".", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", "#", ".", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", "#", ".", ".", ".", "#", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", ".", "#", "#", ".", "#", ".", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", ".", "#", ".", "#", ".", "."], [:optional_space, 3, :space, 2, :space, 1])
  false
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", "#", ".", ".", "#", ".", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  iex> Dec12.valid?([".", "#", "#", "#", ".", "#", "#", ".", "#", ".", ".", "."], [:optional_space, 3, :space, 2, :space, 1])
  true
  """
  def valid?([], []), do: true
  def valid?(["." | rest], []), do: valid?(rest, [])
  def valid?(["." | rest], [:space | reqs]), do: valid?(rest, [:optional_space | reqs])

  def valid?(["." | rest], [:optional_space | reqs]),
    do: valid?(rest, [:optional_space | reqs])

  def valid?(["#" | rest], [:optional_space | reqs]), do: valid?(["#" | rest], reqs)
  def valid?(["#" | rest], [1 | reqs]), do: valid?(rest, reqs)

  def valid?(["#" | rest], [d | reqs]) when is_integer(d) and d > 1,
    do: valid?(rest, [d - 1 | reqs])

  def valid?(["#" | _], [:space | _]), do: false

  def valid?(_, _), do: false

  @doc """
  iex> Dec12.n_valid(["?", "?", "?", ".", "#", "#", "#"], [:optional_space, 1, :space, 1, :space, 3])
  1
  iex> Dec12.n_valid([".", "?", "?", ".", ".", "?", "?", ".", ".", ".", "?", "#", "#", "."], [:optional_space, 1, :space, 1, :space, 3])
  4
  iex> Dec12.n_valid(["?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?", "#", "?"], [:optional_space, 1, :space, 3, :space, 1, :space, 6])
  1
  iex> Dec12.n_valid(["?", "?", "?", "?", ".", "#", ".", ".", ".", "#", ".", ".", "."], [:optional_space, 4, :space, 1, :space, 1])
  1
  iex> Dec12.n_valid(["?", "?", "?", "?", ".", "#", "#", "#", "#", "#", "#", ".", ".", "#", "#", "#", "#", "#", "."], [:optional_space, 1, :space, 6, :space, 5])
  4
  iex> Dec12.n_valid(["?", "#", "#", "#", "?", "?", "?", "?", "?", "?", "?", "?"], [:optional_space, 3, :space, 2, :space, 1])
  10
  """
  def n_valid(conditions, requirements) do
    expand(conditions) |> Enum.filter(fn conds -> valid?(conds, requirements) end) |> Enum.count()
  end

  @doc """
  iex> Dec12.a("???.### 1,1,3
  ...>.??..??...?##. 1,1,3
  ...>?#?#?#?#?#?#?#? 1,3,1,6
  ...>????.#...#... 4,1,1
  ...>????.######..#####. 1,6,5
  ...>?###???????? 3,2,1
  ...>")
  21
  """
  def a(input) do
    input |> parse() |> Enum.map(fn {conds, reqs} -> n_valid(conds, reqs) end) |> Enum.sum()
  end
end
