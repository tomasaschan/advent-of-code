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
    {"???.###", [1, 1, 3]},
    {".??..??...?##.", [1, 1, 3]},
    {"?#?#?#?#?#?#?#?", [1, 3, 1, 6]},
    {"????.#...#...", [4, 1, 1]},
    {"????.######..#####.", [1, 6, 5]},
    {"?###????????", [3, 2, 1]}
  ]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(" ", trim: true)
      |> List.to_tuple()
      |> then(fn {conditions, checksum} ->
        {conditions, checksum |> String.split(",", trim: true) |> Enum.map(&String.to_integer/1)}
      end)
    end)
  end

  @doc """
  iex> Dec12.expand("???.###")
  [
    "###.###",
    "##..###",
    "#.#.###",
    "#...###",
    ".##.###",
    ".#..###",
    "..#.###",
    "....###",
  ]
  """
  def expand(""), do: [""]
  def expand("#" <> rest), do: expand(rest) |> Enum.map(fn x -> "#" <> x end) |> Enum.sort()
  def expand("." <> rest), do: expand(rest) |> Enum.map(fn x -> "." <> x end) |> Enum.sort()

  def expand("?" <> rest),
    do: expand(rest) |> Enum.flat_map(fn x -> ["#" <> x, "." <> x] end) |> Enum.sort()

  @spec valid?(String.t(), [integer()]) :: boolean
  @doc """
  iex> Dec12.valid?("##.#.#", [2,1,1])
  true
  iex> Dec12.valid?(".##.#.#", [2,1,1])
  true

  iex> Dec12.valid?("###.###", [1, 1, 3])
  false
  iex> Dec12.valid?(".##.###", [1, 1, 3])
  false
  iex> Dec12.valid?("#.#.###", [1, 1, 3])
  true
  iex> Dec12.valid?("..#.###", [1, 1, 3])
  false
  iex> Dec12.valid?("##..###", [1, 1, 3])
  false
  iex> Dec12.valid?(".#..###", [1, 1, 3])
  false
  iex> Dec12.valid?("#...###", [1, 1, 3])
  false
  iex> Dec12.valid?("....###", [1, 1, 3])
  false
  iex> Dec12.valid?(".###....##.#", [3, 2, 1])
  true
  iex> Dec12.valid?(".###...#.#.#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###..#..#.#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.#...#.#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###...##..#", [3, 2, 1])
  true
  iex> Dec12.valid?(".###..#.#..#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.#..#..#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###..##...#", [3, 2, 1])
  true
  iex> Dec12.valid?(".###.#.#...#", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.##....#", [3, 2, 1])
  true
  iex> Dec12.valid?(".###...##.#.", [3, 2, 1])
  true
  iex> Dec12.valid?(".###..#.#.#.", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.#..#.#.", [3, 2, 1])
  false
  iex> Dec12.valid?(".###..##..#.", [3, 2, 1])
  true
  iex> Dec12.valid?(".###.#.#..#.", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.##...#.", [3, 2, 1])
  true
  iex> Dec12.valid?(".###..##.#..", [3, 2, 1])
  true
  iex> Dec12.valid?(".###.#.#.#..", [3, 2, 1])
  false
  iex> Dec12.valid?(".###.##..#..", [3, 2, 1])
  true
  iex> Dec12.valid?(".###.##.#...", [3, 2, 1])
  true
  """
  # if there are no more springs, the requirements list must also be empty
  def valid?("", []), do: true
  def valid?("." <> springs, spec), do: valid?(springs, spec)
  def valid?("#" <> _, []), do: false
  def valid?("", [_ | _]), do: false
  def valid?("#" <> springs, [d | spec]), do: valid_group?(springs, d - 1, spec)

  def valid_group?("#" <> _, 0, _), do: false
  def valid_group?("#" <> springs, d, spec) when d > 0, do: valid_group?(springs, d - 1, spec)
  def valid_group?("." <> springs, 0, spec), do: valid?(springs, spec)
  def valid_group?("." <> _, d, _) when d > 0, do: false
  def valid_group?("", 0, spec), do: valid?("", spec)
  def valid_group?("", d, _) when d > 0, do: false

  @doc """
  iex> Dec12.n_valid("???.###", [1, 1, 3])
  1
  iex> Dec12.n_valid(".??..??...?##.", [1, 1, 3])
  4
  iex> Dec12.n_valid("?#?#?#?#?#?#?#?", [1, 3, 1, 6])
  1
  iex> Dec12.n_valid("????.#...#...", [4, 1, 1])
  1
  iex> Dec12.n_valid("????.######..#####.", [1, 6, 5])
  4
  iex> Dec12.n_valid("?###????????", [3, 2, 1])
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
