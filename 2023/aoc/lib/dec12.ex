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
  def n_valid("", []), do: 1
  def n_valid("." <> springs, spec), do: n_valid(springs, spec)
  def n_valid("#" <> _, []), do: 0
  def n_valid("", [_ | _]), do: 0
  def n_valid("#" <> springs, [d | spec]), do: n_valid_group(springs, d - 1, spec)

  def n_valid("?" <> springs, spec),
    do: n_valid("#" <> springs, spec) + n_valid("." <> springs, spec)

  def n_valid_group("#" <> _, 0, _), do: 0
  def n_valid_group("#" <> springs, d, spec) when d > 0, do: n_valid_group(springs, d - 1, spec)
  def n_valid_group("." <> springs, 0, spec), do: n_valid(springs, spec)
  def n_valid_group("." <> _, d, _) when d > 0, do: 0
  def n_valid_group("", 0, spec), do: n_valid("", spec)
  def n_valid_group("", d, _) when d > 0, do: 0

  def n_valid_group("?" <> springs, d, spec) when d > 0,
    do: n_valid_group("#" <> springs, d, spec)

  def n_valid_group("?" <> springs, 0, spec), do: n_valid("." <> springs, spec)

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

  @doc """
  iex> Dec12.b("???.### 1,1,3
  ...>.??..??...?##. 1,1,3
  ...>?#?#?#?#?#?#?#? 1,3,1,6
  ...>????.#...#... 4,1,1
  ...>????.######..#####. 1,6,5
  ...>?###???????? 3,2,1
  ...>")
  525152
  """
  def b(input) do
    input
    |> parse()
    |> Enum.map(fn {springs, spec} ->
      {
        [springs, springs, springs, springs, springs] |> Enum.join("?"),
        Enum.concat([spec, spec, spec, spec, spec])
      }
    end)
    |> Enum.map(fn {springs, spec} -> n_valid(springs, spec) end)
    |> Enum.sum()
  end
end
