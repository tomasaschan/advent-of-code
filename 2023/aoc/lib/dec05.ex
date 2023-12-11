defmodule Dec05 do
  @doc """
  iex> Dec05.parse_map("seed-to-soil map:\\n50 98 2\\n52 50 48")
  %{seed: {:soil, [{50, 2}, {98, -48}, {100, 0}]}}
  iex> Dec05.parse_map("soil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15")
  %{soil: {:fertilizer, [{0, 39}, {15, -15}, {52, -15}, {54, 0}]}}
  iex> Dec05.parse_map("fertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4")
  %{fertilizer: {:water, [{0, 42}, {7, 50}, {11, -11}, {53, -4}, {61, 0}]}}
  iex> Dec05.parse_map("water-to-light map:\\n88 18 7\\n18 25 70")
  %{water: {:light, [{18, 70}, {25, -7}, {95, 0}]}}
  iex> Dec05.parse_map("light-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13")
  %{light: {:temperature, [{45, 36}, {64, 4}, {77, -32}, {100, 0}]}}
  iex> Dec05.parse_map("temperature-to-humidity map:\\n0 69 1\\n1 0 69")
  %{temperature: {:humidity, [{0, 1}, {69, -69}, {70, 0}]}}
  iex> Dec05.parse_map("humidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  %{humidity: {:location, [{56, 4}, {93, -37}, {97, 0}]}}
  """
  def parse_map(input) do
    parts = input |> String.split("\n", trim: true)

    [from, to] =
      parts
      |> Enum.at(0)
      |> String.replace(" map:", "")
      |> String.split("-to-", trim: true)
      |> Enum.map(&String.to_atom/1)

    slices =
      parts
      |> Enum.drop(1)
      |> Enum.map(fn line ->
        line
        |> String.split(" ")
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
        |> then(fn {dst, src, n} -> {src, dst - src, n} end)
      end)
      |> Enum.sort()

    last_slice = slices |> List.last() |> then(fn {src, _, n} -> {src + n, 0} end)

    %{
      from =>
        {to,
         slices |> Enum.map(fn {src, diff, _} -> {src, diff} end) |> Enum.concat([last_slice])}
    }
  end

  @doc """
  iex> Dec05.parse("seeds: 79 14 55 13\\n\\nseed-to-soil map:\\n50 98 2\\n52 50 48\\n\\nsoil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15\\n\\nfertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4\\n\\nwater-to-light map:\\n88 18 7\\n18 25 70\\n\\nlight-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13\\n\\ntemperature-to-humidity map:\\n0 69 1\\n1 0 69\\n\\nhumidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  %{
    seeds: [79, 14, 55, 13],
    seed: {:soil, [{50, 2}, {98, -48}, {100, 0}]},
    soil: {:fertilizer, [{0, 39}, {15, -15}, {52, -15}, {54, 0}]},
    fertilizer: {:water, [{0, 42}, {7, 50}, {11, -11}, {53, -4}, {61, 0}]},
    water: {:light, [{18, 70}, {25, -7}, {95, 0}]},
    light: {:temperature, [{45, 36}, {64, 4}, {77, -32}, {100, 0}]},
    temperature: {:humidity, [{0, 1}, {69, -69}, {70, 0}]},
    humidity: {:location, [{56, 4}, {93, -37}, {97, 0}]},
  }
  """
  def parse(input) do
    parts = input |> String.split("\n\n", trim: true)

    seeds =
      parts
      |> Enum.at(0)
      |> String.split(": ", trim: true)
      |> Enum.at(1)
      |> String.split(" ", trim: true)
      |> Enum.map(&String.to_integer/1)

    maps = parts |> Enum.drop(1) |> Enum.map(&parse_map/1) |> Enum.reduce(&Map.merge/2)
    %{:seeds => seeds} |> Map.merge(maps)
  end

  @doc """
  iex> (0..99) |> Enum.map(fn n -> Dec05.follow(n, [{50, 2}, {98, -48}, {100, 0}]) end)
  [
    [(0..49), (52..99), (50..51)] |> Enum.flat_map(&Range.to_list/1),
    [(49..0//-1), (99-52..0//-1), (1..0//-1)] |> Enum.flat_map(&Range.to_list/1)
  ] |> Enum.zip()
  """
  def follow(n, []) do
    {n, :infinity}
  end

  def follow(n, [{x, d} | [{xn, _} | _]]) when x <= n and n < xn do
    {n + d, xn - n - 1}
  end

  def follow(n, [{x, _} | _]) when n < x do
    {n, x - n - 1}
  end

  def follow(n, [_ | rest]) do
    follow(n, rest)
  end

  def thread(:location, n, k, _) do
    {n, k}
  end

  def thread(target, n, k, maps) do
    {next, map} = maps[target]
    {m, l} = follow(n, map)
    thread(next, m, min(k, l), maps)
  end

  @doc """
  iex> Dec05.lookup(79, {:soil, [{50, 2}, {98, -48}, {100, 0}]})
  {:soil, 81}
  iex> Dec05.lookup(14, {:soil, [{50, 2}, {98, -48}, {100, 0}]})
  {:soil, 14}
  iex> Dec05.lookup(55, {:soil, [{50, 2}, {98, -48}, {100, 0}]})
  {:soil, 57}
  iex> Dec05.lookup(13, {:soil, [{50, 2}, {98, -48}, {100, 0}]})
  {:soil, 13}
  """
  def lookup(value, {target, mapping}) do
    {dest, _} = follow(value, mapping)
    {target, dest}
  end

  @doc """
  iex> Dec05.location(
  ...>  :seed, 79,
  ...>  %{
  ...>    seeds: [79, 14, 55, 13],
  ...>    seed: {:soil, [{50, 2}, {98, -48}, {100, 0}]},
  ...>    soil: {:fertilizer, [{0, 39}, {15, -15}, {52, -15}, {54, 0}]},
  ...>    fertilizer: {:water, [{0, 42}, {7, 50}, {11, -11}, {53, -4}, {61, 0}]},
  ...>    water: {:light, [{18, 70}, {25, -7}, {95, 0}]},
  ...>    light: {:temperature, [{45, 36}, {64, 4}, {77, -32}, {100, 0}]},
  ...>    temperature: {:humidity, [{0, 1}, {69, -69}, {70, 0}]},
  ...>    humidity: {:location, [{56, 4}, {93, -37}, {97, 0}]},
  ...>  }
  ...> )
  82
  """
  def location(:location, value, _) do
    value
  end

  def location(type, value, maps) do
    {target, value} = lookup(value, maps[type])
    location(target, value, maps)
  end

  @doc """
  iex> Dec05.a("seeds: 79 14 55 13\\n\\nseed-to-soil map:\\n50 98 2\\n52 50 48\\n\\nsoil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15\\n\\nfertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4\\n\\nwater-to-light map:\\n88 18 7\\n18 25 70\\n\\nlight-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13\\n\\ntemperature-to-humidity map:\\n0 69 1\\n1 0 69\\n\\nhumidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  35
  """
  def a(input) do
    maps = parse(input)

    maps[:seeds]
    |> Enum.map(fn seed -> location(:seed, seed, maps) end)
    |> Enum.min()
  end

  @doc """
  iex> 3 < :infinity
  true
  """
  def minimize_in_range(a, b, lo, _) when a >= b, do: {lo, 0}

  def minimize_in_range(a, b, lo, maps) do
    {l, k} = thread(:seed, a, :infinity, maps)
    minimize_in_range(a + k + 1, b, min(lo, l), maps)
  end

  @doc """
  iex> Dec05.b("seeds: 79 14 55 13\\n\\nseed-to-soil map:\\n50 98 2\\n52 50 48\\n\\nsoil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15\\n\\nfertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4\\n\\nwater-to-light map:\\n88 18 7\\n18 25 70\\n\\nlight-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13\\n\\ntemperature-to-humidity map:\\n0 69 1\\n1 0 69\\n\\nhumidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  46
  """
  def b(input) do
    maps = parse(input)

    maps[:seeds]
    |> Enum.chunk_every(2)
    |> Enum.map(fn [a, b] -> a..(a + b - 1) end)
    |> Enum.sort()
    |> Enum.map(fn range -> minimize_in_range(range.first, range.last, :infinity, maps) end)
    |> Enum.map(fn {lo, _} -> lo end)
    |> Enum.min()
  end
end
