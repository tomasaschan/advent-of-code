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
  [(0..49), (52..99), (50..51)] |> Enum.flat_map(fn x -> x end)
  """
  def follow(n, []) do
    n
  end

  def follow(n, [{x, d} | [{xn, _} | _]]) when x <= n and n < xn do
    n + d
  end

  def follow(n, [_ | rest]) do
    follow(n, rest)
  end

  @doc """
  iex> Dec05.follow(79, Dec05.simplify_pair([{50, 2}, {98, -48}, {100, 0}], [{0, 39}, {15, -15}, {52, -15}, {54, 0}]))
  81
  iex> Dec05.follow(14, Dec05.simplify_pair([{50, 2}, {98, -48}, {100, 0}], [{0, 39}, {15, -15}, {52, -15}, {54, 0}]))
  53
  """
  def simplify_pair(first, second) do
    first
  end

  # @doc """
  # iex> Dec05.lookup(79, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  # {:soil, 81}
  # iex> Dec05.lookup(14, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  # {:soil, 14}
  # iex> Dec05.lookup(55, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  # {:soil, 57}
  # iex> Dec05.lookup(13, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  # {:soil, 13}
  # """
  # def lookup(value, map) do
  # end

  # @doc """
  # iex> Dec05.location(:seed, 79, %{seeds: [79, 14, 55, 13], seed: {:soil, [{50, 98, 2}, {52, 50, 48}]}, soil: {:fertilizer, [{0, 15, 37}, {37, 52, 2}, {39, 0, 15}]}, fertilizer: {:water, [{49, 53, 8}, {0, 11, 42}, {42, 0, 7}, {57, 7, 4}]}, water: {:light, [{88, 18, 7}, {18, 25, 70}]}, light: {:temperature, [{45, 77, 23}, {81, 45, 19}, {68, 64, 13}]}, temperature: {:humidity, [{0, 69, 1}, {1, 0, 69}]}, humidity: {:location, [{60, 56, 37}, {56, 93, 4}]}})
  # 82
  # """
  # def location(type, value, maps) do
  # end

  # @doc """
  # iex> Dec05.a("seeds: 79 14 55 13\\n\\nseed-to-soil map:\\n50 98 2\\n52 50 48\\n\\nsoil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15\\n\\nfertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4\\n\\nwater-to-light map:\\n88 18 7\\n18 25 70\\n\\nlight-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13\\n\\ntemperature-to-humidity map:\\n0 69 1\\n1 0 69\\n\\nhumidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  # 35
  # """
  # def a(input) do
  #   maps = parse(input)
  # end

  # def b(input) do
  #   maps = parse(input)
  # end
end
