defmodule Dec05 do
  @doc """
  iex> Dec05.parse_map("seed-to-soil map:\\n50 98 2\\n52 50 48")
  %{seed: {:soil, [{50, 98, 2}, {52, 50, 48}]}}
  iex> Dec05.parse_map("soil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15")
  %{soil: {:fertilizer, [{0, 15, 37}, {37, 52, 2}, {39, 0, 15}]}}
  iex> Dec05.parse_map("fertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4")
  %{fertilizer: {:water, [{49, 53, 8}, {0, 11, 42}, {42, 0, 7}, {57, 7, 4}]}}
  iex> Dec05.parse_map("water-to-light map:\\n88 18 7\\n18 25 70")
  %{water: {:light, [{88, 18, 7}, {18, 25, 70}]}}
  iex> Dec05.parse_map("light-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13")
  %{light: {:temperature, [{45, 77, 23}, {81, 45, 19}, {68, 64, 13}]}}
  iex> Dec05.parse_map("temperature-to-humidity map:\\n0 69 1\\n1 0 69")
  %{temperature: {:humidity, [{0, 69, 1}, {1, 0, 69}]}}
  iex> Dec05.parse_map("humidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  %{humidity: {:location, [{60, 56, 37}, {56, 93, 4}]}}
  """
  def parse_map(input) do
    parts = input |> String.split("\n", trim: true)

    [from, to] =
      parts
      |> Enum.at(0)
      |> String.replace(" map:", "")
      |> String.split("-to-", trim: true)
      |> Enum.map(&String.to_atom/1)

    values =
      parts
      |> Enum.drop(1)
      |> Enum.map(&String.split(&1, " ", trim: true))
      |> Enum.map(fn list ->
        list
        |> Enum.map(&String.to_integer/1)
      end)
      |> Enum.map(&List.to_tuple/1)

    %{from => {to, values}}
  end

  @doc """
  iex> Dec05.parse("seeds: 79 14 55 13\\n\\nseed-to-soil map:\\n50 98 2\\n52 50 48\\n\\nsoil-to-fertilizer map:\\n0 15 37\\n37 52 2\\n39 0 15\\n\\nfertilizer-to-water map:\\n49 53 8\\n0 11 42\\n42 0 7\\n57 7 4\\n\\nwater-to-light map:\\n88 18 7\\n18 25 70\\n\\nlight-to-temperature map:\\n45 77 23\\n81 45 19\\n68 64 13\\n\\ntemperature-to-humidity map:\\n0 69 1\\n1 0 69\\n\\nhumidity-to-location map:\\n60 56 37\\n56 93 4\\n")
  %{
    seeds: [79, 14, 55, 13],
    seed: {:soil, [{50, 98, 2}, {52, 50, 48}]},
    soil: {:fertilizer, [{0, 15, 37}, {37, 52, 2}, {39, 0, 15}]},
    fertilizer: {:water, [{49, 53, 8}, {0, 11, 42}, {42, 0, 7}, {57, 7, 4}]},
    water: {:light, [{88, 18, 7}, {18, 25, 70}]},
    light: {:temperature, [{45, 77, 23}, {81, 45, 19}, {68, 64, 13}]},
    temperature: {:humidity, [{0, 69, 1}, {1, 0, 69}]},
    humidity: {:location, [{60, 56, 37}, {56, 93, 4}]},
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
  iex> Dec05.lookup(79, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  {:soil, 81}
  iex> Dec05.lookup(14, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  {:soil, 14}
  iex> Dec05.lookup(55, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  {:soil, 57}
  iex> Dec05.lookup(13, {:soil, [{50, 98, 2}, {52, 50, 48}]})
  {:soil, 13}
  """
  def lookup(value, map) do
    {target, mappings} = map

    if length(mappings) == 0 do
      {target, value}
    else
      {dest, src, steps} = mappings |> Enum.at(0)

      cond do
        src <= value && value < src + steps -> {target, value + dest - src}
        true -> lookup(value, {target, Enum.drop(mappings, 1)})
      end
    end
  end

  @doc """
  iex> Dec05.location(:seed, 79, %{seeds: [79, 14, 55, 13], seed: {:soil, [{50, 98, 2}, {52, 50, 48}]}, soil: {:fertilizer, [{0, 15, 37}, {37, 52, 2}, {39, 0, 15}]}, fertilizer: {:water, [{49, 53, 8}, {0, 11, 42}, {42, 0, 7}, {57, 7, 4}]}, water: {:light, [{88, 18, 7}, {18, 25, 70}]}, light: {:temperature, [{45, 77, 23}, {81, 45, 19}, {68, 64, 13}]}, temperature: {:humidity, [{0, 69, 1}, {1, 0, 69}]}, humidity: {:location, [{60, 56, 37}, {56, 93, 4}]}})
  82
  """
  def location(type, value, maps) do
    if type == :location do
      value
    else
      {target, value} = lookup(value, maps[type])
      location(target, value, maps)
    end
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
  iex> Dec05.seeds([79, 14, 55, 13])
  [79..92, 55..67] |> Enum.flat_map(fn x -> x end) |> List.to_tuple()
  """
  def seeds(list) do
    list
    |> Enum.chunk_every(2)
    |> Enum.map(fn [x, n] -> x..(x + n - 1) end)
    |> Enum.flat_map(fn x -> x end)
    |> List.to_tuple()
  end

  def b(input) do
    maps = parse(input)

    seeds(maps[:seeds])
    |> Enum.map(fn seed -> location(:seed, seed, maps) end)
    |> Enum.min()
  end
end
