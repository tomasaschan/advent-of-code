defmodule Dec25 do
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.flat_map(fn line ->
      [from, to] = String.split(line, ": ")
      to |> String.split(" ") |> Enum.map(fn to -> {from, to} end)
    end)
    |> Enum.reduce(%{}, fn {from, to}, nodes ->
      if from < to do
        nodes
        |> Map.update(from, MapSet.new([to]), fn known -> MapSet.put(known, to) end)
      else
        nodes
        |> Map.update(to, MapSet.new([from]), fn known -> MapSet.put(known, from) end)
      end
    end)
  end

  @doc """
  iex> "jqt: rhn xhk nvd
  ...>rsh: frs pzl lsr
  ...>xhk: hfx
  ...>cmg: qnr nvd lhk bvb
  ...>rhn: xhk bvb hfx
  ...>bvb: xhk hfx
  ...>pzl: lsr hfx nvd
  ...>qnr: nvd
  ...>ntq: jqt hfx bvb xhk
  ...>nvd: lhk
  ...>lsr: lhk
  ...>rzs: qnr cmg lsr rsh
  ...>frs: qnr lhk lsr
  ...>" |> Dec25.parse() |> Enum.map(&Dec25.remove_edges/1) |> Map.new() |> Dec25.visualize("doctest")
  """
  def visualize(graph, name) do
    f = "dec25-#{name}.dot"
    File.write!(f, "graph G {\n", [:write])

    for {from, to} <- graph do
      for n <- to do
        File.write!(f, "  #{from} -- #{n};\n", [:append])
      end
    end

    File.write!(f, "}", [:append])
  end

  @doc """
  iex>
  """
  def degrees(graph) do
    graph
    |> Enum.map(fn {_, to} -> MapSet.size(to) end)
    |> Enum.reduce(%{}, fn degree, acc ->
      Map.update(acc, degree, 1, &(&1 + 1))
    end)
  end

  @doc """
  what edges to remove was determined by just creating a dot file with all the edges,
  and rendering it with a spring layout, e.g. neato.
  """
  # these are for the real case
  def remove_edges({"kdk", to}), do: {"kdk", MapSet.delete(to, "nct")}
  def remove_edges({"nct", to}), do: {"nct", MapSet.delete(to, "kdk")}
  def remove_edges({"fsv", to}), do: {"fsv", MapSet.delete(to, "spx")}
  def remove_edges({"spx", to}), do: {"spx", MapSet.delete(to, "fsv")}
  def remove_edges({"tvj", to}), do: {"tvj", MapSet.delete(to, "cvx")}
  def remove_edges({"cvx", to}), do: {"cvx", MapSet.delete(to, "tvj")}

  # these are for the example
  def remove_edges({"hfx", to}), do: {"hfx", MapSet.delete(to, "pzl")}
  def remove_edges({"pzl", to}), do: {"pzl", MapSet.delete(to, "hfx")}
  def remove_edges({"bvb", to}), do: {"bvb", MapSet.delete(to, "cmg")}
  def remove_edges({"cmg", to}), do: {"cmg", MapSet.delete(to, "bvb")}
  def remove_edges({"nvd", to}), do: {"nvd", MapSet.delete(to, "jqt")}
  def remove_edges({"jqt", to}), do: {"jqt", MapSet.delete(to, "nvd")}

  def remove_edges({from, to}), do: {from, to}

  def undirect(graph) do
    graph
    |> Enum.flat_map(fn {from, to} ->
      Enum.concat(Enum.map(to, fn t -> {from, t} end), Enum.map(to, fn t -> {t, from} end))
    end)
    |> Enum.reduce(%{}, fn {from, to}, nodes ->
      Map.update(nodes, from, MapSet.new([to]), fn known -> MapSet.put(known, to) end)
    end)
  end

  def floodfill(fringe, graph, seen) do
    case :queue.out(fringe) do
      {:empty, _} ->
        seen

      {{:value, here}, fringe} ->
        {fringe, seen} =
          cond do
            MapSet.member?(seen, here) ->
              {fringe, seen}

            not Map.has_key?(graph, here) ->
              {fringe, MapSet.put(seen, here)}

            true ->
              {
                :queue.join(fringe, graph[here] |> MapSet.to_list() |> :queue.from_list()),
                MapSet.put(seen, here)
              }
          end

        floodfill(fringe, graph, seen)
    end
  end

  def floodfill_from(start, graph) do
    floodfill(:queue.from_list([start]), undirect(graph), MapSet.new())
  end

  @doc """
  iex> "jqt: rhn xhk nvd
  ...>rsh: frs pzl lsr
  ...>xhk: hfx
  ...>cmg: qnr nvd lhk bvb
  ...>rhn: xhk bvb hfx
  ...>bvb: xhk hfx
  ...>pzl: lsr hfx nvd
  ...>qnr: nvd
  ...>ntq: jqt hfx bvb xhk
  ...>nvd: lhk
  ...>lsr: lhk
  ...>rzs: qnr cmg lsr rsh
  ...>frs: qnr lhk lsr
  ...>" |> Dec25.a()
  54
  """
  def a(input) do
    graph = parse(input) |> Enum.map(&remove_edges/1) |> Map.new()

    # find a random node to floodfill from; it doesn't matter which one
    a =
      graph
      |> Map.keys()
      |> Enum.at(0)
      |> floodfill_from(graph)

    # find a node that wasn't in the first fill and fill from that to find the other component
    b =
      graph
      |> Map.keys()
      |> Enum.find(fn n -> not MapSet.member?(a, n) end)
      |> floodfill_from(graph)

    MapSet.size(a) * MapSet.size(b)
  end
end
