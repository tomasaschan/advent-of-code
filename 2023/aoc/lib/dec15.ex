defmodule Dec15 do
  @doc """
  iex> Dec15.hash("HASH")
  52
  """
  def hash(input, state \\ 0)

  def hash(s, state) when is_binary(s), do: hash(String.to_charlist(s), state)
  def hash([], state), do: state

  def hash([c | rest], state) do
    hash(rest, rem((state + c) * 17, 256))
  end

  def parse(input) do
    input
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.map(&parse_step/1)
  end

  def parse_step(input) do
    cond do
      String.contains?(input, "=") ->
        [label, f] = String.split(input, "=")
        {:add, String.to_atom(label), String.to_integer(f), hash(label)}

      String.contains?(input, "-") ->
        label = String.trim(input, "-")
        {:remove, String.to_atom(label), hash(label)}
    end
  end

  @doc ~S"""
  iex> Dec15.a("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
  1320
  """
  def a(input) do
    input
    |> String.trim()
    |> String.split(",", trim: true)
    |> Enum.map(&hash/1)
    |> Enum.sum()
  end

  @doc """
  iex> "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  ...> |> Dec15.parse()
  ...> |> Dec15.initialize()
  ...> |> Map.to_list()
  ...> |> Enum.filter(fn {_, v} -> v != [] end)
  ...> |> Map.new()
  %{
    0 => [rn: 1, cm: 2],
    3 => [ot: 7, ab: 5, pc: 6]
  }
  """
  def initialize(steps, boxes \\ 0..255 |> Enum.map(fn k -> {k, []} end) |> Map.new())
  def initialize([], boxes), do: boxes

  def initialize([{:add, label, f, h} | rest], boxes) do
    initialize(rest, boxes |> Map.put(h, insert(boxes[h], {label, f})))
  end

  def initialize([{:remove, label, h} | rest], boxes) do
    initialize(rest, boxes |> Map.put(h, remove(boxes[h], label)))
  end

  def insert([], {label, f}), do: [{label, f}]
  def insert([{l, _} | rest], {label, f}) when l == label, do: [{label, f} | rest]
  def insert([{l, g} | rest], {label, f}) when l != label, do: [{l, g} | insert(rest, {label, f})]

  def remove([], _), do: []
  def remove([{l, _} | rest], label) when l == label, do: remove(rest, label)
  def remove([{l, g} | rest], label) when l != label, do: [{l, g} | remove(rest, label)]

  @doc """
  iex> Dec15.focusing_power(%{0 => [rn: 1, cm: 2], 3 => [ot: 7, ab: 5, pc: 6]})
  145
  """
  def focusing_power(boxes) do
    boxes
    |> Map.to_list()
    |> Enum.flat_map(fn {k, lenses} ->
      lenses |> Enum.with_index(1) |> Enum.map(fn {{_, f}, i} -> (k + 1) * f * i end)
    end)
    |> Enum.sum()
  end

  @doc """
  iex> Dec15.b("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
  145
  """
  def b(input) do
    input
    |> parse()
    |> initialize()
    |> focusing_power()
  end
end
