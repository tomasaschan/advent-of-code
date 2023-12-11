defmodule Dec09 do
  @doc """
  iex> Dec09.parse("0 3 6 9 12 15\\n1 3 6 10 15 21\\n10 13 16 21 30 45\\n")
  [[0, 3, 6, 9, 12, 15], [1, 3, 6, 10, 15, 21], [10, 13, 16, 21, 30, 45]]
  """
  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line -> line |> String.split(" ") |> Enum.map(&String.to_integer/1) end)
  end

  def extrapolate(history) do
    diffs = history |> Enum.zip(history |> Enum.drop(1)) |> Enum.map(fn {a, b} -> b - a end)

    last = history |> List.last()

    cond do
      Enum.all?(diffs, fn n -> n == 0 end) -> last
      true -> last + extrapolate(diffs)
    end
  end

  @doc """
  iex> Dec09.a("0 3 6 9 12 15\\n1 3 6 10 15 21\\n10 13 16 21 30 45\\n")
  114
  """
  def a(input) do
    histories = parse(input)

    histories
    |> Enum.map(&extrapolate/1)
    |> Enum.sum()
  end
end
