defmodule Dec01 do
  @doc """
      iex> Dec01.parse("1abc2")
      12

      iex> Dec01.parse("pqr3stu8vwx")
      38

      iex> Dec01.parse("a1b2c3d4e5f")
      15

      iex> Dec01.parse("treb7uchet")
      77
  """
  def parse(line) do
    line
    |> String.graphemes()
    |> Enum.map(fn c -> Integer.parse(c) end)
    |> Enum.filter(fn n -> n != :error end)
    |> Enum.map(fn {n, _} -> n end)
    |> Enum.filter(fn n -> n > 0 end)
    |> assemble(line)
  end

  def assemble(ns, line) do
    if length(ns) == 0 do
      IO.puts(line)
    end

    10 * List.first(ns) + List.last(ns)
  end

  @doc """
      iex> Dec01.a("1abc2\\npqr3stu8vwx\\na1b2c3d4e5f\\ntreb7uchet")
      142
  """
  def a(input) do
    input
    |> String.split("\n")
    |> Enum.filter(fn line -> line != "" end)
    |> Enum.map(fn line -> Dec01.parse(line) end)
    |> Enum.sum()
  end
end
