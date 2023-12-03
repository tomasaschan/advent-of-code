defmodule Dec01 do
  @doc """
      iex> Dec01.parse_simple("1abc2")
      12

      iex> Dec01.parse_simple("pqr3stu8vwx")
      38

      iex> Dec01.parse_simple("a1b2c3d4e5f")
      15

      iex> Dec01.parse_simple("treb7uchet")
      77
  """
  def parse_simple(line) do
    line
    |> String.graphemes()
    |> Enum.map(fn c -> Integer.parse(c) end)
    |> Enum.filter(fn n -> n != :error end)
    |> Enum.map(fn {n, _} -> n end)
    |> Enum.filter(fn n -> n > 0 end)
    |> assemble()
  end

  @doc """
      iex> Dec01.parse_complex("two1nine")
      29
      iex> Dec01.parse_complex("eightwothree")
      83
      iex> Dec01.parse_complex("abcone2threexyz")
      13
      iex> Dec01.parse_complex("xtwone3four")
      24
      iex> Dec01.parse_complex("4nineeightseven2")
      42
      iex> Dec01.parse_complex("zoneight234")
      14
      iex> Dec01.parse_complex("7pqrstsixteen")
      76
  """
  def parse_complex(line, start \\ 0, found \\ []) do
    cond do
      start == String.length(line) -> assemble(found)
      String.at(line, start) == "0" -> parse_complex(line, start + 1, found ++ [0])
      String.at(line, start) == "1" -> parse_complex(line, start + 1, found ++ [1])
      String.at(line, start) == "2" -> parse_complex(line, start + 1, found ++ [2])
      String.at(line, start) == "3" -> parse_complex(line, start + 1, found ++ [3])
      String.at(line, start) == "4" -> parse_complex(line, start + 1, found ++ [4])
      String.at(line, start) == "5" -> parse_complex(line, start + 1, found ++ [5])
      String.at(line, start) == "6" -> parse_complex(line, start + 1, found ++ [6])
      String.at(line, start) == "7" -> parse_complex(line, start + 1, found ++ [7])
      String.at(line, start) == "8" -> parse_complex(line, start + 1, found ++ [8])
      String.at(line, start) == "9" -> parse_complex(line, start + 1, found ++ [9])
      String.slice(line, start, 4) == "zero" -> parse_complex(line, start + 1, found ++ [0])
      String.slice(line, start, 3) == "one" -> parse_complex(line, start + 1, found ++ [1])
      String.slice(line, start, 3) == "two" -> parse_complex(line, start + 1, found ++ [2])
      String.slice(line, start, 5) == "three" -> parse_complex(line, start + 1, found ++ [3])
      String.slice(line, start, 4) == "four" -> parse_complex(line, start + 1, found ++ [4])
      String.slice(line, start, 4) == "five" -> parse_complex(line, start + 1, found ++ [5])
      String.slice(line, start, 3) == "six" -> parse_complex(line, start + 1, found ++ [6])
      String.slice(line, start, 5) == "seven" -> parse_complex(line, start + 1, found ++ [7])
      String.slice(line, start, 5) == "eight" -> parse_complex(line, start + 1, found ++ [8])
      String.slice(line, start, 4) == "nine" -> parse_complex(line, start + 1, found ++ [9])
      true -> parse_complex(line, start + 1, found)
    end
  end

  def assemble(ns) do
    10 * List.first(ns) + List.last(ns)
  end

  @doc """
      iex> Dec01.a("1abc2\\npqr3stu8vwx\\na1b2c3d4e5f\\ntreb7uchet")
      142
  """
  def a(input) do
    solve(input, &parse_simple/1)
  end

  @doc """
      iex> Dec01.b("two1nine\\neightwothree\\nabcone2threexyz\\nxtwone3four\\n4nineeightseven2\\nzoneight234\\n7pqrstsixteen")
      281
  """
  def b(input) do
    solve(input, &parse_complex/1)
  end

  def solve(input, parser) do
    input
    |> String.split("\n")
    |> Enum.filter(fn line -> line != "" end)
    |> Enum.map(fn line -> parser.(line) end)
    |> Enum.sum()
  end
end
