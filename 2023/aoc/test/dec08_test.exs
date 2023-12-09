defmodule Dec08Test do
  use ExUnit.Case
  doctest Dec08

  test "Dec08" do
    input = File.read!("../../inputs/2023/08.txt")
    assert Dec08.a(input) == 14893
    # assert Dec08.b(input) == 0
  end

  test "every path has a cycle" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    assert starts
           |> Enum.all?(fn start ->
             {_, first, last} = Dec08.find_cycle(map, instructions, start)
             last > first
           end)
  end

  test "every path reaches only a single end during its cycle" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    assert starts
           |> Enum.all?(fn start ->
             {pos, first, last, seen} = Dec08.find_cycle(map, instructions, start, %{}, 0, true)

             Map.keys(seen) |> Enum.map(fn {pos, _} -> pos end) |> Enum.uniq() |> length() == 1
           end)
  end
end
