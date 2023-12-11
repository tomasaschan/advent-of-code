defmodule Dec08Test do
  use ExUnit.Case
  doctest Dec08

  test "Dec08" do
    input = File.read!("../../inputs/2023/08.txt")
    assert Dec08.a(input) == 14893
    assert Dec08.b(input) == 10_241_191_004_509
  end

  test "every path has a cycle" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    assert starts
           |> Enum.map(fn start -> Dec08.find_cycle(map, instructions, start) end)
           |> Enum.all?(fn {_, first, last} -> last > first end)
  end

  test "every path reaches only a single end during its cycle" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    assert starts
           |> Enum.map(fn start ->
             {_, _, _, seen} = Dec08.find_cycle(map, instructions, start, %{}, 0, true)

             Map.keys(seen)
             |> Enum.map(fn {pos, _} -> pos end)
             |> Enum.filter(&Dec08.done_b/1)
             |> Enum.uniq()
             |> length()
           end)
           |> Enum.all?(fn ends -> ends == 1 end)
  end

  test "every cycle starts and ends at the start of the instruction set" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    cycles = starts |> Enum.map(fn start -> Dec08.find_cycle(map, instructions, start) end)

    assert cycles
           |> Enum.map(fn {_, first, last} ->
             {rem(first, length(instructions)), rem(last, length(instructions))}
           end)
           |> Enum.all?(fn {first, last} -> first == 0 and last == 0 end)
  end

  test "every cycle is exactly as long as it takes to reach the end the first time" do
    input = File.read!("../../inputs/2023/08.txt")
    {instructions, map} = Dec08.parse(input)

    starts = map |> Map.keys() |> Enum.filter(fn k -> k |> String.ends_with?("A") end)

    cycles = starts |> Enum.map(fn start -> Dec08.find_cycle(map, instructions, start) end)

    assert cycles |> Enum.all?(fn {_, first, last} -> last = 2 * first end)
  end
end
