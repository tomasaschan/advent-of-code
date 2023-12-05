defmodule Dec04Test do
  use ExUnit.Case
  doctest Dec04

  test "dec 04" do
    input = File.read!("../../inputs/2023/04.txt")
    assert Dec04.a(input) == 28538
    assert Dec04.b(input) == 9_425_061
  end
end
