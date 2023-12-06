defmodule Dec06Tests do
  use ExUnit.Case
  doctest Dec06

  test "Dec06" do
    input = File.read!("../../inputs/2023/06.txt")
    assert Dec06.a(input) == 1_710_720
    assert Dec06.b(input) == 35_349_468
  end
end
