defmodule Dec15Test do
  use ExUnit.Case
  doctest Dec15

  test "Dec 15" do
    input = File.read!("../../inputs/2023/15.txt")

    assert Dec15.a(input) == 510_801
    assert Dec15.b(input) == 212_763
  end
end
