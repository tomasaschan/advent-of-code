defmodule Dec20Test do
  use ExUnit.Case
  doctest Dec20

  test "Dec 20" do
    input = File.read!("../../inputs/2023/20.txt")

    assert Dec20.a(input) == 737_679_780
    assert Dec20.b(input) == 227_411_378_431_763
  end
end
