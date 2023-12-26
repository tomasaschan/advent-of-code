defmodule Dec14Test do
  use ExUnit.Case
  doctest Dec14

  test "Dec 14" do
    input = File.read!("../../inputs/2023/14.txt")

    assert Dec14.a(input) == 106_378
  end
end
