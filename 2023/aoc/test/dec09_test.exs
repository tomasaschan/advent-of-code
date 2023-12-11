defmodule Dec09Test do
  use ExUnit.Case
  doctest Dec09

  test "Dec 09" do
    input = File.read!("../../inputs/2023/09.txt")
    assert Dec09.a(input) == 2_038_472_161
    assert Dec09.b(input) == 1091
  end
end
