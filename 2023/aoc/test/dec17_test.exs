defmodule Dec17Test do
  use ExUnit.Case
  doctest Dec17

  test "Dec17" do
    input = File.read!("../../inputs/2023/17.txt")

    assert Dec17.a(input) == 1263
    assert Dec17.b(input) == 1411
  end
end
