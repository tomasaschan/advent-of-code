defmodule Dec03Test do
  use ExUnit.Case
  doctest Dec03

  test "dec 03" do
    input = File.read!("../../inputs/2023/03.txt")
    assert Dec03.a(input) == 536_576
    assert Dec03.b(input) == 75_741_499
  end
end
