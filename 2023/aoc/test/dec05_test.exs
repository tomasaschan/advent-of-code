defmodule Dec05Test do
  use ExUnit.Case
  doctest Dec05

  test "Dec05" do
    input = File.read!("../../inputs/2023/05.txt")
    assert Dec05.a(input) == 1_181_555_926
    assert Dec05.b(input) == 0
  end
end
