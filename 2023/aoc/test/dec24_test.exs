defmodule Dec24Test do
  use ExUnit.Case
  doctest Dec24

  test "Dec 24" do
    input = File.read!("../../inputs/2023/24.txt")

    assert Dec24.a(input) == 15889
  end
end
