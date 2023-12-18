defmodule Dec11Test do
  use ExUnit.Case
  doctest Dec11

  test "Dec 11" do
    input = File.read!("../../inputs/2023/11.txt")

    assert Dec11.a(input) == 9_681_886
    assert Dec11.b(input) == 791_134_099_634
  end
end
