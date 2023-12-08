defmodule Dec07Test do
  use ExUnit.Case
  doctest Dec07

  test "Dec07" do
    input = File.read!("../../inputs/2023/07.txt")
    assert Dec07.a(input) == 246_795_406
    assert Dec07.b(input) == 249_356_515
  end
end
