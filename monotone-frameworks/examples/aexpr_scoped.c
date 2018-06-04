begin
  proc bereken_maar_eens_wat(val erin, res eruit) is
    a := 2 + 3 + x + y + z;
    z := (99);
    if z < 3 then {
      eruit := z;
    } else {
      call bereken_maar_eens_wat(z, eruit);
      v := v + u;
    }
  end

  y := 2;
  z := 1;

end