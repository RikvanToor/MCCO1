begin
  y := 2;
  z := 1;
  while x > 0 do {
    z := z * y;
    if x > 3 then {
        break;
    }
    else {
        x := x - 1;
    }
  }
continue;
end