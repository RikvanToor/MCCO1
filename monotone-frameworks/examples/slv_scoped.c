begin
  proc square(val u, res v) is
    v := u * u;
  end
  v := 3;
  call square(v,v);
end