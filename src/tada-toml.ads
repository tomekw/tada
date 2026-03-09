package Tada.TOML is
   Parse_Error : exception;

   function Parse (Path : String) return Section_Maps.Map;
end Tada.TOML;
