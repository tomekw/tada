package Tada.Configs is
   Config_Error : exception;

   type Config is record
      Sections : Section_Maps.Map;
   end record;

   function Read (Config_Path : String) return Config;
end Tada.Configs;
