package Tada.Runners is
   function Run_CURL (Source : String; Target : String) return Boolean;

   function Run_GPRBuild (Project : String; Profile : String) return Boolean;

   function Run_Tar (Source : String; Target : String) return Boolean;
end Tada.Runners;
