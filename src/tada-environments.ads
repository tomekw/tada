package Tada.Environments is
   Environment_Error : exception;

   type Environment is tagged private;

   function Is_Windows return Boolean;

   function Get_Exe_Suffix return String;

   function Init return Environment;

   function GNAT_Path (Self : Environment) return String;

   function GPRBuild_Path (Self : Environment) return String;

   function Config_Source (Self : Environment) return String;

private

   type Config_Source_Kind is (Local, Global, Path);

   type Environment is tagged record
      GNAT_Path_Holder : String_Holders.Holder;
      GPRBuild_Path_Holder : String_Holders.Holder;
      Config_Source : Config_Source_Kind;
   end record;
end Tada.Environments;
