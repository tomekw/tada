package Tada.Environments is
   Environment_Error : exception;

   type Environment is private;

   function Is_Windows return Boolean;

   function Exec_Path (Exec_Name : String) return String;

   function Get_Exe_Suffix return String;

   function Init return Environment;

   function GNAT_Path (Self : Environment) return String;

   function GPRBuild_Path (Self : Environment) return String;

   function Config_Source (Self : Environment) return String;

   function Operating_System (Self : Environment) return String;

   function Architecture (Self : Environment) return String;

private

   type Config_Source_Kind is (Local, Global, Path);

   type Operating_System_Kind is (FreeBSD, Linux, MacOS, OpenBSD, Windows, Unknown);
   type Architecture_Kind is (Aarch64, X86_64, Unknown);

   type Environment is record
      GNAT_Path_Holder : String_Holders.Holder;
      GPRBuild_Path_Holder : String_Holders.Holder;
      Config_Source : Config_Source_Kind;
      Operating_System : Operating_System_Kind;
      Architecture : Architecture_Kind;
   end record;
end Tada.Environments;
