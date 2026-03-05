package Tada.Packages is
   Manifest_Name : constant String := "tada.toml";

   type Package_Info is tagged private;

   function Is_Valid_Name (Name : String) return Boolean;

   function Create (Name : String; Version : String) return Package_Info;

   function Name (Self : Package_Info) return String;

   function Version (Self : Package_Info) return String;

   function GPR_Name (Self : Package_Info) return String;

   function GPR_Config_Name (Self : Package_Info) return String;

   function GPR_Deps_Name (Self : Package_Info) return String;

   function GPR_Tests_Deps_Name (Self : Package_Info) return String;

   function GPR_Tests_Name (Self : Package_Info) return String;
private

   type Package_Info is tagged record
      Name_Holder : String_Holders.Holder;
      Version_Holder : String_Holders.Holder;
   end record;
end Tada.Packages;
