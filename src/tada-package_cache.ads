with Tada.Packages;

package Tada.Package_Cache is
   use Packages;

   Package_Cache_Error : exception;

   function Cache_Path return String;

   function Package_Path (P : Package_Info) return String;

   function Manifest_Path (P : Package_Info) return String;

   function GPR_Path (P : Package_Info) return String;

   function GPR_Config_Path (P : Package_Info) return String;

   function GPR_Deps_Path (P : Package_Info) return String;

   function Is_Cached (P : Package_Info) return Boolean;

   procedure Cache_Package (Package_Tmp_Path : String);
end Tada.Package_Cache;
