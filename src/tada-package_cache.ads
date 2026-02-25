with Tada.Packages;

package Tada.Package_Cache is
   use Packages;

   function Root_Path return String;

   function Package_Path (P : Package_Info) return String;

   function Manifest_Path (P : Package_Info) return String;

   function GPR_Path (P : Package_Info) return String;

   function GPR_Config_Path (P : Package_Info) return String;

   function GPR_Deps_Path (P : Package_Info) return String;

   function Is_Cached (P : Package_Info) return Boolean;
end Tada.Package_Cache;
