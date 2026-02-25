with Ada.Directories;
with Ada.Environment_Variables;

with GNAT.OS_Lib;

package body Tada.Package_Cache is
   use Ada;
   use Ada.Directories;

   package OS renames GNAT.OS_Lib;

   function Root_Path return String is
      Is_Windows : constant Boolean := OS.Directory_Separator = '\';
   begin
      if Is_Windows then
         return Compose (Compose (Environment_Variables.Value ("LOCALAPPDATA"), "tada"), "package");
      else
         return Compose (Compose (Compose (Environment_Variables.Value ("HOME"), ".cache"), "tada"), "package");
      end if;
   end Root_Path;

   function Package_Path (P : Package_Info) return String is
   begin
      return Compose (Compose (Root_Path, P.Name), P.Version);
   end Package_Path;

   function Manifest_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), Packages.Manifest_Name);
   end Manifest_Path;

   function GPR_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Name);
   end GPR_Path;

   function GPR_Config_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Config_Name);
   end GPR_Config_Path;

   function GPR_Deps_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Deps_Name);
   end GPR_Deps_Path;

   function Is_Cached (P : Package_Info) return Boolean is
   begin
      return Exists (Manifest_Path (P));
   end Is_Cached;
end Tada.Package_Cache;
