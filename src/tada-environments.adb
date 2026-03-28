with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;

with GNAT.OS_Lib;
with GNAT.Strings;

pragma Warnings (Off, "gnatwi");
with System.OS_Constants;
pragma Warnings (On, "gnatwi");

with Tada.Configs;

package body Tada.Environments is
   use Ada.Directories;

   package OS renames GNAT.OS_Lib;

   function Architecture (Self : Environment) return String is
   begin
      case Self.Architecture is
         when Aarch64 =>
            return "aarch64";
         when X86_64 =>
            return "x86_64";
         when Unknown =>
            return "unknown";
      end case;
   end Architecture;

   function Operating_System (Self : Environment) return String is
   begin
      case Self.Operating_System is
         when FreeBSD =>
            return "freebsd";
         when Linux =>
            return "linux";
         when MacOS =>
            return "macos";
         when OpenBSD =>
            return "openbsd";
         when Windows =>
            return "windows";
         when Unknown =>
            return "unknown";
      end case;
   end Operating_System;

   function Is_Windows return Boolean is
   begin
      return OS.Directory_Separator = '\';
   end Is_Windows;

   function Get_Exe_Suffix return String is
      Suffix : OS.String_Access := OS.Get_Target_Executable_Suffix;
      Result : constant String := Suffix.all;
   begin
      OS.Free (Suffix);
      return Result;
   end Get_Exe_Suffix;

   function Local_Config_Path return String is
   begin
      return Compose (Compose (Current_Directory, ".tada"), "config.toml");
   end Local_Config_Path;

   function Global_Config_Path return String is
   begin
      if Is_Windows then
         return Compose (Compose (Environment_Variables.Value ("LOCALAPPDATA"), "tada"), "config.toml");
      else
         return Compose (Compose (Compose (Environment_Variables.Value ("HOME"), ".config"), "tada"), "config.toml");
      end if;
   end Global_Config_Path;

   function Exec_Path (Exec_Name : String) return String is
      use type GNAT.Strings.String_Access;

      Path : OS.String_Access := OS.Locate_Exec_On_Path (Exec_Name);
      Path_Holder : String_Holders.Holder;
   begin
      if Path /= null then
         Path_Holder := String_Holders.To_Holder (Path.all);
         OS.Free (Path);

         return Path_Holder.Element;
      else
         OS.Free (Path);

         raise Environment_Error with "could not find executable '" & Exec_Name & "' in PATH";
      end if;
   end Exec_Path;

   function Parse_Operating_System (Target_Name : String) return Operating_System_Kind is
      use Ada.Strings.Fixed;
   begin
      if Index (Target_Name, "linux") /= 0 then
         return Linux;
      elsif Index (Target_Name, "freebsd") /= 0 then
         return FreeBSD;
      elsif Index (Target_Name, "openbsd") /= 0 then
         return OpenBSD;
      elsif Index (Target_Name, "apple") /= 0 then
         return MacOS;
      elsif Index (Target_Name, "mingw") /= 0 or else
            OS.Directory_Separator = '\'
      then
         return Windows;
      else
         return Unknown;
      end if;
   end Parse_Operating_System;

   function Parse_Architecture (Target_Name : String) return Architecture_Kind is
      use Ada.Strings.Fixed;
   begin
      if Index (Target_Name, "x86_64") = Target_Name'First then
         return X86_64;
      elsif Index (Target_Name, "aarch64") = Target_Name'First then
         return Aarch64;
      else
         return Unknown;
      end if;
   end Parse_Architecture;

   function Init return Environment is
      OS : constant Operating_System_Kind := Parse_Operating_System (System.OS_Constants.Target_Name);
      Arch : constant Architecture_Kind := Parse_Architecture (System.OS_Constants.Target_Name);

      function From_Config (Config_Path : String; Source : Config_Source_Kind) return Environment is
         Tada_Config : constant Configs.Config := Configs.Read (Config_Path);
         Toolchain : constant String_Maps.Map := Tada_Config.Sections ("toolchain");
      begin
         return (GNAT_Path_Holder => String_Holders.To_Holder (Compose (Toolchain ("gnat_root"), "bin")),
                 GPRBuild_Path_Holder => String_Holders.To_Holder (Compose (Toolchain ("gprbuild_root"), "bin")),
                 Config_Source => Source,
                 Operating_System => OS,
                 Architecture => Arch);
      end From_Config;
   begin
      if Exists (Local_Config_Path) then
         return From_Config (Local_Config_Path, Local);
      elsif Exists (Global_Config_Path) then
         return From_Config (Global_Config_Path, Global);
      else
         return (GNAT_Path_Holder => String_Holders.To_Holder (Containing_Directory (Exec_Path ("gnat"))),
                 GPRBuild_Path_Holder => String_Holders.To_Holder (Containing_Directory (Exec_Path ("gprbuild"))),
                 Config_Source => Path,
                 Operating_System => OS,
                 Architecture => Arch);
      end if;
   end Init;

   function GNAT_Path (Self : Environment) return String is
   begin
      return Self.GNAT_Path_Holder.Element;
   end GNAT_Path;

   function GPRBuild_Path (Self : Environment) return String is
   begin
      return Self.GPRBuild_Path_Holder.Element;
   end GPRBuild_Path;

   function Config_Source (Self : Environment) return String is
   begin
      case Self.Config_Source is
         when Local =>
            return "local";
         when Global =>
            return "global";
         when Path =>
            return "PATH";
      end case;
   end Config_Source;
end Tada.Environments;
