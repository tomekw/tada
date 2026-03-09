with Ada.Directories;
with Ada.Environment_Variables;

with GNAT.OS_Lib;
with GNAT.Strings;

with Tada.Configs;

package body Tada.Environments is
   use Ada.Directories;

   package OS renames GNAT.OS_Lib;

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

   function Init return Environment is
      function From_Config (Config_Path : String; Source : Config_Source_Kind) return Environment is
         Tada_Config : constant Configs.Config := Configs.Read (Config_Path);
         Toolchain : constant String_Maps.Map := Tada_Config.Sections ("toolchain");
      begin
         return (GNAT_Path_Holder => String_Holders.To_Holder (Compose (Compose (Toolchain ("gnat_root"), "bin"), "gnat") & Get_Exe_Suffix),
                 GPRBuild_Path_Holder => String_Holders.To_Holder (Compose (Compose (Toolchain ("gprbuild_root"), "bin"), "gprbuild") & Get_Exe_Suffix),
                 Config_Source => Source);
      end From_Config;
   begin
      if Exists (Local_Config_Path) then
         return From_Config (Local_Config_Path, Local);
      elsif Exists (Global_Config_Path) then
         return From_Config (Global_Config_Path, Global);
      else
         return (GNAT_Path_Holder => String_Holders.To_Holder (Exec_Path ("gnat")),
                 GPRBuild_Path_Holder => String_Holders.To_Holder (Exec_Path ("gprbuild")),
                 Config_Source => Path);
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
