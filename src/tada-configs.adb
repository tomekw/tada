with Ada.Directories;
with Ada.Exceptions;

with Tada.TOML;

package body Tada.Configs is
   procedure Validate (Sections : Section_Maps.Map) is
   begin
      for C in Sections.Iterate loop
         declare
            Section_Name : constant String := Section_Maps.Key (C);
         begin
            if Section_Name /= "toolchain" then
               raise Config_Error with "unknown section [" & Section_Name & "]";
            end if;
         end;
      end loop;

      if not Sections.Contains ("toolchain") then
         raise Config_Error with "missing [toolchain] section";
      end if;

      declare
         Toolchain_Section : constant String_Maps.Map := Sections ("toolchain");
      begin
         if not Toolchain_Section.Contains ("gnat_root") then
            raise Config_Error with "missing 'gnat_root' in [toolchain]";
         end if;

         declare
            GNAT_Root : constant String := Toolchain_Section ("gnat_root");
         begin
            if not Directories.Exists (GNAT_Root) then
               raise Config_Error with "gnat_root '" & GNAT_Root & "' doesn't exist";
            end if;
         end;

         if not Toolchain_Section.Contains ("gprbuild_root") then
            raise Config_Error with "missing 'gprbuild_root' in [toolchain]";
         end if;

         declare
            GPRBuild_Root : constant String := Toolchain_Section ("gprbuild_root");
         begin
            if not Directories.Exists (GPRBuild_Root) then
               raise Config_Error with "gprbuild_root '" & GPRBuild_Root & "' doesn't exist";
            end if;
         end;

         for C in Toolchain_Section.Iterate loop
            declare
               Key : constant String := String_Maps.Key (C);
            begin
               if Key /= "gnat_root" and then
                  Key /= "gprbuild_root"
               then
                  raise Config_Error with "unknown key '" & Key & "' in [toolchain]";
               end if;
            end;
         end loop;
      end;
   end Validate;

   function Read (Config_Path : String) return Config is
      Sections : Section_Maps.Map;
   begin
      Sections := TOML.Parse (Config_Path);

      Validate (Sections);

      return (Sections => Sections);
   exception
      when E : others =>
         raise Config_Error with Exceptions.Exception_Message (E);
   end Read;
end Tada.Configs;
