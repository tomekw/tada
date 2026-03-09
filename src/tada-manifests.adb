with Ada.Exceptions;

with Tada.Packages;
with Tada.TOML;

package body Tada.Manifests is
   procedure Validate (Sections : Section_Maps.Map) is
   begin
      for C in Sections.Iterate loop
         declare
            Section_Name : constant String := Section_Maps.Key (C);
         begin
            if Section_Name /= "package" and then
               Section_Name /= "dependencies" and then
               Section_Name /= "dev-dependencies"
            then
               raise Manifest_Error with "unknown section [" & Section_Name & "]";
            end if;
         end;
      end loop;

      if not Sections.Contains ("package") then
         raise Manifest_Error with "missing [package] section";
      end if;

      declare
         Package_Section : constant String_Maps.Map := Sections ("package");
      begin
         if not Package_Section.Contains ("name") then
            raise Manifest_Error with "missing 'name' in [package]";
         end if;

         declare
            Package_Name : constant String := Package_Section ("name");
         begin
            if not Packages.Is_Valid_Name (Package_Name) then
               raise Manifest_Error with "invalid package name '" & Package_Name & "'";
            end if;
         end;

         if not Package_Section.Contains ("version") then
            raise Manifest_Error with "missing 'version' in [package]";
         end if;

         declare
            Package_Version : constant String := Package_Section ("version");
         begin
            if not Packages.Is_Valid_Version (Package_Version) then
               raise Manifest_Error with "invalid package version '" & Package_Version & "'";
            end if;
         end;

         for C in Package_Section.Iterate loop
            declare
               Key : constant String := String_Maps.Key (C);
            begin
               if Key /= "name" and then
                  Key /= "version"
               then
                  raise Manifest_Error with "unknown key '" & Key & "' in [package]";
               end if;
            end;
         end loop;
      end;

      if Sections.Contains ("dependencies") then
         declare
            Dependencies_Section : constant String_Maps.Map := Sections ("dependencies");
         begin
            for C in Dependencies_Section.Iterate loop
               declare
                  Dependency_Name : constant String := String_Maps.Key (C);
                  Dependency_Version : constant String := String_Maps.Element (C);
               begin
                  if not Packages.Is_Valid_Name (Dependency_Name) then
                     raise Manifest_Error with "invalid dependency name '" & Dependency_Name & "'";
                  end if;

                  if not Packages.Is_Valid_Version (Dependency_Version) then
                     raise Manifest_Error with "invalid dependency version '" & Dependency_Version & "'";
                  end if;
               end;
            end loop;
         end;
      end if;

      if Sections.Contains ("dev-dependencies") then
         declare
            Dev_Dependencies_Section : constant String_Maps.Map := Sections ("dev-dependencies");
         begin
            for C in Dev_Dependencies_Section.Iterate loop
               declare
                  Dependency_Name : constant String := String_Maps.Key (C);
                  Dependency_Version : constant String := String_Maps.Element (C);
               begin
                  if not Packages.Is_Valid_Name (Dependency_Name) then
                     raise Manifest_Error with "invalid dependency name '" & Dependency_Name & "'";
                  end if;

                  if not Packages.Is_Valid_Version (Dependency_Version) then
                     raise Manifest_Error with "invalid dependency version '" & Dependency_Version & "'";
                  end if;
               end;
            end loop;
         end;
      end if;
   end Validate;

   function Read (Manifest_Path : String) return Manifest is
      Sections : Section_Maps.Map;
   begin
      Sections := TOML.Parse (Manifest_Path);

      Validate (Sections);

      return (Sections => Sections);
   exception
      when E : others =>
         raise Manifest_Error with Exceptions.Exception_Message (E);
   end Read;
end Tada.Manifests;
