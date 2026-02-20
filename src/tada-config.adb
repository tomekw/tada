with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Tada.Config is
   use Ada.Strings.Unbounded;

   package String_Vectors is new Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   Reserved_Words : constant String_Vectors.Vector :=
     ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and", "array", "at",
       "begin", "body",
       "case", "constant",
       "declare", "delay", "delta", "digits", "do",
       "else", "elsif", "end", "entry", "exception",
       "exit",
       "for", "function",
       "generic", "goto",
       "if", "in", "interface", "is",
       "limited", "loop",
       "mod",
       "new", "not", "null",
       "of", "or", "others", "out", "overriding",
       "package", "parallel", "pragma", "private",
       "procedure", "protected",
       "raise", "range", "record", "rem", "renames",
       "requeue", "return", "reverse",
       "select", "separate", "some", "subtype",
       "synchronized",
       "tagged", "task", "terminate", "then", "type",
       "until", "use",
       "when", "while", "with",
       "xor"];

   function Is_Reserved_Word (Name : String) return Boolean is
      use Ada.Characters.Handling;

      Lower_Name : constant String := To_Lower (Name);
   begin
      for Word of Reserved_Words loop
         if Lower_Name = Word then
            return True;
         end if;
      end loop;

      return False;
   end Is_Reserved_Word;

   function Valid_Package_Name (Name : String) return Boolean is
      use Characters.Handling;

      Underscore : constant Character := '_';
   begin
      if Name'Length = 0 or else
         not Is_Letter (Name (Name'First)) or else
         not Is_Alphanumeric (Name (Name'Last)) or else
         Is_Reserved_Word (Name)
      then
         return False;
      else
         for I in Name'First .. Name'Last - 1 loop
            if not (Is_Alphanumeric (Name (I)) or else
                    Name (I) = Underscore)
            then
               return False;
            end if;

            if Name (I) = Underscore and then
               Name (I + 1) = Underscore
            then
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Valid_Package_Name;

   function Read (Manifest_Path : String) return Manifest is
      Manifest_File : Text_IO.File_Type;
      Tada_Manifest : Manifest;

      Current_Section : Unbounded_String;
      Line_Number : Natural := 0;

      procedure Validate is
      begin
         for C in Tada_Manifest.Sections.Iterate loop
            declare
               Section_Name : constant String := Section_Maps.Key (C);
            begin
               if Section_Name /= "package" and then
                  Section_Name /= "dependencies"
               then
                  raise Manifest_Error with "unknown section [" & Section_Name & "]";
               end if;
            end;
         end loop;

         if not Tada_Manifest.Sections.Contains ("package") then
            raise Manifest_Error with "missing [package] section";
         end if;

         declare
            Package_Section : constant String_Maps.Map := Tada_Manifest.Sections ("package");
         begin
            if not Package_Section.Contains ("name") then
               raise Manifest_Error with "missing 'name' in [package]";
            end if;

            declare
               Package_Name : constant String := Package_Section ("name");
            begin
               if not Valid_Package_Name (Package_Name) then
                  raise Manifest_Error with "invalid package name '" & Package_Name & "'";
               end if;
            end;

            if not Package_Section.Contains ("version") then
               raise Manifest_Error with "missing 'version' in [package]";
            end if;

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

         if Tada_Manifest.Sections.Contains ("dependencies") then
            declare
               Dependencies_Section : constant String_Maps.Map := Tada_Manifest.Sections ("dependencies");
            begin
               for C in Dependencies_Section.Iterate loop
                  declare
                     Dependency_Name : constant String := String_Maps.Key (C);
                  begin
                     if not Valid_Package_Name (Dependency_Name) then
                        raise Manifest_Error with "invalid dependency name '" & Dependency_Name & "'";
                     end if;
                  end;
               end loop;
            end;
         end if;
      end Validate;
   begin
      begin
         Text_IO.Open (Manifest_File, Text_IO.In_File, Manifest_Path);
      exception
         when Text_IO.Name_Error =>
            raise Manifest_Error with "manifest file '" & Manifest_Path & "' not found";
      end;

      while not Text_IO.End_Of_File (Manifest_File) loop
         Line_Number := Line_Number + 1;

         declare
            Line : constant String := Strings.Fixed.Trim (Text_IO.Get_Line (Manifest_File), Strings.Both);
            Line_Info : constant String := "Line" & Line_Number'Image & ": ";
         begin
            if Line'Length > 0 and then Line (Line'First) /= '#' then
               if Line'Length > 2 and then
                  Line (Line'First) = '[' and then
                  Line (Line'Last) = ']'
               then
                  declare
                     Section_Name : constant String := Line (Line'First + 1 .. Line'Last - 1);
                  begin
                     if Tada_Manifest.Sections.Contains (Section_Name) then
                        raise Manifest_Error with Line_Info & "duplicate section [" & Section_Name & "]";
                     end if;

                     Current_Section := To_Unbounded_String (Section_Name);
                     Tada_Manifest.Sections.Insert (Section_Name, String_Maps.Empty_Map);
                  end;
               else
                  if Length (Current_Section) = 0 then
                     raise Manifest_Error with Line_Info & "key outside of section";
                  end if;

                  declare
                     Eq_Position : constant Natural := Strings.Fixed.Index (Line, "=");
                  begin
                     if Eq_Position = 0 or else
                        Eq_Position = Line'First or else
                        Eq_Position = Line'Last
                     then
                        raise Manifest_Error with Line_Info & "malformed manifest file";
                     end if;

                     declare
                        Key : constant String := Characters.Handling.To_Lower
                          (Strings.Fixed.Trim
                            (Line (Line'First .. Eq_Position - 1), Strings.Both));
                        Raw_Value : constant String := Strings.Fixed.Trim
                          (Line (Eq_Position + 1 .. Line'Last), Strings.Both);
                     begin
                        if Raw_Value'Length < 2 or else
                           Raw_Value (Raw_Value'First) /= '"' or else
                           Raw_Value (Raw_Value'Last) /= '"'
                        then
                           raise Manifest_Error with Line_Info & "invalid value for '" & Key & "'";
                        end if;

                        declare
                           Value : constant String := Raw_Value (Raw_Value'First + 1 .. Raw_Value'Last - 1);
                           Section : constant String := To_String (Current_Section);
                        begin
                           if Tada_Manifest.Sections (Section).Contains (Key) then
                              raise Manifest_Error with Line_Info & "duplicate key '" & Key & "' in [" & Section & "]";
                           end if;

                           Tada_Manifest.Sections (Section).Insert (Key, Value);
                        end;
                     end;
                  end;
               end if;
            end if;
         end;
      end loop;

      Text_IO.Close (Manifest_File);

      Validate;

      return Tada_Manifest;
   exception
      when others =>
         if Text_IO.Is_Open (Manifest_File) then
            Text_IO.Close (Manifest_File);
         end if;

         raise;
   end Read;
end Tada.Config;
