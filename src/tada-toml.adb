with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Tada.TOML is
   function Parse (Path : String) return Section_Maps.Map is
      File : Text_IO.File_Type;

      Sections : Section_Maps.Map;

      Current_Section : String_Holders.Holder;
      Line_Number : Natural := 0;
   begin
      begin
         Text_IO.Open (File, Text_IO.In_File, Path);
      exception
         when Text_IO.Name_Error =>
            raise Parse_Error with "file '" & Path & "' not found";
      end;

      while not Text_IO.End_Of_File (File) loop
         Line_Number := Line_Number + 1;

         declare
            Line : constant String := Strings.Fixed.Trim (Text_IO.Get_Line (File), Strings.Both);
            Line_Info : constant String := "Line" & Line_Number'Image & ": ";
         begin
            if Line'Length > 0 and then Line (Line'First) /= '#' then
               if Line'Length > 2 and then
                  Line (Line'First) = '[' and then
                  Line (Line'Last) = ']'
               then
                  declare
                     Section_Name : constant String := Characters.Handling.To_Lower (Line (Line'First + 1 .. Line'Last - 1));
                  begin
                     if Sections.Contains (Section_Name) then
                        raise Parse_Error with Line_Info & "duplicate section [" & Section_Name & "]";
                     end if;

                     Current_Section := String_Holders.To_Holder (Section_Name);
                     Sections.Insert (Section_Name, String_Maps.Empty_Map);
                  end;
               else
                  if Current_Section.Is_Empty then
                     raise Parse_Error with Line_Info & "key outside of section";
                  end if;

                  declare
                     Eq_Position : constant Natural := Strings.Fixed.Index (Line, "=");
                  begin
                     if Eq_Position = 0 or else
                        Eq_Position = Line'First or else
                        Eq_Position = Line'Last
                     then
                        raise Parse_Error with Line_Info & "malformed file";
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
                           raise Parse_Error with Line_Info & "invalid value for '" & Key & "'";
                        end if;

                        declare
                           Value : constant String := Raw_Value (Raw_Value'First + 1 .. Raw_Value'Last - 1);
                           Section : constant String := Current_Section.Element;
                        begin
                           if Sections (Section).Contains (Key) then
                              raise Parse_Error with Line_Info & "duplicate key '" & Key & "' in [" & Section & "]";
                           end if;

                           Sections (Section).Insert (Key, Value);
                        end;
                     end;
                  end;
               end if;
            end if;
         end;
      end loop;

      Text_IO.Close (File);

      return Sections;
   exception
      when others =>
         if Text_IO.Is_Open (File) then
            Text_IO.Close (File);
         end if;

         raise;
   end Parse;
end Tada.TOML;
