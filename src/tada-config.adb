with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Tada.Config is
   use Ada;

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

   function Valid_Project_Name (Name : String) return Boolean is
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
   end Valid_Project_Name;

   function Parse (Manifest_Path : String) return Manifest_Results.Result is
      use Manifest_Results;

      Manifest_File : Text_IO.File_Type;

      Eq : constant String := "=";

      Project_Name : Project_Name_Results.Result;
      Project_Version : Project_Version_Results.Result;
   begin
      begin
         Text_IO.Open (Manifest_File, Text_IO.In_File, Manifest_Path);
      exception
         when Text_IO.Name_Error =>
            return (Status => Error,
                    Message => To_Unbounded_String
                      ("manifest file '" & Manifest_Path & "' not found"));
      end;

      while not Text_IO.End_Of_File (Manifest_File) loop
         declare
            Line : constant String := Strings.Fixed.Trim (Text_IO.Get_Line (Manifest_File), Strings.Both);
            Eq_Position : constant Natural := Strings.Fixed.Index (Line, Eq);
         begin
            if Line'Length > 0 and then
               Eq_Position > 0
            then
               declare
                  Key : constant String := Characters.Handling.To_Lower
                    (Strings.Fixed.Trim
                      (Line (Line'First .. Eq_Position - 1), Strings.Both));
                  Raw_Value : constant String := Strings.Fixed.Trim
                    (Line (Eq_Position + 1 .. Line'Last), Strings.Both);
                  Value : constant String := Raw_Value (Raw_Value'First + 1 .. Raw_Value'Last - 1);
               begin
                  if Raw_Value'Length < 2 or else
                     Raw_Value (Raw_Value'First) /= '"' or else
                     Raw_Value (Raw_Value'Last) /= '"'
                  then
                     Text_IO.Close (Manifest_File);
                     return (Status => Error,
                             Message => To_Unbounded_String
                               ("invalid value for '" & Key & "'"));
                  end if;

                  if Key = "name" then
                     if Valid_Project_Name (Value) then
                        Project_Name := (Status => Project_Name_Results.Ok,
                                         Value => To_Unbounded_String (Value));
                     else
                        Text_IO.Close (Manifest_File);
                        return (Status => Error,
                                Message => To_Unbounded_String
                                  ("invalid project name '" & Value & "'"));
                     end if;
                  elsif Key = "version" then
                     Project_Version := (Status => Project_Version_Results.Ok,
                                         Value => To_Unbounded_String (Value));
                  else
                     Text_IO.Close (Manifest_File);
                     return (Status => Error,
                             Message => To_Unbounded_String
                               ("unknown manifest key '" & Key & "'"));
                  end if;
               end;
            end if;
         end;
      end loop;

      Text_IO.Close (Manifest_File);

      return (Status => Ok,
              Value => (Name => Project_Name,
                        Version => Project_Version));
   exception
      when others =>
         if Text_IO.Is_Open (Manifest_File) then
            Text_IO.Close (Manifest_File);
         end if;

         return (Status => Error,
                 Message => To_Unbounded_String
                   ("invalid manifest file"));
   end Parse;
end Tada.Config;
