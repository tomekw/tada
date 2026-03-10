with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;

package body Tada.Packages is
   package String_Vectors is new Containers.Indefinite_Vectors
     (Index_Type => Positive,
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

   function Is_Valid_Name (Name : String) return Boolean is
      use Characters.Handling;

      Underscore : constant Character := '_';
      Lower_Name : constant String := To_Lower (Name);
   begin
      if Name'Length = 0 or else
         not Is_Letter (Name (Name'First)) or else
         not Is_Alphanumeric (Name (Name'Last)) or else
         Lower_Name /= Name or else
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
   end Is_Valid_Name;

   function Is_Valid_Segment (Segment : String) return Boolean is
      use Characters.Handling;
   begin
      if Segment'Length = 0 then
         return False;
      end if;

      for C of Segment loop
         if not Is_Digit (C) then
            return False;
         end if;
      end loop;

      if Segment (Segment'First) = '0' and then
         Segment'Length > 1
      then
         return False;
      end if;

      return True;
   end Is_Valid_Segment;

   function Is_Valid_Version (Version : String) return Boolean is
      use Characters.Handling;

      Dot_One : Natural;
      Dot_Two : Natural;
      Dot_Three : Natural;
   begin
      if Version'Length < 5 then
         return False;
      end if;

      Dot_One := Strings.Fixed.Index (Version, ".", From => Version'First);
      if Dot_One = 0 then
         return False;
      end if;

      Dot_Two := Strings.Fixed.Index (Version, ".", From => Dot_One + 1);
      if Dot_Two = 0 then
         return False;
      end if;

      Dot_Three := Strings.Fixed.Index (Version, ".", From => Dot_Two + 1);
      if Dot_Three /= 0 then
         return False;
      end if;

      declare
         Major : constant String := Version (Version'First .. Dot_One - 1);
         Minor : constant String := Version (Dot_One + 1 .. Dot_Two - 1);

         Dash : constant Natural := Strings.Fixed.Index (Version, "-", From => Dot_Two + 1);
      begin
         if not Is_Valid_Segment (Major) then
            return False;
         end if;

         if not Is_Valid_Segment (Minor) then
            return False;
         end if;

         if Dash = 0 then
            return Is_Valid_Segment (Version (Dot_Two + 1 .. Version'Last));
         else
            if not Is_Valid_Segment (Version (Dot_Two + 1 .. Dash - 1)) then
               return False;
            end if;

            declare
               Prerelease : constant String := Version (Dash + 1 .. Version'Last);
            begin
               if Prerelease'Length = 0 then
                  return False;
               end if;

               for C of Prerelease loop
                  if not Is_Letter (C) and then
                     not Is_Digit (C) and then
                     C /= '_'
                  then
                     return False;
                  end if;
               end loop;
            end;
         end if;
      end;

      return True;
   end Is_Valid_Version;

   function Create (Name : String; Version : String) return Package_Info is
      use String_Holders;

      Lower_Name : constant String := Characters.Handling.To_Lower (Name);
   begin
      if not Is_Valid_Name (Lower_Name) then
         raise Constraint_Error with "invalid package name";
      end if;

      return (Name_Holder => To_Holder (Lower_Name), Version_Holder => To_Holder (Version));
   end Create;

   function Name (Self : Package_Info) return String is
   begin
      return Self.Name_Holder.Element;
   end Name;

   function Version (Self : Package_Info) return String is
   begin
      return Self.Version_Holder.Element;
   end Version;

   function GPR_Name (Self : Package_Info) return String is
   begin
      return Self.Name & ".gpr";
   end GPR_Name;

   function GPR_Config_Name (Self : Package_Info) return String is
   begin
      return Self.Name & "_config.gpr";
   end GPR_Config_Name;

   function GPR_Deps_Name (Self : Package_Info) return String is
   begin
      return Self.Name & "_deps.gpr";
   end GPR_Deps_Name;

   function GPR_Tests_Deps_Name (Self : Package_Info) return String is
   begin
      return Self.Name & "_tests_deps.gpr";
   end GPR_Tests_Deps_Name;

   function GPR_Tests_Name (Self : Package_Info) return String is
   begin
      return Self.Name & "_tests.gpr";
   end GPR_Tests_Name;
end Tada.Packages;
