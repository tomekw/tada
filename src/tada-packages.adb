with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

package body Tada.Packages is
   use Ada;

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
