with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Tada.Config;

package body Tada_Config_Tests is
   use Ada;
   use Tada;

   overriding
   function Name (Unused_T : Test_Case) return Message_String is
   begin
      return Format ("Tada.Config");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Validate_Package_Names'Access, "Package name is validated");
   end Register_Tests;

   package Package_Names_To_Valid is new Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Boolean,
      Hash => Strings.Hash,
      Equivalent_Keys => "=");

   procedure Test_Validate_Package_Names (Unused_T : in out Test_Cases.Test_Case'Class) is
      Names : constant Package_Names_To_Valid.Map :=
        ["hello" => True,
         "hello_world" => True,
         "a" => True,
         "a1" => True,
         "this_is_so_valid" => True,
         "" => False,
         "1a" => False,
         "_hello" => False,
         "hello_" => False,
         "hello__world" => False,
         "hello-world" => False,
         "hello.world" => False,
         "hello world" => False,
         "package" => False,
         "BEGIN" => False,
         "Type" => False];
   begin
      for C in Names.Iterate loop
         declare
            Package_Name : constant String := Package_Names_To_Valid.Key (C);
            Validity : constant Boolean := Package_Names_To_Valid.Element (C);
         begin
            Assert (Config.Valid_Package_Name (Package_Name) = Validity,
                    "Expected: '" & Package_Name & "' to be: " & Validity'Image);
         end;
      end loop;
   end Test_Validate_Package_Names;
end Tada_Config_Tests;
