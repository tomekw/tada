with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Tada.Packages;

package body Tada_Packages_Tests is
   use Ada;
   use Tada;

   package Strings_To_Valid is new Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Boolean,
      Hash => Strings.Hash,
      Equivalent_Keys => "=");

   procedure Test_Validate_Package_Names (T : in out Test_Context) is
      Names : constant Strings_To_Valid.Map :=
        ["hello" => True,
         "Hello" => False,
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
            Package_Name : constant String := Strings_To_Valid.Key (C);
            Validity : constant Boolean := Strings_To_Valid.Element (C);
         begin
            T.Expect (Packages.Is_Valid_Name (Package_Name) = Validity,
                      "Expected: '" & Package_Name & "' to be: " & Validity'Image);
         end;
      end loop;
   end Test_Validate_Package_Names;

   procedure Test_Validate_Package_Versions (T : in out Test_Context) is
      Versions : constant Strings_To_Valid.Map :=
        ["" => False,
         "abc" => False,
         "..." => False,
         "1" => False,
         "1.1" => False,
         "1.1.1" => True,
         "0.0.0" => True,
         "01.0.0" => False,
         "0.01.0" => False,
         "0.0.01" => False,
         "1.1.1.1" => False,
         "v1.0.0" => False,
         "1.0.a" => False,
         "1.0.0a" => False,
         "1.1.1-dev" => True,
         "1.0.0-rc2" => True,
         "1.0.0-beta_1" => True,
         "1.0.0-123" => True,
         "1.0.0-" => False,
         "1.0.0-a-b" => False,
         "1.1-foo" => False];
   begin
      for C in Versions.Iterate loop
         declare
            Package_Version : constant String := Strings_To_Valid.Key (C);
            Validity : constant Boolean := Strings_To_Valid.Element (C);
         begin
            T.Expect (Packages.Is_Valid_Version (Package_Version) = Validity,
                      "Expected: '" & Package_Version & "' to be: " & Validity'Image);
         end;
      end loop;
   end Test_Validate_Package_Versions;
end Tada_Packages_Tests;
