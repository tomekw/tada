with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Tada.Packages;

package body Tada_Packages_Tests is
   use Ada;
   use Tada;

   package Package_Names_To_Valid is new Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => Boolean,
      Hash => Strings.Hash,
      Equivalent_Keys => "=");

   procedure Test_Validate_Package_Names (T : in out Test_Context) is
      Names : constant Package_Names_To_Valid.Map :=
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
            Package_Name : constant String := Package_Names_To_Valid.Key (C);
            Validity : constant Boolean := Package_Names_To_Valid.Element (C);
         begin
            T.Expect (Packages.Is_Valid_Name (Package_Name) = Validity,
                      "Expected: '" & Package_Name & "' to be: " & Validity'Image);
         end;
      end loop;
   end Test_Validate_Package_Names;
end Tada_Packages_Tests;
