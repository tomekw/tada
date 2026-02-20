with AUnit.Test_Cases;

package Tada_Config_Tests is
   use AUnit;

   type Test_Case is new Test_Cases.Test_Case with null record;

   overriding
   function Name (Unused_T : Test_Case) return Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Validate_Package_Names (Unused_T : in out Test_Cases.Test_Case'Class);
end Tada_Config_Tests;
