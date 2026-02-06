with AUnit.Test_Cases;

package Tada_Tests is

   type Tada_Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Tada_Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Tada_Test_Case);

private

   procedure Test_True_Is_True
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Tada_Tests;
