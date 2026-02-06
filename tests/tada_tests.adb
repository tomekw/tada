with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package body Tada_Tests is

   overriding
   function Name
     (Unused_T : Tada_Test_Case)
      return AUnit.Message_String
   is
   begin
      return AUnit.Format ("Tada");
   end Name;

   overriding
   procedure Register_Tests (T : in out Tada_Test_Case) is
   begin
      Registration.Register_Routine
        (T, Test_True_Is_True'Access, "True is True");
   end Register_Tests;

   procedure Test_True_Is_True
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class)
   is
   begin
      Assert (True, "Expected True to be True");
   end Test_True_Is_True;

end Tada_Tests;
