with Tada_Tests;

package body Tada_Suite is

   Result : aliased AUnit.Test_Suites.Test_Suite;
   Tada_Test : aliased Tada_Tests.Tada_Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
   begin
      AUnit.Test_Suites.Add_Test
        (Result'Access, Tada_Test'Access);
      return Result'Access;
   end Suite;

end Tada_Suite;
