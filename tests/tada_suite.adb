with Tada_Commands_Tests;
with Tada_Config_Tests;

package body Tada_Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Tada_Commands : aliased Tada_Commands_Tests.Test_Case;
   Tada_Config : aliased Tada_Config_Tests.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Tada_Commands'Access);
      AUnit.Test_Suites.Add_Test (Result'Access, Tada_Config'Access);

      return Result'Access;
   end Suite;
end Tada_Suite;
