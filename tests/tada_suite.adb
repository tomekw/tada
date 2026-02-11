with Parse_Project_Name_Tests;

package body Tada_Suite is

   Result : aliased AUnit.Test_Suites.Test_Suite;
   Parse_Name_Tests : aliased Parse_Project_Name_Tests.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
   begin
      AUnit.Test_Suites.Add_Test
        (Result'Access, Parse_Name_Tests'Access);
      return Result'Access;
   end Suite;

end Tada_Suite;
