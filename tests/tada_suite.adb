with Parse_Args_Tests;
with Parse_Command_Tests;
with Parse_Manifest_Tests;
with Parse_Profile_Tests;
with Parse_Project_Name_Tests;
with Parse_Project_Type_Tests;
with Valid_Project_Name_Tests;

package body Tada_Suite is

   Result : aliased AUnit.Test_Suites.Test_Suite;
   Parse_Name_Tests : aliased Parse_Project_Name_Tests.Test_Case;
   Profile_Tests : aliased Parse_Profile_Tests.Test_Case;
   Project_Type_Tests : aliased
     Parse_Project_Type_Tests.Test_Case;
   Args_Tests : aliased Parse_Args_Tests.Test_Case;
   Command_Tests : aliased Parse_Command_Tests.Test_Case;
   Manifest_Tests : aliased Parse_Manifest_Tests.Test_Case;
   Name_Validation_Tests : aliased
     Valid_Project_Name_Tests.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
   begin
      AUnit.Test_Suites.Add_Test
        (Result'Access, Parse_Name_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Profile_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Project_Type_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Args_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Command_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Manifest_Tests'Access);
      AUnit.Test_Suites.Add_Test
        (Result'Access, Name_Validation_Tests'Access);
      return Result'Access;
   end Suite;

end Tada_Suite;
