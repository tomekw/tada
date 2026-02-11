with AUnit.Test_Cases;

package Parse_Project_Type_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Default_Type
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Explicit_Exe
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Explicit_Lib
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Invalid_Type
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Project_Type_Tests;
