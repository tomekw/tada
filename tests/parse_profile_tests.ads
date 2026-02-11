with AUnit.Test_Cases;

package Parse_Profile_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Build_Default_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Explicit_Debug
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Release
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Profile_Case_Insensitive
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Missing_Profile_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Invalid_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Build_Unexpected_Option
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Test_Default_Profile
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Test_Release
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Test_Missing_Profile_Value
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Test_Unexpected_Option
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Profile_Tests;
