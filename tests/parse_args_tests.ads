with AUnit.Test_Cases;

package Parse_Args_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Run_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_With_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Separator_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Single_Arg
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Profile_And_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Profile_Release_And_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Profile_No_Separator
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Run_Profile_Separator_No_Args
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Args_Tests;
