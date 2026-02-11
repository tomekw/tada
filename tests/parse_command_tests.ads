with AUnit.Test_Cases;

package Parse_Command_Tests is

   type Test_Case is new AUnit.Test_Cases.Test_Case
     with null record;

   overriding
   function Name
     (Unused_T : Test_Case)
      return AUnit.Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Empty_Is_Help
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Help_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Clean_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Unknown_Command
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Command_Case_Insensitive
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Test_Init_Missing_Name
     (Unused_T : in out AUnit.Test_Cases.Test_Case'Class);

end Parse_Command_Tests;
