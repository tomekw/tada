with AUnit.Test_Cases;

package Tada_Commands_Tests is
   use AUnit;

   type Test_Case is new Test_Cases.Test_Case with null record;

   overriding
   function Name (Unused_T : Test_Case) return Message_String;

   overriding
   procedure Register_Tests (T : in out Test_Case);

private

   procedure Test_Parse_Empty_Args (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Help (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Clean (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Version (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Foo (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build_Debug (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build_Release (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build_Profile (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build_Unknown (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Build_Unexpected (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Run (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Run_No_Args (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Run_Args (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name_Exe (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name_Lib (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name_Foo (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name_Camel (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Init_Name_Package (Unused_T : in out Test_Cases.Test_Case'Class);

   procedure Test_Parse_Cache (Unused_T : in out Test_Cases.Test_Case'Class);
end Tada_Commands_Tests;
