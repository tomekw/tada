with Testy.Tests;

package Tada_Commands_Tests is
   use Testy.Tests;

   procedure Test_Parse_Empty_Args (T : in out Test_Context);

   procedure Test_Parse_Help (T : in out Test_Context);

   procedure Test_Parse_Clean (T : in out Test_Context);

   procedure Test_Parse_Version (T : in out Test_Context);

   procedure Test_Parse_Foo (T : in out Test_Context);

   procedure Test_Parse_Build (T : in out Test_Context);

   procedure Test_Parse_Build_Debug (T : in out Test_Context);

   procedure Test_Parse_Build_Release (T : in out Test_Context);

   procedure Test_Parse_Build_Profile (T : in out Test_Context);

   procedure Test_Parse_Build_Unknown (T : in out Test_Context);

   procedure Test_Parse_Build_Unexpected (T : in out Test_Context);

   procedure Test_Parse_Run (T : in out Test_Context);

   procedure Test_Parse_Run_No_Args (T : in out Test_Context);

   procedure Test_Parse_Run_Args (T : in out Test_Context);

   procedure Test_Parse_Init (T : in out Test_Context);

   procedure Test_Parse_Init_Name (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Exe (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Lib (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Foo (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Camel (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Package (T : in out Test_Context);

   procedure Test_Parse_Cache (T : in out Test_Context);
end Tada_Commands_Tests;
