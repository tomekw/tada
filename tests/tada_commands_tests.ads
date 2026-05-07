with Testy.Tests;

package Tada_Commands_Tests is
   use Testy.Tests;

   procedure Test_Parse_Clean (T : in out Test_Context);

   procedure Test_Parse_Version (T : in out Test_Context);

   procedure Test_Parse_Doc (T : in out Test_Context);

   procedure Test_Parse_Build (T : in out Test_Context);

   procedure Test_Parse_Build_Debug (T : in out Test_Context);

   procedure Test_Parse_Build_Release (T : in out Test_Context);

   procedure Test_Parse_Build_Unknown (T : in out Test_Context);

   procedure Test_Parse_Run (T : in out Test_Context);

   procedure Test_Parse_Run_Args (T : in out Test_Context);

   procedure Test_Parse_Test (T : in out Test_Context);

   procedure Test_Parse_Test_Seed (T : in out Test_Context);

   procedure Test_Parse_Init (T : in out Test_Context);

   procedure Test_Parse_Init_Name (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Exe (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Lib (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Camel (T : in out Test_Context);

   procedure Test_Parse_Init_Name_Package (T : in out Test_Context);

   procedure Test_Parse_Install (T : in out Test_Context);

   procedure Test_Parse_Cache (T : in out Test_Context);

   procedure Test_Parse_Cache_Force (T : in out Test_Context);
end Tada_Commands_Tests;
