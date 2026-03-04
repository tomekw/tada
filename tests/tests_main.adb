with Testy.Runners;
with Testy.Reporters.Text;

with Tada_Commands_Tests;
with Tada_Packages_Tests;

procedure Tests_Main is
   use Testy;

   Test_Runner : Runners.Runner := Runners.Create;
   Test_Reporter : Reporters.Text.Text_Reporter;
begin
   Test_Runner.Add ("No args parses to Help", Tada_Commands_Tests.Test_Parse_Empty_Args'Access);
   Test_Runner.Add ("help parses to Help", Tada_Commands_Tests.Test_Parse_Help'Access);
   Test_Runner.Add ("clean parses to Clean", Tada_Commands_Tests.Test_Parse_Clean'Access);
   Test_Runner.Add ("version parses to Version", Tada_Commands_Tests.Test_Parse_Version'Access);
   Test_Runner.Add ("foo raises Parse_Error", Tada_Commands_Tests.Test_Parse_Foo'Access);
   Test_Runner.Add ("build parses to Build, profile: debug", Tada_Commands_Tests.Test_Parse_Build'Access);
   Test_Runner.Add ("build --profile debug parses to Build, profile: debug", Tada_Commands_Tests.Test_Parse_Build_Debug'Access);
   Test_Runner.Add ("build --profile release parses to Build, profile: release", Tada_Commands_Tests.Test_Parse_Build_Release'Access);
   Test_Runner.Add ("build --profile raises Parse_Error", Tada_Commands_Tests.Test_Parse_Build_Profile'Access);
   Test_Runner.Add ("build --profile foo raises Parse_Error", Tada_Commands_Tests.Test_Parse_Build_Unknown'Access);
   Test_Runner.Add ("build --foo raises Parse_Error", Tada_Commands_Tests.Test_Parse_Build_Unexpected'Access);
   Test_Runner.Add ("run parses to Run, profile: debug, no run args", Tada_Commands_Tests.Test_Parse_Run'Access);
   Test_Runner.Add ("run -- raises Parse_Error", Tada_Commands_Tests.Test_Parse_Run_No_Args'Access);
   Test_Runner.Add ("run -- foo bar parses to Run, profile: debug, run args", Tada_Commands_Tests.Test_Parse_Run_Args'Access);
   Test_Runner.Add ("init raises Parse_Error", Tada_Commands_Tests.Test_Parse_Init'Access);
   Test_Runner.Add ("init hello parses to Init, kind: exe", Tada_Commands_Tests.Test_Parse_Init_Name'Access);
   Test_Runner.Add ("init hello --exe parses to Init, kind: exe", Tada_Commands_Tests.Test_Parse_Init_Name_Exe'Access);
   Test_Runner.Add ("init hello --lib parses to Init, kind: lib", Tada_Commands_Tests.Test_Parse_Init_Name_Lib'Access);
   Test_Runner.Add ("init hello --foo raises Parse_Error", Tada_Commands_Tests.Test_Parse_Init_Name_Foo'Access);
   Test_Runner.Add ("init cAmEl parses to Init, kind: exe", Tada_Commands_Tests.Test_Parse_Init_Name_Camel'Access);
   Test_Runner.Add ("init package raises Parse_Error", Tada_Commands_Tests.Test_Parse_Init_Name_Package'Access);
   Test_Runner.Add ("cache parses to Cache", Tada_Commands_Tests.Test_Parse_Cache'Access);

   Test_Runner.Add ("Package name is validated", Tada_Packages_Tests.Test_Validate_Package_Names'Access);

   Test_Runner.Run (Test_Reporter);
end Tests_Main;
