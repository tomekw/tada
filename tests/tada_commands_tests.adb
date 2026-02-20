with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Strings.Unbounded;

with Tada.CL_Arguments;
with Tada.Commands;

package body Tada_Commands_Tests is
   use Ada.Strings.Unbounded;

   use Tada;
   use Tada.CL_Arguments;
   use Tada.Commands;

   overriding
   function Name (Unused_T : Test_Case) return Message_String is
   begin
      return Format ("Tada.Commands");
   end Name;

   overriding
   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Parse_Empty_Args'Access, "No args parses to Help");
      Registration.Register_Routine (T, Test_Parse_Help'Access, "help parses to Help");
      Registration.Register_Routine (T, Test_Parse_Clean'Access, "clean parses to Clean");
      Registration.Register_Routine (T, Test_Parse_Version'Access, "version parses to Version");
      Registration.Register_Routine (T, Test_Parse_Foo'Access, "foo raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Build'Access, "build parses to Build, profile: debug");
      Registration.Register_Routine (T, Test_Parse_Build_Debug'Access, "build --profile debug parses to Build, profile: debug");
      Registration.Register_Routine (T, Test_Parse_Build_Release'Access, "build --profile release parses to Build, profile: release");
      Registration.Register_Routine (T, Test_Parse_Build_Profile'Access, "build --profile raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Build_Unknown'Access, "build --profile foo raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Build_Unexpected'Access, "build --foo raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Run'Access, "run parses to Run, profile: debug, no run args");
      Registration.Register_Routine (T, Test_Parse_Run_No_Args'Access, "run -- raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Run_Args'Access, "run parses to Run, profile: debug, run args");
      Registration.Register_Routine (T, Test_Parse_Init'Access, "init raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Init_Name'Access, "init hello parses to Init, kind: exe");
      Registration.Register_Routine (T, Test_Parse_Init_Name_Exe'Access, "init hello --exe parses to Init, kind: exe");
      Registration.Register_Routine (T, Test_Parse_Init_Name_Lib'Access, "init hello --lib parses to Init, kind: lib");
      Registration.Register_Routine (T, Test_Parse_Init_Name_Foo'Access, "init hello --foo raises Parse_Error");
      Registration.Register_Routine (T, Test_Parse_Init_Name_Camel'Access, "init cAmEl parses to Init, kind: exe");
      Registration.Register_Routine (T, Test_Parse_Init_Name_Package'Access, "init package raises Parse_Error");
   end Register_Tests;

   procedure Test_Parse_Empty_Args (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := Argument_List.Empty_Vector;
   begin
      Assert (Commands.Parse (Args) = (Kind => Help), "Expected command: Help");
   end Test_Parse_Empty_Args;

   procedure Test_Parse_Help (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["help"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Help), "Expected command: Help");
   end Test_Parse_Help;

   procedure Test_Parse_Clean (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["clean"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Clean), "Expected command: Clean");
   end Test_Parse_Clean;

   procedure Test_Parse_Version (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["version"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Commands.Version), "Expected command: Version");
   end Test_Parse_Version;

   procedure Parse_Foo is
      Args : constant Argument_List.Vector := ["foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Foo;

   procedure Test_Parse_Foo (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Foo'Access, "Expected: Parse_Error");
   end Test_Parse_Foo;

   procedure Test_Parse_Build (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["build"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build;

   procedure Test_Parse_Build_Debug (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["build", "--profile", "debug"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build_Debug;

   procedure Test_Parse_Build_Release (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["build", "--profile", "release"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Build, Build_Profile => Release), "Expected command: Build, profile: release");
   end Test_Parse_Build_Release;

   procedure Parse_Build_Profile is
      Args : constant Argument_List.Vector := ["build", "--profile"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Profile;

   procedure Test_Parse_Build_Profile (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Build_Profile'Access, "Expected: Parse_Error");
   end Test_Parse_Build_Profile;

   procedure Parse_Build_Unknown is
      Args : constant Argument_List.Vector := ["build", "--profile", "foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Unknown;

   procedure Test_Parse_Build_Unknown (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Build_Unknown'Access, "Expected: Parse_Error");
   end Test_Parse_Build_Unknown;

   procedure Parse_Build_Unexpected is
      Args : constant Argument_List.Vector := ["build", "--foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Unexpected;

   procedure Test_Parse_Build_Unexpected (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Build_Unexpected'Access, "Expected: Parse_Error");
   end Test_Parse_Build_Unexpected;

   procedure Test_Parse_Run (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["run"];
      Run_Args : constant Argument_List.Vector := Argument_List.Empty_Vector;
   begin
      Assert (Commands.Parse (Args) = (Kind => Run,
                                       Run_Profile => Debug,
                                       Args => Run_Args),
              "Expected command: Run, profile: debug, run args: empty");
   end Test_Parse_Run;

   procedure Parse_Run_No_Args is
      Args : constant Argument_List.Vector := ["run", "--"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Run_No_Args;

   procedure Test_Parse_Run_No_Args (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Run_No_Args'Access, "Expected: Parse_Error");
   end Test_Parse_Run_No_Args;

   procedure Test_Parse_Run_Args (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["run", "--", "foo", "bar"];
      Run_Args : constant Argument_List.Vector := ["foo", "bar"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Run,
                                       Run_Profile => Debug,
                                       Args => Run_Args),
              "Expected command: Run, profile: debug, run args: foo, bar");
   end Test_Parse_Run_Args;

   procedure Parse_Init is
      Args : constant Argument_List.Vector := ["init"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Init;

   procedure Test_Parse_Init (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Init'Access, "Expected: Parse_Error");
   end Test_Parse_Init;

   procedure Test_Parse_Init_Name (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["init", "hello"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => To_Unbounded_String ("hello"),
                                       Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name;

   procedure Test_Parse_Init_Name_Exe (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["init", "hello", "--exe"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => To_Unbounded_String ("hello"),
                                       Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name_Exe;

   procedure Test_Parse_Init_Name_Lib (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["init", "hello", "--lib"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => To_Unbounded_String ("hello"),
                                       Package_Type => Lib),
              "Expected command: Init, name: hello, kind: lib");
   end Test_Parse_Init_Name_Lib;

   procedure Parse_Init_Name_Foo is
      Args : constant Argument_List.Vector := ["init", "hello", "--foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Init_Name_Foo;

   procedure Test_Parse_Init_Name_Foo (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Init_Name_Foo'Access, "Expected: Parse_Error");
   end Test_Parse_Init_Name_Foo;

   procedure Test_Parse_Init_Name_Camel (Unused_T : in out Test_Cases.Test_Case'Class) is
      Args : constant Argument_List.Vector := ["init", "cAmEl"];
   begin
      Assert (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => To_Unbounded_String ("camel"),
                                       Package_Type => Exe),
              "Expected command: Init, name: camel, kind: exe");
   end Test_Parse_Init_Name_Camel;

   procedure Parse_Init_Name_Package is
      Args : constant Argument_List.Vector := ["init", "package"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Init_Name_Package;

   procedure Test_Parse_Init_Name_Package (Unused_T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Exception (Parse_Init_Name_Package'Access, "Expected: Parse_Error");
   end Test_Parse_Init_Name_Package;
end Tada_Commands_Tests;
