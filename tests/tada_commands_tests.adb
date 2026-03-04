with Tada.CL_Arguments;
with Tada.Commands;

package body Tada_Commands_Tests is
   use Tada;
   use Tada.CL_Arguments;
   use Tada.Commands;

   procedure Test_Parse_Empty_Args (T : in out Test_Context) is
      Args : constant Argument_List.Vector := Argument_List.Empty_Vector;
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Help), "Expected command: Help");
   end Test_Parse_Empty_Args;

   procedure Test_Parse_Help (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["help"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Help), "Expected command: Help");
   end Test_Parse_Help;

   procedure Test_Parse_Clean (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["clean"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Clean), "Expected command: Clean");
   end Test_Parse_Clean;

   procedure Test_Parse_Version (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["version"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Commands.Version), "Expected command: Version");
   end Test_Parse_Version;

   procedure Parse_Foo is
      Args : constant Argument_List.Vector := ["foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Foo;

   procedure Test_Parse_Foo (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Foo'Access, Parse_Error'Identity, "unknown command 'foo'");
   end Test_Parse_Foo;

   procedure Test_Parse_Build (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["build"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build;

   procedure Test_Parse_Build_Debug (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["build", "--profile", "debug"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build_Debug;

   procedure Test_Parse_Build_Release (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["build", "--profile", "release"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Build, Build_Profile => Release), "Expected command: Build, profile: release");
   end Test_Parse_Build_Release;

   procedure Parse_Build_Profile is
      Args : constant Argument_List.Vector := ["build", "--profile"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Profile;

   procedure Test_Parse_Build_Profile (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Build_Profile'Access, Parse_Error'Identity, "missing profile");
   end Test_Parse_Build_Profile;

   procedure Parse_Build_Unknown is
      Args : constant Argument_List.Vector := ["build", "--profile", "foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Unknown;

   procedure Test_Parse_Build_Unknown (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Build_Unknown'Access, Parse_Error'Identity, "invalid profile 'foo'");
   end Test_Parse_Build_Unknown;

   procedure Parse_Build_Unexpected is
      Args : constant Argument_List.Vector := ["build", "--foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Build_Unexpected;

   procedure Test_Parse_Build_Unexpected (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Build_Unexpected'Access, Parse_Error'Identity, "unexpected option '--foo'");
   end Test_Parse_Build_Unexpected;

   procedure Test_Parse_Run (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["run"];
      Run_Args : constant Argument_List.Vector := Argument_List.Empty_Vector;
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Run,
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

   procedure Test_Parse_Run_No_Args (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Run_No_Args'Access, Parse_Error'Identity, "missing args");
   end Test_Parse_Run_No_Args;

   procedure Test_Parse_Run_Args (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["run", "--", "foo", "bar"];
      Run_Args : constant Argument_List.Vector := ["foo", "bar"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Run,
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

   procedure Test_Parse_Init (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Init'Access, Parse_Error'Identity, "missing package name");
   end Test_Parse_Init;

   procedure Test_Parse_Init_Name (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["init", "hello"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => String_Holders.To_Holder ("hello"),
                                       Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name;

   procedure Test_Parse_Init_Name_Exe (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["init", "hello", "--exe"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => String_Holders.To_Holder ("hello"),
                                       Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name_Exe;

   procedure Test_Parse_Init_Name_Lib (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["init", "hello", "--lib"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => String_Holders.To_Holder ("hello"),
                                       Package_Type => Lib),
              "Expected command: Init, name: hello, kind: lib");
   end Test_Parse_Init_Name_Lib;

   procedure Parse_Init_Name_Foo is
      Args : constant Argument_List.Vector := ["init", "hello", "--foo"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Init_Name_Foo;

   procedure Test_Parse_Init_Name_Foo (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Init_Name_Foo'Access, Parse_Error'Identity, "invalid package type '--foo'");
   end Test_Parse_Init_Name_Foo;

   procedure Test_Parse_Init_Name_Camel (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["init", "cAmEl"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Init,
                                       Package_Name => String_Holders.To_Holder ("camel"),
                                       Package_Type => Exe),
              "Expected command: Init, name: camel, kind: exe");
   end Test_Parse_Init_Name_Camel;

   procedure Parse_Init_Name_Package is
      Args : constant Argument_List.Vector := ["init", "package"];
      Unused_Command : Command;
   begin
      Unused_Command := Commands.Parse (Args);
   end Parse_Init_Name_Package;

   procedure Test_Parse_Init_Name_Package (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Init_Name_Package'Access, Parse_Error'Identity, "invalid package name 'package'");
   end Test_Parse_Init_Name_Package;

   procedure Test_Parse_Cache (T : in out Test_Context) is
      Args : constant Argument_List.Vector := ["cache"];
   begin
      T.Expect (Commands.Parse (Args) = (Kind => Cache), "Expected command: Cache");
   end Test_Parse_Cache;
end Tada_Commands_Tests;
