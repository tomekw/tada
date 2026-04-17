with Tackle.Opts;
with Tada.Commands;

package body Tada_Commands_Tests is
   use Tackle;
   use Tackle.Opts;
   use Tada;
   use Tada.Commands;

   Commands : constant Command_List := [Cmd ("build", "Compile the package",
                                             [Arg ("profile", 'p', "Build profile, 'debug' or 'release'")]),
                                        Cmd ("cache", "Install the package to the local cache",
                                             [Flag ("force", 'f', "Overwrite cache")]),
                                        Cmd ("clean", "Remove build artifacts",
                                             []),
                                        Cmd ("config", "Display configuration",
                                             []),
                                        Cmd ("init", "Create a new package",
                                             [Arg ("name", 'n', "Package name"),
                                              Arg ("type", 't', "Package type, 'exe' or 'lib'")]),
                                        Cmd ("install", "Install dependencies",
                                             []),
                                        Cmd ("run", "Build and run the executable",
                                             [Arg ("profile", 'p', "Run profile, 'debug' or 'release'")],
                                             Passthrough => True),
                                        Cmd ("test", "Build and run the tests",
                                             [Arg ("profile", 'p', "Test profile, 'debug' or 'release'")]),
                                        Cmd ("version", "Display version",
                                             [])];

   procedure Test_Parse_Clean (T : in out Test_Context) is
      Arguments : constant Argument_List := ["clean"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Clean), "Expected command: Clean");
   end Test_Parse_Clean;

   procedure Test_Parse_Version (T : in out Test_Context) is
      Arguments : constant Argument_List := ["version"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Tada.Commands.Version), "Expected command: Version");
   end Test_Parse_Version;

   procedure Test_Parse_Build (T : in out Test_Context) is
      Arguments : constant Argument_List := ["build"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build;

   procedure Test_Parse_Build_Debug (T : in out Test_Context) is
      Arguments : constant Argument_List := ["build", "--profile", "debug"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Build, Build_Profile => Debug), "Expected command: Build, profile: debug");
   end Test_Parse_Build_Debug;

   procedure Test_Parse_Build_Release (T : in out Test_Context) is
      Arguments : constant Argument_List := ["build", "--profile", "release"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Build, Build_Profile => Release), "Expected command: Build, profile: release");
   end Test_Parse_Build_Release;

   procedure Parse_Build_Unknown is
      Arguments : constant Argument_List := ["build", "--profile", "foo"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      declare
         Unused_Command : Tada.Commands.Command := Tada.Commands.Parse (Result);
      begin
         null;
      end;
   end Parse_Build_Unknown;

   procedure Test_Parse_Build_Unknown (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Build_Unknown'Access, Parse_Error'Identity, "invalid profile 'foo'");
   end Test_Parse_Build_Unknown;

   procedure Test_Parse_Run (T : in out Test_Context) is
      Arguments : constant Argument_List := ["run"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
      Run_Args : constant Argument_List := [];
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Run,
                                                Run_Profile => Debug,
                                                Args => Run_Args),
              "Expected command: Run, profile: debug, run args: empty");
   end Test_Parse_Run;

   procedure Test_Parse_Run_Args (T : in out Test_Context) is
      Arguments : constant Argument_List := ["run", "--", "foo", "bar"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
      Run_Args : constant Argument_List := ["foo", "bar"];
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Run,
                                                Run_Profile => Debug,
                                                Args => Run_Args),
              "Expected command: Run, profile: debug, run args: foo, bar");
   end Test_Parse_Run_Args;

   procedure Parse_Init is
      Arguments : constant Argument_List := ["init"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      declare
         Unused_Command : Tada.Commands.Command := Tada.Commands.Parse (Result);
      begin
         null;
      end;
   end Parse_Init;

   procedure Test_Parse_Init (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Init'Access, Parse_Error'Identity, "invalid package name ''");
   end Test_Parse_Init;

   procedure Test_Parse_Init_Name (T : in out Test_Context) is
      Arguments : constant Argument_List := ["init", "--name", "hello"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Init,
                                                Package_Name => Tada.String_Holders.To_Holder ("hello"),
                                                Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name;

   procedure Test_Parse_Init_Name_Exe (T : in out Test_Context) is
      Arguments : constant Argument_List := ["init", "--name", "hello", "--type", "exe"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Init,
                                                Package_Name => Tada.String_Holders.To_Holder ("hello"),
                                                Package_Type => Exe),
              "Expected command: Init, name: hello, kind: exe");
   end Test_Parse_Init_Name_Exe;

   procedure Test_Parse_Init_Name_Lib (T : in out Test_Context) is
      Arguments : constant Argument_List := ["init", "--name", "hello", "--type", "lib"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Init,
                                                Package_Name => Tada.String_Holders.To_Holder ("hello"),
                                                Package_Type => Lib),
              "Expected command: Init, name: hello, kind: lib");
   end Test_Parse_Init_Name_Lib;

   procedure Test_Parse_Init_Name_Camel (T : in out Test_Context) is
      Arguments : constant Argument_List := ["init", "--name", "cAmEl"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Init,
                                                Package_Name => Tada.String_Holders.To_Holder ("camel"),
                                                Package_Type => Exe),
              "Expected command: Init, name: camel, kind: exe");
   end Test_Parse_Init_Name_Camel;

   procedure Parse_Init_Name_Package is
      Arguments : constant Argument_List := ["init", "--name", "package"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      declare
         Unused_Command : Tada.Commands.Command := Tada.Commands.Parse (Result);
      begin
         null;
      end;
   end Parse_Init_Name_Package;

   procedure Test_Parse_Init_Name_Package (T : in out Test_Context) is
   begin
      T.Expect_Raises (Parse_Init_Name_Package'Access, Parse_Error'Identity, "invalid package name 'package'");
   end Test_Parse_Init_Name_Package;

   procedure Test_Parse_Install (T : in out Test_Context) is
      Arguments : constant Argument_List := ["install"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Install), "Expected command: Install");
   end Test_Parse_Install;

   procedure Test_Parse_Cache (T : in out Test_Context) is
      Arguments : constant Argument_List := ["cache"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Cache, Force => False), "Expected command: Cache, Force: False");
   end Test_Parse_Cache;

   procedure Test_Parse_Cache_Force (T : in out Test_Context) is
      Arguments : constant Argument_List := ["cache", "--force"];
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      T.Expect (Tada.Commands.Parse (Result) = (Kind => Cache, Force => True), "Expected command: Cache, Force: True");
   end Test_Parse_Cache_Force;
end Tada_Commands_Tests;
