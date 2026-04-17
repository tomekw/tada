with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with Tackle.Opts;

with Tada.Commands;

procedure Tada.Main is
   use Ada;
   use Tackle;
   use Tackle.Opts;

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
   Arguments : constant Opts.Argument_List := Opts.Consume_Arguments;
begin
   declare
      Result : constant Opts.Result := Opts.Parse (Arguments, Commands);
   begin
      if Result.Cmd = "" or else
         Result.Has_Flag ("help")
      then
         Opts.Print_Usage (Result.Cmd, "tada", Commands);
      else
         Tada.Commands.Execute (Tada.Commands.Parse (Result));
      end if;
   end;
exception
   when E : Opts.Option_Error =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Text_IO.New_Line (Text_IO.Standard_Error);
      Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada --help' for usage.");
      Command_Line.Set_Exit_Status (Command_Line.Failure);
   when E : Tada.Commands.Parse_Error =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Text_IO.New_Line (Text_IO.Standard_Error);
      Text_IO.Put_Line (Text_IO.Standard_Error, "Run 'tada --help' for usage.");
      Command_Line.Set_Exit_Status (Command_Line.Failure);
   when E : others =>
      Text_IO.Put_Line (Text_IO.Standard_Error, "tada: " & Exceptions.Exception_Message (E));
      Command_Line.Set_Exit_Status (Command_Line.Failure);
end Tada.Main;
