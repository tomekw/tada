with GNAT.OS_Lib;

with Tada.CL_Arguments;

package body Tada.Runners is
   package OS renames GNAT.OS_Lib;

   function Spawn (Executable : String; Arguments : CL_Arguments.Argument_List.Vector) return Boolean is
      use type OS.String_Access;

      Arguments_Count : constant Natural := Natural (Arguments.Length);
      Args : OS.Argument_List (1 .. Arguments_Count);

      Executable_Path : OS.String_Access := OS.Locate_Exec_On_Path (Executable);

      Result : Boolean := False;
   begin
      for I in 1 .. Arguments_Count loop
         Args (I) := new String'(Arguments (I));
      end loop;

      if Executable_Path /= null then
         OS.Spawn (Executable_Path.all, Args, Result);
      end if;

      OS.Free (Executable_Path);
      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   exception
      when others =>
         OS.Free (Executable_Path);
         for Arg of Args loop
            OS.Free (Arg);
         end loop;

         return False;
   end Spawn;

   function Run_CURL (Source : String; Target : String) return Boolean is
   begin
      return Spawn ("curl", ["-fsSL", "-o", Target, Source]);
   end Run_CURL;

   function Run_GPRBuild (Project : String; Profile : String) return Boolean is
   begin
      return Spawn ("gprbuild", ["-P", Project & ".gpr", "-XBUILD_PROFILE=" & Profile, "-p"]);
   end Run_GPRBuild;

   function Run_Tar (Source : String; Target : String) return Boolean is
   begin
      return Spawn ("tar", ["xzf", Source, "-C", Target]);
   end Run_Tar;
end Tada.Runners;
