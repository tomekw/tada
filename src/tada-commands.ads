with Tackle.Opts;

package Tada.Commands is
   use Tackle;

   Parse_Error : exception;
   Execute_Error : exception;

   type Command_Kind is (Build,
                         Cache,
                         Config,
                         Clean,
                         Init,
                         Install,
                         Run,
                         Test,
                         Version);

   type Profile_Kind is (Debug,
                         Release);

   type Command (Kind : Command_Kind) is record
      case Kind is
         when Build =>
            Build_Profile : Profile_Kind;
         when Cache =>
            Force : Boolean;
         when Init =>
            Package_Name : String_Holders.Holder;
            Package_Type : Package_Kind;
         when Run =>
            Run_Profile : Profile_Kind;
            Args : Opts.Argument_List;
         when Test =>
            Test_Profile : Profile_Kind;
            Seed : String_Holders.Holder;
         when Clean | Config | Install | Version =>
            null;
      end case;
   end record;

   function Parse (Result : Opts.Result) return Command;

   procedure Execute (Cmd : Command);
end Tada.Commands;
