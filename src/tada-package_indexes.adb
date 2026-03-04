with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

package body Tada.Package_Indexes is
   use Ada.Directories;

   package OS renames GNAT.OS_Lib;

   function Package_Index_Path return String is
      Is_Windows : constant Boolean := OS.Directory_Separator = '\';
   begin
      if Is_Windows then
         return Compose (Compose (Environment_Variables.Value ("LOCALAPPDATA"), "tada"), "packages-index");
      else
         return Compose (Compose (Compose (Environment_Variables.Value ("HOME"), ".cache"), "tada"), "packages-index");
      end if;
   end Package_Index_Path;

   function Package_Index_File_Path return String is
   begin
      return Compose (Package_Index_Path, "index");
   end Package_Index_File_Path;

   function Run_CURL (Source : String; Target : String) return Boolean is
      use type OS.String_Access;

      CURL_Path : OS.String_Access := OS.Locate_Exec_On_Path ("curl");
      Args : OS.Argument_List (1 .. 4) :=
        [new String'("-fsSL"),
          new String'("-o"),
          new String'(Target),
          new String'(Source)];
      Result : Boolean := False;
   begin
      if CURL_Path /= null then
         OS.Spawn (CURL_Path.all, Args, Result);
      end if;

      OS.Free (CURL_Path);
      for Arg of Args loop
         OS.Free (Arg);
      end loop;

      return Result;
   exception
      when others =>
         OS.Free (CURL_Path);
         for Arg of Args loop
            OS.Free (Arg);
         end loop;

         return False;
   end Run_CURL;

   function Read (Package_Index_Url : String) return Package_Index is
      Index_File : Text_IO.File_Type;
      Packages_Vector : Package_Index_Entry_Vectors.Vector;
   begin
      begin
         Create_Path (Package_Index_Path);
      exception
         when Use_Error =>
            raise Package_Index_Error with "unable to create '" & Package_Index_Path & "'";
      end;

      if not Run_CURL (Package_Index_Url, Package_Index_File_Path) then
         raise Package_Index_Error with "unable to fetch index '" & Package_Index_Url & "'";
      end if;

      begin
         Text_IO.Open (Index_File, Text_IO.In_File, Package_Index_File_Path);
      exception
         when Text_IO.Name_Error =>
            raise Package_Index_Error with "index file '" & Package_Index_File_Path & "' not found";
      end;

      while not Text_IO.End_Of_File (Index_File) loop
         declare
            Line : constant String := Strings.Fixed.Trim (Text_IO.Get_Line (Index_File), Strings.Both);
            Slice_Start : Positive := Line'First;
            Separator : Positive := Strings.Fixed.Index (Line (Slice_Start .. Line'Last), "|");

            Name_Holder : String_Holders.Holder;
            Version_Holder : String_Holders.Holder;
            Url_Holder : String_Holders.Holder;
            Checksum_Holder : String_Holders.Holder;
         begin
            Name_Holder := String_Holders.To_Holder (Line (Slice_Start .. Separator - 1));
            Slice_Start := Separator + 1;

            Separator := Strings.Fixed.Index (Line (Slice_Start .. Line'Last), "|");
            Version_Holder := String_Holders.To_Holder (Line (Slice_Start .. Separator - 1));
            Slice_Start := Separator + 1;

            Separator := Strings.Fixed.Index (Line (Slice_Start .. Line'Last), "|");
            Url_Holder := String_Holders.To_Holder (Line (Slice_Start .. Separator - 1));
            Slice_Start := Separator + 1;

            Checksum_Holder := String_Holders.To_Holder (Line (Slice_Start .. Line'Last));

            Packages_Vector.Append (Package_Index_Entry'(Name => Name_Holder,
                                                         Version => Version_Holder,
                                                         Url => Url_Holder,
                                                         Checksum => Checksum_Holder));
         end;
      end loop;

      return (Packages => Packages_Vector);
   end Read;
end Tada.Package_Indexes;
