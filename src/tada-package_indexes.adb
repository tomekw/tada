with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.SHA256;

with Tada.Package_Cache;
with Tada.Runners;

package body Tada.Package_Indexes is
   use Ada.Directories;

   function Package_Index_Path return String is
   begin
      return Package_Cache.Cache_Path;
   end Package_Index_Path;

   function Package_Index_File_Path return String is
   begin
      return Compose (Package_Index_Path, "index");
   end Package_Index_File_Path;

   function Package_Index_Tmp_Path return String is
   begin
      return Compose (Package_Index_Path, "tmp");
   end Package_Index_Tmp_Path;

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

      Text_IO.Put_Line ("Fetching package index '" & Package_Index_Url & "'...");
      if not Runners.Run_CURL (Package_Index_Url, Package_Index_File_Path) then
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
         begin
            if Line'Length > 0 then
               declare
                  Slice_Start : Positive := Line'First;
                  Separator : Natural := Strings.Fixed.Index (Line (Slice_Start .. Line'Last), "|");

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
            end if;
         end;
      end loop;

      Text_IO.Close (Index_File);

      return (Packages => Packages_Vector);
   exception
      when others =>
         if Text_IO.Is_Open (Index_File) then
            Text_IO.Close (Index_File);
         end if;

         raise;
   end Read;

   function Find (Index : Package_Index; P : Packages.Package_Info) return Package_Index_Entry is
   begin
      for C in Index.Packages.Iterate loop
         declare
            Package_Entry : constant Package_Index_Entry := Package_Index_Entry_Vectors.Element (C);
         begin
            if Package_Entry.Name.Element = P.Name and then
               Package_Entry.Version.Element = P.Version
            then
               return Package_Entry;
            end if;
         end;
      end loop;

      raise Package_Index_Error with "package '" & P.Name & " " & P.Version & "' not in packages index";
   end Find;

   function Package_Tmp_Path (P : Package_Index_Entry) return String is
   begin
      return Compose (Package_Index_Tmp_Path, P.Name.Element & "-" & P.Version.Element);
   end Package_Tmp_Path;

   function Package_Archive_Path (P : Package_Index_Entry) return String is
   begin
      return Package_Tmp_Path (P) & ".tar.gz";
   end Package_Archive_Path;

   procedure Download (P : Package_Index_Entry) is
   begin
      Text_IO.Put_Line ("Downloading package '" & P.Name.Element & " " & P.Version.Element & "'...");

      begin
         Create_Path (Package_Index_Tmp_Path);
      exception
         when Use_Error =>
            raise Package_Index_Error with "unable to create '" & Package_Tmp_Path (P) & "'";
      end;
      if not Runners.Run_CURL (P.Url.Element, Package_Archive_Path (P)) then
         raise Package_Index_Error with "unable to download package from '" & P.Url.Element & "'";
      end if;
   end Download;

   procedure Verify_Checksum (P : Package_Index_Entry) is
      use Ada.Streams;
      use GNAT;

      File : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. 8192);
      Last : Stream_Element_Offset;
      Ctx : SHA256.Context := SHA256.Initial_Context;
   begin
      Text_IO.Put_Line ("Verifying package '" & P.Name.Element & " " & P.Version.Element & "'...");

      begin
         Stream_IO.Open (File, Stream_IO.In_File, Package_Archive_Path (P));
      exception
         when Stream_IO.Name_Error | Stream_IO.Use_Error =>
            raise Package_Index_Error with "unable to open archive '" & Package_Archive_Path (P) & "'";
      end;

      loop
         Stream_IO.Read (File, Buffer, Last);
         exit when Last < Buffer'First;
         SHA256.Update (Ctx, Buffer (Buffer'First .. Last));
      end loop;

      Stream_IO.Close (File);

      declare
         use Ada.Characters.Latin_1;

         Hex : constant SHA256.Message_Digest := SHA256.Digest (Ctx);
      begin
         if Hex /= P.Checksum.Element then
            raise Package_Index_Error with
              "checksum mismatch for '" & P.Name.Element & " " & P.Version.Element & "'" & LF &
              "expected: '" & P.Checksum.Element & "'" & LF &
              "     got: '" & Hex & "'" & LF &
              "the downloaded tarball may be corrupted or tampered with";
         end if;
      end;
   end Verify_Checksum;

   procedure Extract (P : Package_Index_Entry) is
   begin
      Text_IO.Put_Line ("Extracting package '" & P.Name.Element & " " & P.Version.Element & "'...");

      if not Runners.Run_Tar (Package_Archive_Path (P), Package_Index_Tmp_Path) then
         raise Package_Index_Error with "unable to extract package from '" & Package_Archive_Path (P) & "'";
      end if;
   end Extract;

   procedure Cleanup is
   begin
      if Directories.Exists (Package_Index_Tmp_Path) then
         Delete_Tree (Package_Index_Tmp_Path);
      end if;
   end Cleanup;
end Tada.Package_Indexes;
