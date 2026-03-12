with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO;

with Tada.Environments;
with Tada.Manifests;

package body Tada.Package_Cache is
   use Ada.Directories;

   function Cache_Path return String is
   begin
      if Environments.Is_Windows then
         return Compose (Environment_Variables.Value ("LOCALAPPDATA"), "tada");
      else
         return Compose (Compose (Environment_Variables.Value ("HOME"), ".cache"), "tada");
      end if;
   end Cache_Path;

   function Root_Path return String is
   begin
      return Compose (Cache_Path, "package");
   end Root_Path;

   function Package_Path (P : Package_Info) return String is
   begin
      return Compose (Compose (Root_Path, P.Name), P.Version);
   end Package_Path;

   function Manifest_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), Packages.Manifest_Name);
   end Manifest_Path;

   function GPR_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Name);
   end GPR_Path;

   function GPR_Config_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Config_Name);
   end GPR_Config_Path;

   function GPR_Deps_Path (P : Package_Info) return String is
   begin
      return Compose (Package_Path (P), P.GPR_Deps_Name);
   end GPR_Deps_Path;

   function Is_Cached (P : Package_Info) return Boolean is
   begin
      return Exists (Manifest_Path (P));
   end Is_Cached;

   procedure Copy_Tree (Tada_Package : Packages.Package_Info; Source_Path : String; Target_Path : String) is
      Tree_Search : Search_Type;
      Search_Item : Directory_Entry_Type;
      Search_Filter : constant Filter_Type := [Ordinary_File => True,
                                               Directory => True,
                                               Special_File => False];
   begin
      Create_Path (Target_Path);
      Start_Search (Tree_Search, Source_Path, "", Search_Filter);

      while More_Entries (Tree_Search) loop
         Get_Next_Entry (Tree_Search, Search_Item);

         declare
            Entry_Name : constant String := Simple_Name (Search_Item);
         begin
            if Kind (Search_Item) = Ordinary_File and then
               Entry_Name /= Tada_Package.GPR_Deps_Name and then
               Entry_Name /= Tada_Package.GPR_Tests_Deps_Name and then
               Entry_Name /= Tada_Package.GPR_Tests_Name
            then
               Copy_File (Full_Name (Search_Item), Compose (Target_Path, Entry_Name));
            elsif Kind (Search_Item) = Directory and then
                  Entry_Name /= "." and then
                  Entry_Name /= ".." and then
                  Entry_Name /= ".git" and then
                  Entry_Name /= "target" and then
                  Entry_Name /= ".tada" and then
                  Entry_Name /= "tests"
            then
               Copy_Tree (Tada_Package, Compose (Source_Path, Entry_Name), Compose (Target_Path, Entry_Name));
            end if;
         end;
      end loop;

      End_Search (Tree_Search);
   end Copy_Tree;

   procedure Cache_Package (Package_Tmp_Path : String) is
      Tada_Manifest : constant Manifests.Manifest := Manifests.Read (Compose (Package_Tmp_Path, Packages.Manifest_Name));
      Tada_Package : constant Packages.Package_Info :=
        Packages.Create (Tada_Manifest.Sections ("package") ("name"),
                         Tada_Manifest.Sections ("package") ("version"));
      Tada_Package_Cache_Path : constant String := Package_Path (Tada_Package);
   begin
      if Is_Cached (Tada_Package) then
         Text_IO.Put_Line ("Package '" & Tada_Package.Name & " " & Tada_Package.Version &
                           "' already installed at '" & Tada_Package_Cache_Path & "'. Skipping.");
         return;
      end if;

      begin
         Copy_Tree (Tada_Package, Package_Tmp_Path, Tada_Package_Cache_Path);

         Text_IO.Put_Line ("Installed package '" & Tada_Package.Name & " " & Tada_Package.Version &
                           "' at '" & Tada_Package_Cache_Path & "'");
      exception
         when E : others =>
            Delete_Tree (Tada_Package_Cache_Path);

            raise Package_Cache_Error with Exceptions.Exception_Message (E);
      end;
   end Cache_Package;
end Tada.Package_Cache;
