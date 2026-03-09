package Tada.Manifests is
   Manifest_Error : exception;

   type Manifest is record
      Sections : Section_Maps.Map;
   end record;

   function Read (Manifest_Path : String) return Manifest;
end Tada.Manifests;
