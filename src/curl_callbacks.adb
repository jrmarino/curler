with Ada.Unchecked_Conversion;
with curl_header;

package body curl_callbacks is

   package STM renames Ada.Streams;

   function write_file (ptr      : IC.Strings.chars_ptr;
                        size     : IC.size_t;
                        nmemb    : IC.size_t;
                        userdata : System.Address) return IC.size_t
   is
      bytes_passed : constant Natural := Natural (nmemb);
      zdata : curldata;
      for zdata'Address use userdata;
      pragma Import (Ada, zdata);
   begin
      if bytes_passed = 0 then
         return IC.size_t (0);
      end if;

      declare
         subtype cdatatype is IC.char_array (1 .. nmemb);
         subtype adatatype is STM.Stream_Element_Array (1 .. STM.Stream_Element_Offset (nmemb));

         function char_to_stream is new Ada.Unchecked_Conversion
           (Source => cdatatype,
            Target => adatatype);

         cdata : cdatatype := IC.Strings.Value (ptr, nmemb);
         adata : adatatype := char_to_stream (cdata);

      begin
         SIO.Write (zdata.file_handle, adata);
      end;

      zdata.totalsize := zdata.totalsize + bytes_passed;
      return nmemb;

   end write_file;

end curl_callbacks;
