with System;
with Interfaces.C.Strings;
with Ada.Streams.Stream_IO;

package curl_callbacks is

   package IC renames Interfaces.C;
   package SIO renames Ada.Streams.Stream_IO;

   type curldata is
      record
         file_handle : SIO.File_Type;
         totalsize   : Natural;
      end record;

   function write_file (ptr      : IC.Strings.chars_ptr;
                        size     : IC.size_t;
                        nmemb    : IC.size_t;
                        userdata : System.Address) return IC.size_t;
   pragma Export (C, write_file);

end curl_callbacks;
