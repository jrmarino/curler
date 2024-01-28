with System;
with Interfaces.C.Strings;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with curl_header;

package curl_callbacks is

   package IC renames Interfaces.C;
   package SIO renames Ada.Streams.Stream_IO;
   package ASU renames Ada.Strings.Unbounded;

   type curldata is
      record
         file_handle : SIO.File_Type;
         totalsize   : Natural := 0;
         max_age     : Natural := 0;
         etag_file   : ASU.Unbounded_String;
         curlobj     : curl_header.CURLX;
      end record;

   function write_file
     (ptr      : IC.Strings.chars_ptr;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t;
   pragma Export (C, write_file);

   function process_header
     (ptr      : IC.Strings.chars_ptr;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t;
   pragma Export (C, process_header);

   --  returns true if the target_file is found, the etag file is found and
   --  the mtime is in the future.
   function found_current_etag_file (filename : String; target_file : String) return Boolean;

   --  returns a string of the current etag
   function saved_etag (filename : String) return String;

private

   --  returns the part of the etag between double quotations
   --  If a pair of quotes aren't present, it's not a valid etag and a blank string is returned.
   function extract_etag (raw_etag : String) return String;

   --  Creates (overwrites) file with etag string for the contents
   procedure write_etag_file (filename : String; etag : String);

end curl_callbacks;
