with Ada.Text_IO;
with Ada.Characters.Latin_1;
with System;
with curl_header;
with curl_callbacks;

procedure repology
is
   package CAL renames curl_callbacks;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   latest : constant String := "https://raw.githubusercontent.com/Ravenports/Ravenports/" &
                               "master/Mk/Misc/repology.json";

   etag_file : constant String := "/tmp/snapshot.etag";
   downloaded_file : constant String := "/tmp/snapshot.json";

   data : CAL.curldata;
   curlobj : curl_header.CURLX;
   response_code : Long_Integer;
   header_list : curl_header.access_curl_slist := null;
begin

   data.totalsize := 0;
   data.etag_file := CAL.ASU.To_Unbounded_String (etag_file);
   CAL.SIO.Create (data.file_handle, CAL.SIO.Out_File, downloaded_file);

   curlobj := curl_header.curl_easy_init;
   data.curlobj := curlobj;

   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, False);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NOPROGRESS, True);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_TRANSFER_ENCODING, True);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, latest);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_WRITEDATA, data'Address);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HEADERDATA, data'Address);
   curl_header.set_write_callback (curlobj, CAL.write_file'Access);
   curl_header.set_header_callback (curlobj, CAL.process_header'Access);

   if CAL.found_current_etag_file (etag_file, downloaded_file) then
      declare
         set_etag : constant String := "If-None-Match: " &
           LAT.Quotation & CAL.saved_etag (etag_file) & LAT.Quotation;
      begin
         curl_header.build_header (header_list, set_etag);
         curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HTTPHEADER, header_list);
      end;
   end if;

   curl_header.execute_curl (curlobj);

   CAL.SIO.Close (data.file_handle);

   curl_header.curl_slist_free_all (header_list);

   response_code := curl_header.get_info_value_long (curlobj,
                                                     curl_header.CURLINFO_RESPONSE_CODE);
   Ada.Text_IO.Put_Line ("response code:" & response_code'Img);

end repology;
