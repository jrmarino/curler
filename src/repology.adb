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

   etag_file       : constant String := "/tmp/snapshot.etag";
   downloaded_file : constant String := "/tmp/snapshot.json";
   temporary_file  : constant String := CAL.randomized_download_target (downloaded_file);

   data : CAL.curldata;
   curlobj : curl_header.CURLX;
   response_code : Long_Integer;
   header_list : curl_header.access_curl_slist := null;
begin
   --  Don't do anything if:
   --  1) downloaded file exists
   --  2) etag file exists
   --  3) etag file modification time is in the future
   if CAL.target_file_cached (downloaded_file, etag_file) then
      Ada.Text_IO.Put_Line ("cached " & downloaded_file & " still valid, nothing done.");
      return;
   end if;

   data.totalsize := 0;
   data.etag_file := CAL.ASU.To_Unbounded_String (etag_file);
   CAL.SIO.Create (data.file_handle, CAL.SIO.Out_File, temporary_file);

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

   if CAL.found_etag_file (etag_file) then
      declare
         set_etag : constant String := "If-None-Match: " &
           LAT.Quotation & CAL.file_to_string (etag_file) & LAT.Quotation;
      begin
         curl_header.build_header (header_list, set_etag);
         curl_header.set_curl_option (curlobj, curl_header.CURLOPT_HTTPHEADER, header_list);
      end;
   end if;

   curl_header.execute_curl (curlobj);

   CAL.SIO.Close (data.file_handle);

   curl_header.curl_slist_free_all (header_list);

   if not CAL.set_expiration_time (etag_file, data.max_age) then
      Ada.Text_IO.Put_Line ("Failed to update modification time of " & etag_file);
   end if;

   response_code := curl_header.get_info_value_long (curlobj,
                                                     curl_header.CURLINFO_RESPONSE_CODE);
   Ada.Text_IO.Put_Line ("response code:" & response_code'Img);
   if response_code = 200 then
      CAL.rename_temporary_file (downloaded_file, temporary_file);
   else
      CAL.remove_temporary_file (temporary_file);
   end if;

end repology;
