with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Latin_1;
with System;
with curl_header;
with curl_callbacks;

procedure release is
   package CAL renames curl_callbacks;
   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   function download_file
     (remote_file_url : String;
      etag_file       : String;
      downloaded_file : String) return Boolean
   is
      temporary_file  : constant String := CAL.randomized_download_target (downloaded_file);

      data : CAL.curldata;
      curlobj : curl_header.CURLX;
      response_code : Long_Integer;
      header_list : curl_header.access_curl_slist := null;
   begin
      if CAL.target_file_cached (downloaded_file, etag_file) then
         return True;
      end if;

      data.totalsize := 0;
      data.etag_file := CAL.ASU.To_Unbounded_String (etag_file);
      CAL.SIO.Create (data.file_handle, CAL.SIO.Out_File, temporary_file);

      curlobj := curl_header.curl_easy_init;
      data.curlobj := curlobj;

      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, False);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NOPROGRESS, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_TRANSFER_ENCODING, True);
      curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, remote_file_url);
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
         return False;
      end if;

      response_code := curl_header.get_info_value_long (curlobj,
                                                     curl_header.CURLINFO_RESPONSE_CODE);
      if response_code = 200 then
         CAL.rename_temporary_file (downloaded_file, temporary_file);
      else
         CAL.remove_temporary_file (temporary_file);
      end if;
      return Ada.Directories.Exists (downloaded_file);
   end download_file;

   Ravenports : constant String := "https://raw.githubusercontent.com/Ravenports/Ravenports";
begin
   if not download_file
     (remote_file_url => Ravenports & "/master/Mk/Misc/latest_release.txt",
      etag_file       => "/tmp/latest_release.etag",
      downloaded_file => "/tmp/latest_release.txt")
   then
      TIO.Put_Line ("Failed to get latest release version.");
      return;
   end if;

   declare
      contents   : constant String := CAL.file_to_string ("/tmp/latest_release.txt");
      releasever : constant String := CAL.first_line (contents);
   begin
      if not download_file
        (remote_file_url => Ravenports & "/" & releasever & "/Mk/Misc/repology.json",
         etag_file       => "/tmp/repology.etag",
         downloaded_file => "/tmp/repology.json")
      then
         TIO.Put_Line ("Failed to get repology version of the last release.");
         return;
      end if;
   end;

end release;
