with Interfaces.C.Strings;

package Unix is

   package IC renames Interfaces.C;

   type file_type   is (directory, regular, symlink, hardlink, fifo, unsupported);
   type nanoseconds is mod 2 ** 32;
   type filetime    is mod 2 ** 64;
   type permissions is mod 2 ** 16;
   type owngrp_id   is mod 2 ** 32;
   type metadata_rc is mod 2 ** 5;

   type Time_Characteristics is
      record
         ftype : file_type;
         mtime : filetime;
         mnsec : nanoseconds;
         error : Boolean;
      end record;

   --  Return file modification time given the path to a file or a directory
   function get_time_characteristics (path : String) return Time_Characteristics;

   --  Returns True if the given file has a modification time in the past
   function tag_expired (mtime : filetime) return Boolean;

   function adjust_metadata
     (path         : String;
      reset_owngrp : Boolean;
      reset_perms  : Boolean;
      reset_mtime  : Boolean;
      type_of_file : file_type;
      new_uid      : owngrp_id;
      new_gid      : owngrp_id;
      new_perms    : permissions;
      new_m_secs   : filetime;
      new_m_nano   : nanoseconds) return metadata_rc;

private

   type stat_block is array (1 .. 256) of IC.unsigned_char;
   type struct_stat is limited
      record
         --  sizeof(struct stat) is 128 on DragonFly
         --  Double that to ensure we allocate enough
         block : stat_block;
      end record;

   type struct_stat_Access is access all struct_stat;
   pragma Convention (C, struct_stat_Access);

   type time_t is new IC.long;
   type timespec is record
      tv_sec   : time_t;
      tv_nsec  : IC.long;
   end record;
   pragma Convention (C, timespec);

   type time_specification is
      record
         epoch : filetime;
         nsecs : nanoseconds;
      end record;

   function arc_get_mtime (sb : struct_stat_Access) return timespec;
   pragma Import (C, arc_get_mtime, "get_mtime");

   function arc_stat
     (path : IC.Strings.chars_ptr;
      sb   : struct_stat_Access) return IC.int;
   pragma Import (C, arc_stat, "lstat");

   function arc_time (tloc : access filetime) return filetime;
   pragma Import (C, arc_time, "time");

   function arc_get_file_type (sb : struct_stat_Access) return IC.unsigned_char;
   pragma Import (C, arc_get_file_type, "get_file_type");

   function success (rc : IC.int) return Boolean;

   function file_modification_time (sb : struct_stat_Access) return time_specification;

   function type_of_file (sb : struct_stat_Access) return file_type;

   function set_metadata
     (path              : IC.char_array;
      reset_modtime     : IC.unsigned_char;
      reset_ownership   : IC.unsigned_char;
      reset_permissions : IC.unsigned_char;
      new_user_id       : IC.unsigned;
      new_group_id      : IC.unsigned;
      new_permissions   : IC.short;
      new_mtime_epoch   : time_t;
      new_mtime_nsecs   : IC.long) return IC.unsigned_char;
   pragma Import (C, set_metadata);

end Unix;
