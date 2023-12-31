with System;
with interfaces.C.Strings;

package curl_header is

   package IC renames interfaces.C;

   type CURLX is new System.Address;

   type CURLcode is
     (CURLE_OK,
      CURLE_UNSUPPORTED_PROTOCOL,
      CURLE_FAILED_INIT,
      CURLE_URL_MALFORMAT,
      CURLE_NOT_BUILT_IN,
      CURLE_COULDNT_RESOLVE_PROXY,
      CURLE_COULDNT_RESOLVE_HOST,
      CURLE_COULDNT_CONNECT,
      CURLE_WEIRD_SERVER_REPLY,
      CURLE_REMOTE_ACCESS_DENIED,
      CURLE_FTP_ACCEPT_FAILED,
      CURLE_FTP_WEIRD_PASS_REPLY,
      CURLE_FTP_ACCEPT_TIMEOUT,
      CURLE_FTP_WEIRD_PASV_REPLY,
      CURLE_FTP_WEIRD_227_FORMAT,
      CURLE_FTP_CANT_GET_HOST,
      CURLE_HTTP2,
      CURLE_FTP_COULDNT_SET_TYPE,
      CURLE_PARTIAL_FILE,
      CURLE_FTP_COULDNT_RETR_FILE,
      CURLE_QUOTE_ERROR,
      CURLE_HTTP_RETURNED_ERROR,
      CURLE_WRITE_ERROR,
      CURLE_UPLOAD_FAILED,
      CURLE_READ_ERROR,
      CURLE_OUT_OF_MEMORY,
      CURLE_OPERATION_TIMEDOUT,
      CURLE_FTP_PORT_FAILED,
      CURLE_FTP_COULDNT_USE_REST,
      CURLE_RANGE_ERROR,
      CURLE_HTTP_POST_ERROR,
      CURLE_SSL_CONNECT_ERROR,
      CURLE_BAD_DOWNLOAD_RESUME,
      CURLE_FILE_COULDNT_READ_FILE,
      CURLE_LDAP_CANNOT_BIND,
      CURLE_LDAP_SEARCH_FAILED,
      CURLE_FUNCTION_NOT_FOUND,
      CURLE_ABORTED_BY_CALLBACK,
      CURLE_BAD_FUNCTION_ARGUMENT,
      CURLE_INTERFACE_FAILED,
      CURLE_TOO_MANY_REDIRECTS,
      CURLE_UNKNOWN_OPTION,
      CURLE_SETOPT_OPTION_SYNTAX,
      CURLE_GOT_NOTHING,
      CURLE_SSL_ENGINE_NOTFOUND,
      CURLE_SSL_ENGINE_SETFAILED,
      CURLE_SEND_ERROR,
      CURLE_RECV_ERROR,
      CURLE_SSL_CERTPROBLEM,
      CURLE_SSL_CIPHER,
      CURLE_PEER_FAILED_VERIFICATION,
      CURLE_BAD_CONTENT_ENCODING,
      CURLE_FILESIZE_EXCEEDED,
      CURLE_USE_SSL_FAILED,
      CURLE_SEND_FAIL_REWIND,
      CURLE_SSL_ENGINE_INITFAILED,
      CURLE_LOGIN_DENIED,
      CURLE_TFTP_NOTFOUND,
      CURLE_TFTP_PERM,
      CURLE_REMOTE_DISK_FULL,
      CURLE_TFTP_ILLEGAL,
      CURLE_TFTP_UNKNOWNID,
      CURLE_REMOTE_FILE_EXISTS,
      CURLE_TFTP_NOSUCHUSER,
      CURLE_SSL_CACERT_BADFILE,
      CURLE_REMOTE_FILE_NOT_FOUND,
      CURLE_SSH,
      CURLE_SSL_SHUTDOWN_FAILED,
      CURLE_AGAIN,
      CURLE_SSL_CRL_BADFILE,
      CURLE_SSL_ISSUER_ERROR,
      CURLE_FTP_PRET_FAILED,
      CURLE_RTSP_CSEQ_ERROR,
      CURLE_RTSP_SESSION_ERROR,
      CURLE_FTP_BAD_FILE_LIST,
      CURLE_CHUNK_FAILED,
      CURLE_NO_CONNECTION_AVAILABLE,
      CURLE_SSL_PINNEDPUBKEYNOTMATCH,
      CURLE_SSL_INVALIDCERTSTATUS,
      CURLE_HTTP2_STREAM,
      CURLE_RECURSIVE_API_CALL,
      CURLE_AUTH_ERROR,
      CURLE_HTTP3,
      CURLE_QUIC_CONNECT_ERROR,
      CURLE_PROXY,
      CURLE_SSL_CLIENTCERT,
      CURLE_UNRECOVERABLE_POLL,
      CURLE_TOO_LARGE,
      CURL_LAST
     );

   for CURLcode use
     (CURLE_OK                       => 0,
      CURLE_UNSUPPORTED_PROTOCOL     => 1,
      CURLE_FAILED_INIT              => 2,
      CURLE_URL_MALFORMAT            => 3,
      CURLE_NOT_BUILT_IN             => 4,
      CURLE_COULDNT_RESOLVE_PROXY    => 5,
      CURLE_COULDNT_RESOLVE_HOST     => 6,
      CURLE_COULDNT_CONNECT          => 7,
      CURLE_WEIRD_SERVER_REPLY       => 8,
      CURLE_REMOTE_ACCESS_DENIED     => 9,
      CURLE_FTP_ACCEPT_FAILED        => 10,
      CURLE_FTP_WEIRD_PASS_REPLY     => 11,
      CURLE_FTP_ACCEPT_TIMEOUT       => 12,
      CURLE_FTP_WEIRD_PASV_REPLY     => 13,
      CURLE_FTP_WEIRD_227_FORMAT     => 14,
      CURLE_FTP_CANT_GET_HOST        => 15,
      CURLE_HTTP2                    => 16,
      CURLE_FTP_COULDNT_SET_TYPE     => 17,
      CURLE_PARTIAL_FILE             => 18,
      CURLE_FTP_COULDNT_RETR_FILE    => 19,
      CURLE_QUOTE_ERROR              => 21,
      CURLE_HTTP_RETURNED_ERROR      => 22,
      CURLE_WRITE_ERROR              => 23,
      CURLE_UPLOAD_FAILED            => 25,
      CURLE_READ_ERROR               => 26,
      CURLE_OUT_OF_MEMORY            => 27,
      CURLE_OPERATION_TIMEDOUT       => 28,
      CURLE_FTP_PORT_FAILED          => 30,
      CURLE_FTP_COULDNT_USE_REST     => 31,
      CURLE_RANGE_ERROR              => 33,
      CURLE_HTTP_POST_ERROR          => 34,
      CURLE_SSL_CONNECT_ERROR        => 35,
      CURLE_BAD_DOWNLOAD_RESUME      => 36,
      CURLE_FILE_COULDNT_READ_FILE   => 37,
      CURLE_LDAP_CANNOT_BIND         => 38,
      CURLE_LDAP_SEARCH_FAILED       => 39,
      CURLE_FUNCTION_NOT_FOUND       => 41,
      CURLE_ABORTED_BY_CALLBACK      => 42,
      CURLE_BAD_FUNCTION_ARGUMENT    => 43,
      CURLE_INTERFACE_FAILED         => 45,
      CURLE_TOO_MANY_REDIRECTS       => 47,
      CURLE_UNKNOWN_OPTION           => 48,
      CURLE_SETOPT_OPTION_SYNTAX     => 49,
      CURLE_GOT_NOTHING              => 52,
      CURLE_SSL_ENGINE_NOTFOUND      => 53,
      CURLE_SSL_ENGINE_SETFAILED     => 54,
      CURLE_SEND_ERROR               => 55,
      CURLE_RECV_ERROR               => 56,
      CURLE_SSL_CERTPROBLEM          => 58,
      CURLE_SSL_CIPHER               => 59,
      CURLE_PEER_FAILED_VERIFICATION => 60,
      CURLE_BAD_CONTENT_ENCODING     => 61,
      CURLE_FILESIZE_EXCEEDED        => 63,
      CURLE_USE_SSL_FAILED           => 64,
      CURLE_SEND_FAIL_REWIND         => 65,
      CURLE_SSL_ENGINE_INITFAILED    => 66,
      CURLE_LOGIN_DENIED             => 67,
      CURLE_TFTP_NOTFOUND            => 68,
      CURLE_TFTP_PERM                => 69,
      CURLE_REMOTE_DISK_FULL         => 70,
      CURLE_TFTP_ILLEGAL             => 71,
      CURLE_TFTP_UNKNOWNID           => 72,
      CURLE_REMOTE_FILE_EXISTS       => 73,
      CURLE_TFTP_NOSUCHUSER          => 74,
      CURLE_SSL_CACERT_BADFILE       => 77,
      CURLE_REMOTE_FILE_NOT_FOUND    => 78,
      CURLE_SSH                      => 79,
      CURLE_SSL_SHUTDOWN_FAILED      => 80,
      CURLE_AGAIN                    => 81,
      CURLE_SSL_CRL_BADFILE          => 82,
      CURLE_SSL_ISSUER_ERROR         => 83,
      CURLE_FTP_PRET_FAILED          => 84,
      CURLE_RTSP_CSEQ_ERROR          => 85,
      CURLE_RTSP_SESSION_ERROR       => 86,
      CURLE_FTP_BAD_FILE_LIST        => 87,
      CURLE_CHUNK_FAILED             => 88,
      CURLE_NO_CONNECTION_AVAILABLE  => 89,
      CURLE_SSL_PINNEDPUBKEYNOTMATCH => 90,
      CURLE_SSL_INVALIDCERTSTATUS    => 91,
      CURLE_HTTP2_STREAM             => 92,
      CURLE_RECURSIVE_API_CALL       => 93,
      CURLE_AUTH_ERROR               => 94,
      CURLE_HTTP3                    => 95,
      CURLE_QUIC_CONNECT_ERROR       => 96,
      CURLE_PROXY                    => 97,
      CURLE_SSL_CLIENTCERT           => 98,
      CURLE_UNRECOVERABLE_POLL       => 99,
      CURLE_TOO_LARGE                => 100,
      CURL_LAST                      => 999
     );
   pragma Convention (C, CURLcode);

   type OptionString is
     (CURLOPT_URL,
      CURLOPT_PROXY,
      CURLOPT_USERPWD,
      CURLOPT_PROXYUSERPWD,
      CURLOPT_RANGE,
      CURLOPT_REFERER,
      CURLOPT_FTPPORT,
      CURLOPT_USERAGENT,
      CURLOPT_COOKIE,
      CURLOPT_SSLCERT,
      CURLOPT_KEYPASSWD,
      CURLOPT_COOKIEFILE,
      CURLOPT_CUSTOMREQUEST,
      CURLOPT_INTERFACE,
      CURLOPT_KRBLEVEL,
      CURLOPT_CAINFO,
      CURLOPT_RANDOM_FILE,
      CURLOPT_EGDSOCKET,
      CURLOPT_COOKIEJAR,
      CURLOPT_SSL_CIPHER_LIST,
      CURLOPT_SSLCERTTYPE,
      CURLOPT_SSLKEY,
      CURLOPT_SSLKEYTYPE,
      CURLOPT_SSLENGINE,
      CURLOPT_CAPATH,
      CURLOPT_ACCEPT_ENCODING,
      CURLOPT_NETRC_FILE,
      CURLOPT_FTP_ACCOUNT,
      CURLOPT_COOKIELIST,
      CURLOPT_FTP_ALTERNATIVE_TO_USER,
      CURLOPT_SSH_PUBLIC_KEYFILE,
      CURLOPT_SSH_PRIVATE_KEYFILE,
      CURLOPT_SSH_HOST_PUBLIC_KEY_MD5,
      CURLOPT_CRLFILE,
      CURLOPT_ISSUERCERT,
      CURLOPT_USERNAME,
      CURLOPT_PASSWORD,
      CURLOPT_PROXYUSERNAME,
      CURLOPT_PROXYPASSWORD,
      CURLOPT_NOPROXY,
      CURLOPT_SSH_KNOWNHOSTS,
      CURLOPT_TLSAUTH_USERNAME,
      CURLOPT_TLSAUTH_PASSWORD,
      CURLOPT_TLSAUTH_TYPE,
      CURLOPT_DNS_SERVERS,
      CURLOPT_DNS_INTERFACE,
      CURLOPT_DNS_LOCAL_IP4,
      CURLOPT_DNS_LOCAL_IP6,
      CURLOPT_LOGIN_OPTIONS,
      CURLOPT_PINNEDPUBLICKEY,
      CURLOPT_UNIX_SOCKET_PATH,
      CURLOPT_PROXY_SERVICE_NAME,
      CURLOPT_SERVICE_NAME,
      CURLOPT_DEFAULT_PROTOCOL,
      CURLOPT_PROXY_CAINFO,
      CURLOPT_PROXY_CAPATH,
      CURLOPT_PROXY_TLSAUTH_USERNAME,
      CURLOPT_PROXY_TLSAUTH_PASSWORD,
      CURLOPT_PROXY_TLSAUTH_TYPE,
      CURLOPT_PROXY_SSLCERT,
      CURLOPT_PROXY_SSLCERTTYPE,
      CURLOPT_PROXY_SSLKEY,
      CURLOPT_PROXY_SSLKEYTYPE,
      CURLOPT_PROXY_KEYPASSWD,
      CURLOPT_PROXY_SSL_CIPHER_LIST,
      CURLOPT_PROXY_CRLFILE,
      CURLOPT_PRE_PROXY,
      CURLOPT_PROXY_PINNEDPUBLICKEY,
      CURLOPT_ABSTRACT_UNIX_SOCKET,
      CURLOPT_REQUEST_TARGET,
      CURLOPT_ALTSVC,
      CURLOPT_PROXY_ISSUERCERT,
      CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256
     );

   for OptionString use
     (CURLOPT_URL             => 2,
      CURLOPT_PROXY           => 4,
      CURLOPT_USERPWD         => 5,
      CURLOPT_PROXYUSERPWD    => 6,
      CURLOPT_RANGE           => 7,
      CURLOPT_REFERER         => 16,
      CURLOPT_FTPPORT         => 17,
      CURLOPT_USERAGENT       => 18,
      CURLOPT_COOKIE          => 22,
      CURLOPT_SSLCERT         => 25,
      CURLOPT_KEYPASSWD       => 26,
      CURLOPT_COOKIEFILE      => 31,
      CURLOPT_CUSTOMREQUEST   => 36,
      CURLOPT_INTERFACE       => 62,
      CURLOPT_KRBLEVEL        => 63,
      CURLOPT_CAINFO          => 65,
      CURLOPT_RANDOM_FILE     => 76,
      CURLOPT_EGDSOCKET       => 77,
      CURLOPT_COOKIEJAR       => 82,
      CURLOPT_SSL_CIPHER_LIST => 83,
      CURLOPT_SSLCERTTYPE     => 86,
      CURLOPT_SSLKEY          => 87,
      CURLOPT_SSLKEYTYPE      => 88,
      CURLOPT_SSLENGINE       => 89,
      CURLOPT_CAPATH          => 97,
      CURLOPT_ACCEPT_ENCODING => 102,
      CURLOPT_NETRC_FILE      => 118,
      CURLOPT_FTP_ACCOUNT     => 134,
      CURLOPT_COOKIELIST      => 135,
      CURLOPT_FTP_ALTERNATIVE_TO_USER => 147,
      CURLOPT_SSH_PUBLIC_KEYFILE      => 152,
      CURLOPT_SSH_PRIVATE_KEYFILE     => 153,
      CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 => 162,
      CURLOPT_CRLFILE                 => 169,
      CURLOPT_ISSUERCERT              => 170,
      CURLOPT_USERNAME                => 173,
      CURLOPT_PASSWORD                => 174,
      CURLOPT_PROXYUSERNAME           => 175,
      CURLOPT_PROXYPASSWORD           => 176,
      CURLOPT_NOPROXY                 => 177,
      CURLOPT_SSH_KNOWNHOSTS          => 183,
      CURLOPT_TLSAUTH_USERNAME        => 204,
      CURLOPT_TLSAUTH_PASSWORD        => 205,
      CURLOPT_TLSAUTH_TYPE            => 206,
      CURLOPT_DNS_SERVERS             => 211,
      CURLOPT_DNS_INTERFACE           => 221,
      CURLOPT_DNS_LOCAL_IP4           => 222,
      CURLOPT_DNS_LOCAL_IP6           => 223,
      CURLOPT_LOGIN_OPTIONS           => 224,
      CURLOPT_PINNEDPUBLICKEY         => 230,
      CURLOPT_UNIX_SOCKET_PATH        => 231,
      CURLOPT_PROXY_SERVICE_NAME      => 235,
      CURLOPT_SERVICE_NAME            => 236,
      CURLOPT_DEFAULT_PROTOCOL        => 238,
      CURLOPT_PROXY_CAINFO            => 246,
      CURLOPT_PROXY_CAPATH            => 247,
      CURLOPT_PROXY_TLSAUTH_USERNAME  => 251,
      CURLOPT_PROXY_TLSAUTH_PASSWORD  => 252,
      CURLOPT_PROXY_TLSAUTH_TYPE      => 253,
      CURLOPT_PROXY_SSLCERT           => 254,
      CURLOPT_PROXY_SSLCERTTYPE       => 255,
      CURLOPT_PROXY_SSLKEY            => 256,
      CURLOPT_PROXY_SSLKEYTYPE        => 257,
      CURLOPT_PROXY_KEYPASSWD         => 258,
      CURLOPT_PROXY_SSL_CIPHER_LIST   => 259,
      CURLOPT_PROXY_CRLFILE           => 260,
      CURLOPT_PRE_PROXY               => 262,
      CURLOPT_PROXY_PINNEDPUBLICKEY   => 263,
      CURLOPT_ABSTRACT_UNIX_SOCKET    => 264,
      CURLOPT_REQUEST_TARGET          => 266,
      CURLOPT_ALTSVC                  => 287,
      CURLOPT_PROXY_ISSUERCERT        => 296,
      CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256 => 311
     );
   pragma Convention (C, OptionString);

   type OptionLong is
     (CURLOPT_PORT,
      CURLOPT_TIMEOUT,
      CURLOPT_INFILESIZE,
      CURLOPT_LOW_SPEED_LIMIT,
      CURLOPT_LOW_SPEED_TIME,
      CURLOPT_RESUME_FROM,
      CURLOPT_SSLVERSION,
      CURLOPT_TIMECONDITION,
      CURLOPT_TIMEVALUE,
      CURLOPT_NETRC,
      CURLOPT_PROXYPORT,
      CURLOPT_POSTFIELDSIZE,
      CURLOPT_HTTPPROXYTUNNEL,
      CURLOPT_MAXREDIRS,
      CURLOPT_MAXCONNECTS,
      CURLOPT_CONNECTTIMEOUT,
      CURLOPT_SSL_VERIFYHOST,
      CURLOPT_HTTP_VERSION,
      CURLOPT_SSLENGINE_DEFAULT,
      CURLOPT_DNS_CACHE_TIMEOUT,
      CURLOPT_COOKIESESSION,
      CURLOPT_BUFFERSIZE,
      CURLOPT_PROXYTYPE,
      CURLOPT_HTTPAUTH,
      CURLOPT_PROXYAUTH,
      CURLOPT_SERVER_RESPONSE_TIMEOUT,
      CURLOPT_IPRESOLVE,
      CURLOPT_MAXFILESIZE,
      CURLOPT_USE_SSL,
      CURLOPT_LOCALPORT,
      CURLOPT_LOCALPORTRANGE,
      CURLOPT_SSH_AUTH_TYPES,
      CURLOPT_FTP_SSL_CCC,
      CURLOPT_TIMEOUT_MS,
      CURLOPT_CONNECTTIMEOUT_MS,
      CURLOPT_NEW_FILE_PERMS,
      CURLOPT_NEW_DIRECTORY_PERMS,
      CURLOPT_POSTREDIR,
      CURLOPT_ADDRESS_SCOPE,
      CURLOPT_TFTP_BLKSIZE,
      CURLOPT_SOCKS5_GSSAPI_NEC,
      CURLOPT_RTSP_CLIENT_CSEQ,
      CURLOPT_RTSP_SERVER_CSEQ,
      CURLOPT_TRANSFER_ENCODING,
      CURLOPT_ACCEPTTIMEOUT_MS,
      CURLOPT_TCP_KEEPIDLE,
      CURLOPT_TCP_KEEPINTVL,
      CURLOPT_SSL_OPTIONS,
      CURLOPT_SASL_IR,
      CURLOPT_EXPECT_100_TIMEOUT_MS,
      CURLOPT_HEADEROPT,
      CURLOPT_STREAM_WEIGHT,
      CURLOPT_PROXY_SSL_VERIFYHOST,
      CURLOPT_PROXY_SSLVERSION,
      CURLOPT_PROXY_SSL_OPTIONS,
      CURLOPT_SUPPRESS_CONNECT_HEADERS,
      CURLOPT_SOCKS5_AUTH,
      CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS,
      CURLOPT_DISALLOW_USERNAME_IN_URL,
      CURLOPT_UPLOAD_BUFFERSIZE,
      CURLOPT_UPKEEP_INTERVAL_MS,
      CURLOPT_ALTSVC_CTRL,
      CURLOPT_MAXAGE_CONN,
      CURLOPT_MAIL_RCPT_ALLOWFAILS,
      CURLOPT_HSTS_CTRL,
      CURLOPT_DOH_SSL_VERIFYHOST,
      CURLOPT_MAXLIFETIME_CONN,
      CURLOPT_MIME_OPTIONS,
      CURLOPT_WS_OPTIONS,
      CURLOPT_CA_CACHE_TIMEOUT,
      CURLOPT_SERVER_RESPONSE_TIMEOUT_MS
     );
   for OptionLong use
     (CURLOPT_PORT                       => 3,
      CURLOPT_TIMEOUT                    => 13,
      CURLOPT_INFILESIZE                 => 14,
      CURLOPT_LOW_SPEED_LIMIT            => 19,
      CURLOPT_LOW_SPEED_TIME             => 20,
      CURLOPT_RESUME_FROM                => 21,
      CURLOPT_SSLVERSION                 => 32,
      CURLOPT_TIMECONDITION              => 33,
      CURLOPT_TIMEVALUE                  => 34,
      CURLOPT_NETRC                      => 51,
      CURLOPT_PROXYPORT                  => 59,
      CURLOPT_POSTFIELDSIZE              => 60,
      CURLOPT_HTTPPROXYTUNNEL            => 61,
      CURLOPT_MAXREDIRS                  => 68,
      CURLOPT_MAXCONNECTS                => 71,
      CURLOPT_CONNECTTIMEOUT             => 78,
      CURLOPT_SSL_VERIFYHOST             => 81,
      CURLOPT_HTTP_VERSION               => 84,
      CURLOPT_SSLENGINE_DEFAULT          => 90,
      CURLOPT_DNS_CACHE_TIMEOUT          => 92,
      CURLOPT_COOKIESESSION              => 96,
      CURLOPT_BUFFERSIZE                 => 98,
      CURLOPT_PROXYTYPE                  => 101,
      CURLOPT_HTTPAUTH                   => 107,
      CURLOPT_PROXYAUTH                  => 111,
      CURLOPT_SERVER_RESPONSE_TIMEOUT    => 112,
      CURLOPT_IPRESOLVE                  => 113,
      CURLOPT_MAXFILESIZE                => 114,
      CURLOPT_USE_SSL                    => 119,
      CURLOPT_LOCALPORT                  => 139,
      CURLOPT_LOCALPORTRANGE             => 140,
      CURLOPT_SSH_AUTH_TYPES             => 151,
      CURLOPT_FTP_SSL_CCC                => 154,
      CURLOPT_TIMEOUT_MS                 => 155,
      CURLOPT_CONNECTTIMEOUT_MS          => 156,
      CURLOPT_NEW_FILE_PERMS             => 159,
      CURLOPT_NEW_DIRECTORY_PERMS        => 160,
      CURLOPT_POSTREDIR                  => 161,
      CURLOPT_ADDRESS_SCOPE              => 171,
      CURLOPT_TFTP_BLKSIZE               => 178,
      CURLOPT_SOCKS5_GSSAPI_NEC          => 180,
      CURLOPT_RTSP_CLIENT_CSEQ           => 193,
      CURLOPT_RTSP_SERVER_CSEQ           => 194,
      CURLOPT_TRANSFER_ENCODING          => 207,
      CURLOPT_ACCEPTTIMEOUT_MS           => 212,
      CURLOPT_TCP_KEEPIDLE               => 214,
      CURLOPT_TCP_KEEPINTVL              => 215,
      CURLOPT_SSL_OPTIONS                => 216,
      CURLOPT_SASL_IR                    => 218,
      CURLOPT_EXPECT_100_TIMEOUT_MS      => 227,
      CURLOPT_HEADEROPT                  => 229,
      CURLOPT_STREAM_WEIGHT              => 239,
      CURLOPT_PROXY_SSL_VERIFYHOST       => 249,
      CURLOPT_PROXY_SSLVERSION           => 250,
      CURLOPT_PROXY_SSL_OPTIONS          => 261,
      CURLOPT_SUPPRESS_CONNECT_HEADERS   => 265,
      CURLOPT_SOCKS5_AUTH                => 267,
      CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS  => 271,
      CURLOPT_DISALLOW_USERNAME_IN_URL   => 278,
      CURLOPT_UPLOAD_BUFFERSIZE          => 280,
      CURLOPT_UPKEEP_INTERVAL_MS         => 281,
      CURLOPT_ALTSVC_CTRL                => 286,
      CURLOPT_MAXAGE_CONN                => 288,
      CURLOPT_MAIL_RCPT_ALLOWFAILS       => 290,
      CURLOPT_HSTS_CTRL                  => 299,
      CURLOPT_DOH_SSL_VERIFYHOST         => 307,
      CURLOPT_MAXLIFETIME_CONN           => 314,
      CURLOPT_MIME_OPTIONS               => 315,
      CURLOPT_WS_OPTIONS                 => 320,
      CURLOPT_CA_CACHE_TIMEOUT           => 321,
      CURLOPT_SERVER_RESPONSE_TIMEOUT_MS => 324
     );
   pragma Convention (C, OptionLong);

   type OptionBool is
     (CURLOPT_CRLF,
      CURLOPT_VERBOSE,
      CURLOPT_HEADER,
      CURLOPT_NOPROGRESS,
      CURLOPT_NOBODY,
      CURLOPT_FAILONERROR,
      CURLOPT_UPLOAD,
      CURLOPT_POST,
      CURLOPT_DIRLISTONLY,
      CURLOPT_APPEND,
      CURLOPT_FOLLOWLOCATION,
      CURLOPT_TRANSFERTEXT,
      CURLOPT_AUTOREFERER,
      CURLOPT_SSL_VERIFYPEER,
      CURLOPT_FILETIME,
      CURLOPT_FRESH_CONNECT,
      CURLOPT_FORBID_REUSE,
      CURLOPT_HTTPGET,
      CURLOPT_FTP_USE_EPSV,
      CURLOPT_NOSIGNAL,
      CURLOPT_UNRESTRICTED_AUTH,
      CURLOPT_FTP_USE_EPRT,
      CURLOPT_FTP_CREATE_MISSING_DIRS,
      CURLOPT_TCP_NODELAY,
      CURLOPT_IGNORE_CONTENT_LENGTH,
      CURLOPT_FTP_SKIP_PASV_IP,
      CURLOPT_CONNECT_ONLY,
      CURLOPT_SSL_SESSIONID_CACHE,
      CURLOPT_HTTP_TRANSFER_DECODING,
      CURLOPT_HTTP_CONTENT_DECODING,
      CURLOPT_PROXY_TRANSFER_MODE,
      CURLOPT_CERTINFO,
      CURLOPT_FTP_USE_PRET,
      CURLOPT_WILDCARDMATCH,
      CURLOPT_TCP_KEEPALIVE,
      CURLOPT_SSL_ENABLE_ALPN,
      CURLOPT_SSL_VERIFYSTATUS,
      CURLOPT_SSL_FALSESTART,
      CURLOPT_PATH_AS_IS,
      CURLOPT_PIPEWAIT,
      CURLOPT_TFTP_NO_OPTIONS,
      CURLOPT_TCP_FASTOPEN,
      CURLOPT_KEEP_SENDING_ON_ERROR,
      CURLOPT_PROXY_SSL_VERIFYPEER,
      CURLOPT_SSH_COMPRESSION,
      CURLOPT_HAPROXYPROTOCOL,
      CURLOPT_DNS_SHUFFLE_ADDRESSES,
      CURLOPT_HTTP09_ALLOWED,
      CURLOPT_DOH_SSL_VERIFYPEER,
      CURLOPT_DOH_SSL_VERIFYSTATUS,
      CURLOPT_QUICK_EXIT
     );
   for OptionBool use
     (CURLOPT_CRLF                       => 27,
      CURLOPT_VERBOSE                    => 41,
      CURLOPT_HEADER                     => 42,
      CURLOPT_NOPROGRESS                 => 43,
      CURLOPT_NOBODY                     => 44,
      CURLOPT_FAILONERROR                => 45,
      CURLOPT_UPLOAD                     => 46,
      CURLOPT_POST                       => 47,
      CURLOPT_DIRLISTONLY                => 48,
      CURLOPT_APPEND                     => 50,
      CURLOPT_FOLLOWLOCATION             => 52,
      CURLOPT_TRANSFERTEXT               => 53,
      CURLOPT_AUTOREFERER                => 58,
      CURLOPT_SSL_VERIFYPEER             => 64,
      CURLOPT_FILETIME                   => 69,
      CURLOPT_FRESH_CONNECT              => 74,
      CURLOPT_FORBID_REUSE               => 75,
      CURLOPT_HTTPGET                    => 80,
      CURLOPT_FTP_USE_EPSV               => 85,
      CURLOPT_NOSIGNAL                   => 99,
      CURLOPT_UNRESTRICTED_AUTH          => 105,
      CURLOPT_FTP_USE_EPRT               => 106,
      CURLOPT_FTP_CREATE_MISSING_DIRS    => 110,
      CURLOPT_TCP_NODELAY                => 121,
      CURLOPT_IGNORE_CONTENT_LENGTH      => 136,
      CURLOPT_FTP_SKIP_PASV_IP           => 137,
      CURLOPT_CONNECT_ONLY               => 141,
      CURLOPT_SSL_SESSIONID_CACHE        => 150,
      CURLOPT_HTTP_TRANSFER_DECODING     => 157,
      CURLOPT_HTTP_CONTENT_DECODING      => 158,
      CURLOPT_PROXY_TRANSFER_MODE        => 166,
      CURLOPT_CERTINFO                   => 172,
      CURLOPT_FTP_USE_PRET               => 188,
      CURLOPT_WILDCARDMATCH              => 197,
      CURLOPT_TCP_KEEPALIVE              => 213,
      CURLOPT_SSL_ENABLE_ALPN            => 226,
      CURLOPT_SSL_VERIFYSTATUS           => 232,
      CURLOPT_SSL_FALSESTART             => 233,
      CURLOPT_PATH_AS_IS                 => 234,
      CURLOPT_PIPEWAIT                   => 237,
      CURLOPT_TFTP_NO_OPTIONS            => 242,
      CURLOPT_TCP_FASTOPEN               => 244,
      CURLOPT_KEEP_SENDING_ON_ERROR      => 245,
      CURLOPT_PROXY_SSL_VERIFYPEER       => 248,
      CURLOPT_SSH_COMPRESSION            => 268,
      CURLOPT_HAPROXYPROTOCOL            => 274,
      CURLOPT_DNS_SHUFFLE_ADDRESSES      => 275,
      CURLOPT_HTTP09_ALLOWED             => 285,
      CURLOPT_DOH_SSL_VERIFYPEER         => 306,
      CURLOPT_DOH_SSL_VERIFYSTATUS       => 308,
      CURLOPT_QUICK_EXIT                 => 322
    );
   pragma Convention (C, OptionBool);

   type OptionCallback is
     (CURLOPT_READDATA,
      CURLOPT_HEADERDATA,
      CURLOPT_XFERINFODATA,
      CURLOPT_DEBUGDATA,
      CURLOPT_SSL_CTX_DATA,
      CURLOPT_SOCKOPTDATA,
      CURLOPT_OPENSOCKETDATA,
      CURLOPT_SEEKDATA,
      CURLOPT_SSH_KEYDATA,
      CURLOPT_CHUNK_DATA,
      CURLOPT_CLOSESOCKETDATA,
      CURLOPT_RESOLVER_START_DATA,
      CURLOPT_TRAILERDATA,
      CURLOPT_HSTSREADDATA,
      CURLOPT_HSTSWRITEDATA,
      CURLOPT_PREREQDATA,
      CURLOPT_SSH_HOSTKEYDATA
     );

   for OptionCallback use
     (CURLOPT_READDATA            => 9,
      CURLOPT_HEADERDATA          => 29,
      CURLOPT_XFERINFODATA        => 57,
      CURLOPT_DEBUGDATA           => 95,
      CURLOPT_SSL_CTX_DATA        => 109,
      CURLOPT_SOCKOPTDATA         => 149,
      CURLOPT_OPENSOCKETDATA      => 164,
      CURLOPT_SEEKDATA            => 168,
      CURLOPT_SSH_KEYDATA         => 185,
      CURLOPT_CHUNK_DATA          => 201,
      CURLOPT_CLOSESOCKETDATA     => 209,
      CURLOPT_RESOLVER_START_DATA => 273,
      CURLOPT_TRAILERDATA         => 284,
      CURLOPT_HSTSREADDATA        => 302,
      CURLOPT_HSTSWRITEDATA       => 304,
      CURLOPT_PREREQDATA          => 313,
      CURLOPT_SSH_HOSTKEYDATA     => 317
     );
   pragma Convention (C, OptionCallback);

   type curl_callback is access function
     (ptr      : IC.Strings.chars_ptr;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t;
   pragma Convention (C, curl_callback);

   --  initialize curl object
   function curl_easy_init return CURLX;
   pragma Import (C, curl_easy_init, "curl_easy_init");

   --  finalize curl object
   procedure curl_easy_cleanup (curl : CURLX);
   pragma Import (C, curl_easy_cleanup, "curl_easy_cleanup");

   --  overloaded procedure to set curl object string properties
   procedure set_curl_option (curlobj : CURLX; option : OptionString; optvalue : String);

   --  overloaded procedure to set curl object integer properties
   procedure set_curl_option (curlobj : CURLX; option : OptionLong; optvalue : Long_Integer);

   --  overloaded procedure to set curl object boolean properties
   procedure set_curl_option (curlobj : CURLX; option : OptionBool; optvalue : Boolean);

   --  overloaded procedure to set curl object callback properties
   procedure set_curl_option (curlobj : CURLX; option : OptionCallback; optvalue : curl_callback);

   procedure execute_curl (curlobj : CURLX);

private

   function curl_setopt_string
     (curl   : CURLX;
      option : OptionString;
      value  : IC.Strings.chars_ptr) return CURLcode;
   pragma Import (C, curl_setopt_string, "curl_easy_setopt");

   function curl_setopt_long
     (curl   : CURLX;
      option : OptionLong;
      value  : IC.long) return CURLcode;
   pragma Import (C, curl_setopt_long, "curl_easy_setopt");

   function curl_setopt_bool
     (curl   : CURLX;
      option : OptionBool;
      value  : IC.long) return CURLcode;
   pragma Import (C, curl_setopt_bool, "curl_easy_setopt");

   function curl_setopt_callback
     (curl   : CURLX;
      option : OptionCallback;
      value  : curl_callback) return CURLcode;
   pragma Import (C, curl_setopt_callback, "curl_easy_setopt");

   function curl_easy_perform
     (curl   : CURLX) return CURLcode;
   pragma Import (C, curl_easy_perform, "curl_easy_perform");


end curl_header;
