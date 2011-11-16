dnl sxe-crypto.m4 -- Cryptographical stuff

dnl SSL detection
dnl =============

AC_DEFUN([SXE_PATH_OPENSSL_BIN], [dnl
	AC_CHECK_PROG([have_openssl_bin], [openssl], [yes], [no])
	AC_PATH_PROG([OPENSSL_BIN], [openssl], [echo])
])dnl SXE_PATH_OPENSSL_BIN

AC_DEFUN([SXE_OPENSSL_VERSION], [dnl
	## assumes SXE_PATH_OPENSSL_BIN has been run already
	AC_MSG_CHECKING([for openssl version])
	if test "$have_openssl_bin" = "yes"; then
		OPENSSL_VERSION=`$OPENSSL_BIN version`
	else
		OPENSSL_VERSION="unknown"
	fi
	AC_MSG_RESULT([$OPENSSL_VERSION])

	AC_MSG_CHECKING([whether OpenSSL version is recent enough])
	## we allow 0.9.7e-?, 0.9.8* and 0.9.9*
	allowed_versions="0.9.7[e-z] 0.9.8* 0.9.9* 1.0.0*"
	OPENSSL_SANE_P=no
	for ver in $allowed_versions; do
		if echo "$OPENSSL_VERSION" | grep -q "$ver"; then
			OPENSSL_SANE_P="yes"
			break;
		fi
	done
	AC_MSG_RESULT([$OPENSSL_SANE_P])
])dnl SXE_OPENSSL_VERSION

AC_DEFUN([SXE_TRY_OPENSSL_HISTORICAL_PREFIX], [dnl
	## ooh, maybe this historical trap to install at /usr/local/ssl
	OPENSSL_CPPFLAGS="-I/usr/local/ssl/include"
	OPENSSL_LDFLAGS="-L/usr/local/ssl/lib"

	## now append these candidates to our c_switch and ld_switch
	SXE_DUMP_LIBS
	SXE_APPEND([$OPENSSL_CPPFLAGS], [CPPFLAGS])
	SXE_APPEND([$OPENSSL_LDFLAGS], [LDFLAGS])

	## check again
	SXE_CHECK_HEADERS([openssl/crypto.h])
	SXE_CHECK_HEADERS([openssl/x509.h openssl/pem.h])
	SXE_CHECK_HEADERS([openssl/ssl.h openssl/bio.h])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])

	## restore
	SXE_RESTORE_LIBS
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto" != "yes yes"; then
		OPENSSL_CPPFLAGS=
		OPENSSL_LDFLAGS=
		openssl_historical_prefix_worked="no"
	else
		openssl_historical_prefix_worked="yes"
	fi
])dnl SXE_TRY_OPENSSL_HISTORICAL_PREFIX

AC_DEFUN([SXE_TRY_OPENSSL_BIN_PREFIX], [dnl
	## use the dirname of the openssl binary to determine the prefix of SSL
	openssl_bindir=`dirname $OPENSSL_BIN`
	openssl_prefix_maybe=`dirname $openssl_bindir`
	OPENSSL_CPPFLAGS="-I$openssl_prefix_maybe/include"
	OPENSSL_LDFLAGS="-L$openssl_prefix_maybe/lib"

	## now append these candidates to our c_switch and ld_switch
	SXE_DUMP_LIBS
	SXE_APPEND([$OPENSSL_CPPFLAGS], [CPPFLAGS])
	SXE_APPEND([$OPENSSL_LDFLAGS], [LDFLAGS])

	## check again
	SXE_CHECK_HEADERS([openssl/crypto.h])
	SXE_CHECK_HEADERS([openssl/x509.h openssl/pem.h])
	SXE_CHECK_HEADERS([openssl/ssl.h openssl/bio.h])
	AC_CHECK_LIB([ssl], [SSL_connect],
		[have_libssl=yes], [have_libssl=no])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])

	## restore
	SXE_RESTORE_LIBS
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto $have_libssl " != "yes yes yes"; then
		OPENSSL_CPPFLAGS=
		OPENSSL_LDFLAGS=
		openssl_bin_prefix_worked="no"
	else
		openssl_bin_prefix_worked="yes"
	fi
])dnl SXE_TRY_OPENSSL_BIN_PREFIX

AC_DEFUN([SXE_CHECK_OPENSSL_LOCS], [dnl
	## defines OPENSSL_CPPFLAGS and OPENSSL_LDFLAGS if needed

	dnl Look for these standard header file locations
	OPENSSL_LIBS="-lssl -lcrypto"
	SXE_CHECK_HEADERS([openssl/crypto.h])
	SXE_CHECK_HEADERS([openssl/x509.h openssl/pem.h])
	SXE_CHECK_HEADERS([openssl/ssl.h openssl/bio.h])
	AC_CHECK_LIB([crypto], [OPENSSL_cleanse],
		[have_libcrypto=yes], [have_libcrypto=no])
	AC_CHECK_LIB([ssl], [SSL_connect],
		[have_libssl=yes], [have_libssl=no])
	if test "$ac_cv_header_openssl_crypto_h $have_libcrypto $have_libssl" != "yes yes yes"; then
	        OPENSSL_LIBS=""
		unset ac_cv_header_openssl_crypto_h
		unset ac_cv_lib_crypto_OPENSSL_cleanse
		SXE_TRY_OPENSSL_BIN_PREFIX
		if test "$openssl_bin_prefix_worked" != "yes"; then
			###/* sigh */
			unset ac_cv_header_openssl_crypto_h
			unset ac_cv_lib_crypto_OPENSSL_cleanse
			SXE_TRY_OPENSSL_HISTORICAL_PREFIX
		fi
	else
		## the location was known already, nothing to do now
		:
	fi
])dnl SXE_CHECK_OPENSSL_LOCS

AC_DEFUN([SXE_CHECK_OPENSSL_FEATURES], [dnl
	dnl test for some special purpose stuff in libcrypto
	AC_CHECK_LIB([crypto], [RSA_new], [openssl_no_rsa=no], [openssl_no_rsa=yes])
	AC_CHECK_LIB([crypto], [DSA_new], [openssl_no_dsa=no], [openssl_no_dsa=yes])
	AC_CHECK_LIB([crypto], [ECDSA_SIG_new], [openssl_no_ecdsa=no],
						[openssl_no_ecdsa=yes])
	AC_CHECK_LIB([crypto], [ECDH_OpenSSL], [openssl_no_ecdh=no],
						[openssl_no_ecdh=yes])
	AC_CHECK_LIB([crypto], [EC_KEY_new], [openssl_no_ec=no], [openssl_no_ec=yes])
	AC_CHECK_LIB([crypto], [DH_new], [openssl_no_dh=no], [openssl_no_dh=yes])
	if test "$openssl_no_rsa" = "yes"; then
		AC_DEFINE([OPENSSL_NO_RSA], [1], [Description here!])
	fi
	if test "$openssl_no_dsa" = "yes"; then
		AC_DEFINE([OPENSSL_NO_DSA], [1], [Description here!])
	fi
	if test "$openssl_no_ecdsa" = "yes"; then
		AC_DEFINE([OPENSSL_NO_ECDSA], [1], [Description here!])
	fi
	if test "$openssl_no_ecdh" = "yes"; then
		AC_DEFINE([OPENSSL_NO_ECDH], [1], [Description here!])
	fi
	if test "$openssl_no_ec" = "yes"; then
		AC_DEFINE([OPENSSL_NO_EC], [1], [Description here!])
	fi
	if test "$openssl_no_dh" = "yes"; then
		AC_DEFINE([OPENSSL_NO_DH], [1], [Description here!])
	fi
	
	dnl check for libssl support
	AC_CHECK_LIB([ssl], [SSL_new], [openssl_ssl=yes], [openssl_ssl=no])
	if test "$openssl_ssl" = "yes"; then
		AC_DEFINE([OPENSSL_SSL], [1], [Description here!])
	fi
])dnl SXE_CHECK_OPENSSL_FEATURES

AC_DEFUN([SXE_CHECK_OPENSSL_FUNCS], [dnl
	SXE_DUMP_LIBS
	LDFLAGS="$LDFLAGS $OPENSSL_LDFLAGS"
	CPPFLAGS="$CPPFLAGS $OPENSSL_CPPFLAGS"
	LIBS="$LIBS $OPENSSL_LIBS"
	AC_CHECK_FUNCS([dnl
		OpenSSL_add_all_digests OpenSSL_add_all_ciphers dnl
		RAND_bytes RAND_query_egd_bytes RAND_status dnl
		EVP_cleanup EVP_MD_CTX_init EVP_DigestInit_ex dnl
		EVP_DigestUpdate EVP_DigestFinal_ex EVP_MD_CTX_cleanup dnl
		HMAC_CTX_init HMAC_Init HMAC_Update HMAC_Final HMAC_CTX_cleanup dnl
		EVP_BytesToKey EVP_CIPHER_CTX_init EVP_EncryptInit dnl
		EVP_EncryptUpdate EVP_EncryptFinal EVP_DecryptInit dnl
		EVP_DecryptUpdate EVP_DecryptFinal EVP_CIPHER_CTX_cleanup dnl
		EVP_PKEY_new RSA_generate_key DSA_generate_parameters dnl
		DSA_generate_key EC_get_builtin_curves dnl
		EC_KEY_new_by_curve_name EC_KEY_generate_key dnl
		EC_KEY_set_private_key EC_KEY_dup dnl
		EVP_SealInit EVP_SealFinal EVP_OpenInit EVP_OpenFinal dnl
		EVP_SignFinal EVP_VerifyFinal dnl
		PEM_read_X509 PEM_read_PUBKEY PEM_read_PrivateKey dnl
		PEM_write_PUBKEY PEM_write_PKCS8PrivateKey dnl
		BIO_new BIO_free BIO_printf BIO_dump BIO_get_callback_arg dnl
		BIO_set_callback BIO_set_callback_arg BIO_read dnl
		SSL_library_init SSL_load_error_strings dnl
		SSLv2_client_method SSLv3_client_method dnl
		SSLv23_client_method TLSv1_client_method dnl
		SSLv2_server_method SSLv3_server_method dnl
		SSLv23_server_method TLSv1_server_method dnl
		SSL_CTX_new SSL_CTX_free SSL_CTX_add_client_CA dnl
		SSL_CTX_load_verify_locations SSL_CTX_use_certificate dnl
		SSL_CTX_use_PrivateKey SSL_CTX_check_private_key dnl
		SSL_CTX_use_certificate_file SSL_CTX_use_PrivateKey_file dnl
		SSL_do_handshake SSL_get_error ssl_verify_cert_chain dnl
		SSL_get_peer_cert_chain SSL_pending SSL_get_certificate dnl
		SSL_get_peer_certificate X509_verify_cert_error_string dnl
		SSL_get_verify_result SSL_get_current_cipher SSL_CIPHER_get_bits])
	if test x"$ac_TLSv1_client_method" = xyes; then
	        AC_DEFINE([HAVE_TLSV1_CLIENT_METHOD], 1, [TLSv1 client methods available])
	fi
	if test x"$ac_SSLv2_client_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV2_CLIENT_METHOD], 1, [SSLv2 client methods available])
	fi
	if test x"$ac_SSLv3_client_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV3_CLIENT_METHOD], 1, [SSLv3 client methods available])
	fi
	if test x"$ac_SSLv23_client_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV23_CLIENT_METHOD], 1, [SSLv23 client methods available])
	fi
	if test x"$ac_TLSv1_server_method" = xyes; then
	        AC_DEFINE([HAVE_TLSV1_SERVER_METHOD], 1, [TLSv1 server methods available])
	fi
	if test x"$ac_SSLv2_server_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV2_SERVER_METHOD], 1, [SSLv2 server methods available])
	fi
	if test x"$ac_SSLv3_server_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV3_SERVER_METHOD], 1, [SSLv3 server methods available])
	fi
	if test x"$ac_SSLv23_server_method" = xyes; then
	        AC_DEFINE([HAVE_SSLV23_SERVER_METHOD], 1, [SSLv23 server methods available])
	fi
	if test x"$ac_ssl_verify_cert_chain" = xyes; then
	        AC_DEFINE([HAVE_SSL_VERIFY_CERT_CHAIN], 1, [ssl_verify_cert_chain available])
	fi
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_OPENSSL_FUNCS

AC_DEFUN([SXE_CHECK_OPENSSL], [dnl
	AC_MSG_CHECKING([for OpenSSL])
	AC_MSG_RESULT([])

	SXE_PATH_OPENSSL_BIN
	dnl defines OPENSSL_VERSION and OPENSSL_SANE_P
	SXE_OPENSSL_VERSION
	if test "$OPENSSL_SANE_P" = "yes"; then
	   	SXE_CHECK_OPENSSL_LOCS
	   	if test "$have_libssl $have_libcrypto" = "yes yes"; then
			have_openssl=yes
			SXE_CHECK_OPENSSL_FEATURES
			SXE_CHECK_OPENSSL_FUNCS
           	fi
	fi
])dnl SXE_CHECK_OPENSSL


dnl Kerberos detection
dnl ==================

AC_DEFUN([SXE_CHECK_KERBEROS], [dnl
	## defines sxe_cv_feat_kerberos
	## call like this SXE_CHECK_GMP([<if-found>], [<if-not-found>])
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for kerberos support],
		[sxe_cv_feat_kerberos], [_SXE_CHECK_KERBEROS])

	if test "$sxe_cv_feat_kerberos5" = "yes"; then
		AC_DEFINE([HAVE_KERBEROS5], [1],
			[Whether kerberos5 support is available!])
	fi
	if test "$sxe_cv_feat_kerberos" = "yes"; then
		ACTION_IF_FOUND
		AC_DEFINE([HAVE_KERBEROS], [1],
			[Whether kerberos support is available!])
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_KERBEROS

AC_DEFUN([SXE_CHECK_KERBEROS5], [dnl
	## defines sxe_cv_feat_kerberos
	## call like this SXE_CHECK_GMP([<if-found>], [<if-not-found>])
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for kerberos5 support],
		[sxe_cv_feat_kerberos5], [_SXE_CHECK_KERBEROS])

	if test "$sxe_cv_feat_kerberos5" = "yes"; then
		ACTION_IF_FOUND
		AC_DEFINE([HAVE_KERBEROS5], [1],
			[Whether kerberos5 support is available!])
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_CHECK_KERBEROS5

AC_DEFUN([_SXE_CHECK_KERBEROS], [dnl
	AC_REQUIRE([SXE_CHECK_KERBEROS_HEADERS])
	AC_REQUIRE([SXE_CHECK_KERBEROS_LIBS])

	if test "$ac_cv_header_krb5_krb5_h" = "yes" -a \
		"$ac_cv_lib_krb5_krb5_sendauth" = "yes" -o \
		"$ac_cv_header_krb5_h" = "yes" -a \
		"$ac_cv_lib_krb5_krb5_sendauth" = "yes"; then
		sxe_cv_feat_kerberos="yes"
		sxe_cv_feat_kerberos5="yes"
		KERBEROS_LIBS="-lkrb5"
	elif test "$ac_cv_header_krb_krb_h" = "yes" -a \
		"$ac_cv_lib_krb_krb_sendauth" = "yes" -o \
		"$ac_cv_header_krb_h" = "yes" -a \
		"$ac_cv_lib_krb_krb_sendauth" = "yes"; then
		sxe_cv_feat_kerberos="yes"
		sxe_cv_feat_kerberos5="no"
		KERBEROS_LIBS="-lkrb"
	elif test "$ac_cv_header_kerberos_krb_h" = "yes" -a \
		"$ac_cv_lib_krb_krb_sendauth" = "yes" -o \
		"$ac_cv_header_kerberosIV_krb_h" = "yes" -a \
		"$ac_cv_lib_krb_krb_sendauth" = "yes"; then
		sxe_cv_feat_kerberos="yes"
		sxe_cv_feat_kerberos5="no"
		KERBEROS_LIBS="-lkrb"
	else
		sxe_cv_feat_kerberos="no"
		sxe_cv_feat_kerberos5="no"
		KERBEROS_LIBS=
	fi
])dnl _SXE_CHECK_KERBEROS

AC_DEFUN([SXE_CHECK_KERBEROS_HEADERS], [dnl
	AC_CHECK_HEADERS([des.h krb.h krb/krb.h])
	AC_CHECK_HEADERS([kerberos/krb.h kerberosIV/krb.h])
	AC_CHECK_HEADERS([krb5.h krb5/krb5.h])
	AC_CHECK_HEADERS([com_err.h krb/com_err.h kerberosIV/krb_err.h])
])dnl SXE_CHECK_KERBEROS_HEADERS

AC_DEFUN([SXE_CHECK_KERBEROS_LIBS], [dnl
	AC_CHECK_LIB([krb], [krb_sendauth], [:])
	AC_CHECK_LIB([krb5], [krb5_sendauth], [:])
])dnl SXE_CHECK_KERBEROS_LIBS

dnl sxe-maths.m4 ends here
