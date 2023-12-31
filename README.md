# curler
Test bed for curl bindings

# Repository structure

{ABI}/
{ABI}/meta.rvn
{ABI}/digests.rvn
{ABI}/site.rvn
{ABI]/packages/
{ABI}/packages/ACATS-complete-standard-4.1.31.rvn
{ABI}/packages/ACATS-docs-standard-4.1.31.rvn


# Repository configuration example

FreeBSD: {
	enabled: true
	protocol: "https"
	host: "pkg.freebsd.org"
	folder: "${ABI}"
	srv_mirror: true
	signature_type: "fingerprints",
	fingerprints: "/usr/share/keys/pkg",
}

DragonFly: {
	enabled: false
	protocol: "http"
	host: "muug.ca/mirror/dragonflybsd/rports"
	folder: "${ABI}"
}

DragonFly2: {
	enabled        : True
	protocol       : "https"
	host           : "mirror-master.dragonflybsd.org/rports"
	folder         : "6.4-packages"
	pubkey         : /raven/etc/ravensw/keys/ravenports.key
        signature_type : PUBKEY
}


# http mirror design

After a lot of thought, the http mirror option was eliminated.
There's two functions of a mirror. 
1) Use the closest server to you.
2) Try an alternate host if the primary host is down.

Both of these can be handled with the repository configuration.
The root sets the primary and backup repositories manually and
there you go.

It doesn't appear that FreeBSD pkg uses HTTP mirrors anywhere and
there just seems to be a lot of people configuring HTTP mirrors when
they really mean NONE.  So lets avoid the confusion.

