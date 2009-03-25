#
#
#

CC	      = gcc
LD	      = gcc

#
# The only include file needed is teeterl.h
# it can usually be found in teeterl/xvm
#
INCLUDES  = -I../teeterl/xvm
CCOPTS	  = -ggdb $(INCLUDES) -DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC

#
# Edit the following lines to point to APR libs and teeterl lib
#
APRLIBS = /opt/local/lib/libapr-1.a /opt/local/lib/libaprutil-1.a
TEETERL_LIB = ../teeterl/bin/teeterl.a

#
# Edit to point to exec utility produced when building teeterl
#
EXEC		= ../teeterl/bin/exec

#
# No need to edit much underneath this line
###########################################

M			= mod
X			= xbin
B			= bin

XS			= $X/spurl.cx \
			  $X/base64.cx \
			  $X/ber.cx \
			  $X/hmac.cx \
			  $X/rsa.cx \
			  $X/prf.cx \
			  $X/tls.cx \
			  $X/ssl.cx \
			  $X/xml.cx \
			  $X/base64.cx \
			  $X/html.cx \
			  $X/nip.cx \
			  $X/ason.cx \
			  $X/muse.cx \
			  $X/json.cx \
			  $X/comet.cx \
			  $X/orkut.cx \
			  $X/test.cx

XO			= $B/main.o \
			  $B/ber.o \
			  $B/hmac.o \
			  $B/rsa.o \
			  $B/prf.o \
			  $B/tls.o \
			  $B/ssl.o \
			  $B/xml.o \
			  $B/base64.o \
			  $B/html.o \
			  $B/nip.o \
			  $B/ason.o \
			  $B/spurl.o \
			  $B/muse.o \
			  $B/json.o \
			  $B/comet.o \
			  $B/orkut.o \
			  $B/test.o

main.c:
	$(EXEC) appgen main spurl muse spurl xml base64 ber hmac rsa prf tls ssl json ason comet orkut test .

$B/spurl: $(XO) $(TEETERL_LIB)
	$(LD) $(LDOPTS) -Xlinker -lpthread -o $B/spurl $(XO) $(TEETERL_LIB) $(APRLIBS) $(STDLIBS)

all:	$(XS) $B/spurl

clean:
	-rm $B/*.o $B/spurl

.SUFFIXES:: .c .erl .x .cx

$X/%.cx : $M/%.erl
	$(EXEC) x_compile files $< $X

$B/%.o : $X/%.cx
	$(CC) -c $(CCOPTS) -o $@ -x c $<

$B/%.o : %.c
	$(CC) -c $(CCOPTS) -o $@ $<

# EOF
