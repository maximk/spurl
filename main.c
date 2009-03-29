//
//
//

#include "teeterl.h"

extern unsigned int test_bin_size;
extern unsigned char test_bin_data[];
extern unsigned int orkut_bin_size;
extern unsigned char orkut_bin_data[];
extern unsigned int comet_bin_size;
extern unsigned char comet_bin_data[];
extern unsigned int ason_bin_size;
extern unsigned char ason_bin_data[];
extern unsigned int json_bin_size;
extern unsigned char json_bin_data[];
extern unsigned int ssl_bin_size;
extern unsigned char ssl_bin_data[];
extern unsigned int tls_bin_size;
extern unsigned char tls_bin_data[];
extern unsigned int prf_bin_size;
extern unsigned char prf_bin_data[];
extern unsigned int rsa_bin_size;
extern unsigned char rsa_bin_data[];
extern unsigned int hmac_bin_size;
extern unsigned char hmac_bin_data[];
extern unsigned int ber_bin_size;
extern unsigned char ber_bin_data[];
extern unsigned int base64_bin_size;
extern unsigned char base64_bin_data[];
extern unsigned int xml_bin_size;
extern unsigned char xml_bin_data[];
extern unsigned int spurl_bin_size;
extern unsigned char spurl_bin_data[];
extern unsigned int muse_bin_size;
extern unsigned char muse_bin_data[];

int main(int ac, const char *av[])
{
	teeterl_init();

	teeterl_add_stdmods();

	teeterl_add_compmods();

	teeterl_add_mod("\04test", test_bin_data, test_bin_size);
	teeterl_add_mod("\05orkut", orkut_bin_data, orkut_bin_size);
	teeterl_add_mod("\05comet", comet_bin_data, comet_bin_size);
	teeterl_add_mod("\04ason", ason_bin_data, ason_bin_size);
	teeterl_add_mod("\04json", json_bin_data, json_bin_size);
	teeterl_add_mod("\03ssl", ssl_bin_data, ssl_bin_size);
	teeterl_add_mod("\03tls", tls_bin_data, tls_bin_size);
	teeterl_add_mod("\03prf", prf_bin_data, prf_bin_size);
	teeterl_add_mod("\03rsa", rsa_bin_data, rsa_bin_size);
	teeterl_add_mod("\04hmac", hmac_bin_data, hmac_bin_size);
	teeterl_add_mod("\03ber", ber_bin_data, ber_bin_size);
	teeterl_add_mod("\06base64", base64_bin_data, base64_bin_size);
	teeterl_add_mod("\03xml", xml_bin_data, xml_bin_size);
	teeterl_add_mod("\05spurl", spurl_bin_data, spurl_bin_size);
	teeterl_add_mod("\04muse", muse_bin_data, muse_bin_size);

	teeterl_exec("muse", "start", av+1);
}

//EOF
