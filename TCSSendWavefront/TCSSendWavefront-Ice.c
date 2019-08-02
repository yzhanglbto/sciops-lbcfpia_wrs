/*
 * IIF command line program to be used by lbcfpia to send Zernikes to the TCS.
 * C. Biddick following Andrea Di Paola
 * $Id: TCSSendWavefront.c 9303 2011-05-18 18:57:41Z cjb $
 *
 * This executable and the lbtIIF.client configuration file are copied to
 * /home/lbcobs/LBCFPIA/lbcfpia/ for use by DOFPIA
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ICE_C.h"

int main( int argc, char* argv[] )
{
	char szLine[1024],*ptr;
	char focalStation[64];
	char *szTCSCommandProxy="LBC_Wavefront_Command_Proxy";
	int i,nr,n,ndx,retcode;
	double param, paramerror,AOParam[POLY_ORDER],AOParamError[POLY_ORDER];
	FILE *fptr;
	sWaveFront waveFront;
        sResult result = {EXIT_SUCCESS, 0, NULL};

	printf("\n--------------------------\n");
	printf("TCS Send Wavefront program\n");
	printf("--------------------------\n\n");

	if(argc != 3) {
		printf( " Usage: TCSSendWavefront <filename> <telescopeside>\n" );
		printf( "    ie: TCSSendWavefront lbciaCoeffs.dat left\n" );
		return 1;
	}

	sprintf(focalStation,"prime %s",argv[2]);
	result = ICE_initialize(focalStation,"LBC",szTCSCommandProxy);
	if(result.rescode != EXIT_SUCCESS) {
		printf("Failed to find the IIF subsystem.\n");
		return 1;
	}

	if(!ICE_Authorize(szTCSCommandProxy)) {
		printf("LBC %s is not authorized.\n",argv[2]);
		return 1;
	}

	// read file
	printf("Reading \"%s\" file ... ",argv[1]);
	fptr = fopen(argv[1],"rt");
	if(!fptr) {
		printf("Could not open %s\n",argv[1]);
		return 1;
	}

	*(szLine+sizeof(szLine)-1) = '\0';
	ptr = fgets(szLine,sizeof(szLine),fptr);
	if(ptr) {
		nr = sscanf(szLine,"%d",&retcode);
		if(nr == 1) {
			ptr = fgets(szLine,sizeof(szLine),fptr);
			if(ptr) {
//				printf("%s\n",szLine);
				ptr = fgets(szLine,sizeof(szLine),fptr);
				if(ptr) {
					for(n=0; n<POLY_ORDER; n++) {
						AOParam[n] = 0.0;
						AOParamError[n] = -1.0;
					}
					fgets(szLine,sizeof(szLine),fptr);
					while(!feof(fptr)) {
						nr = sscanf(szLine,"%d %le %le",&ndx,&param,&paramerror);
						if(nr == 3) {;
							AOParam[ndx] = param;
							AOParamError[ndx] = paramerror;
						}
						fgets(szLine,sizeof(szLine),fptr);
					}
					printf("Sending data to TCS ... ");
					fflush(NULL);
					for(n=0; n<POLY_ORDER; n++) waveFront.coeffs[n] = AOParam[n];
					result = ICE_SendWavefront(szTCSCommandProxy,argv[2],&waveFront);
					if(result.rescode == EXIT_SUCCESS) {
					 printf("Success\n");
					} 
					else {
					  char iifCmdResult[1024];
					  for(i=0; i<result.rescount; i++) 
					    strncat( iifCmdResult, result.resmesg[i], strlen(result.resmesg[i]) );
					  printf("SendWavefront failed: %s\n", iifCmdResult);
					}
				
				}
			}
		}
	}
	fclose(fptr);
  
	Result_destroy(&result);
	ICE_destroy(szTCSCommandProxy);

	printf("\n");

	return 0;
}
