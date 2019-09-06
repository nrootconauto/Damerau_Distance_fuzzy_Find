#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
/*
 *
 * M is text length
 * N is pattern length
 * W is  word size
 * K is maximum error
 * A[i...j] is 1-indexed
 * |A| is length of A
 * e is empty string
 * Ar is A reversed
 * E is alphabet
 * s is alphabet size
 * T[1...n] is text 
 * P[1...m] is pattern
 * ed(A,B) is distance between 2 strings
 * edL(A,B) is levensthien distance
 * edD(AmB) is damerau distance 
 * [1^3]0 is 1110
 * PM[l] is bits forletter l
 * Rd is row d
 * Rdj is bit j of Rd
 */
void printBinary(char* bin,int len) {
	len*=8;
	for(int i=len-1;i!=-1;i--) {
		putchar((bin[i/8]&(1<<(i%8)))?'1':'0');
	}
	putchar('\n');
};
typedef char LD_fuzzyFindChar;
typedef struct {
	char* PMl;
	char* VP;
	char* VN;
	char* DO;
	char* HP;
	char* HN;
	char* TC;
} LD_fuzzyFinderData;
typedef struct {
	char* byte;
	int whatBit;
} bset_bitPointer;
void consec_or(char* dest,char* start,char* start2,int n) {
	for(int i=0;i!=n;i++)
		dest[i]=start[i]|start2[i];
}
void consec_bshiftL1(char* dest,char* start1,int n) {
	unsigned int temp=0;
	for(int i=0;i!=n;i++) {
		temp|=((start1[i]<<1)|(temp&1))&0xff;
		dest[i]=temp;
		temp>>=8;
	}
}
void consec_add(unsigned char* dest,unsigned char* start1,unsigned char* start2,int n) {
	unsigned int A=0;
	for(int i=0;i!=n;i++) {
		A+=(unsigned int)(start1[i])+(unsigned int)(start2[i]);
		dest[i]=(unsigned char)(A&0xff);
		A>>=8;
	}
}
void consec_and(char* dest,char* start1,char* start2,int n) {
	for(int i=0;i!=n;i++)
		dest[i]=start1[i]&start2[i];
}
void consec_xor(char* dest,char* start1,char* start2,int n) {
	for(int i=0;i!=n;i++)
		dest[i]=start1[i]^start2[i];
}
void cap(char* loc,int bits) {
	loc[0]&=(0xff);
}
void consec_invert(char* dest,char* start1,int n) {
	for(int i=0;i!=n;i++)
		dest[i]=~start1[i];
}
void bset_setOrResetXbitsStartingAt(bset_bitPointer* start,int len,bool setOrReset) {
	char mask;
	char offset;
	char maskWidth;
	start->byte--;
	while(len!=0) {
		start->byte++;
		if(start->whatBit+len>=8) {
			offset=start->whatBit;
			start->whatBit=0;
			len-=8-offset;
			maskWidth=8;
		} else if(start->whatBit+len<8) {
			maskWidth=len;
			offset=start->whatBit;
			len=0;
		}
		mask=(0xff>>(8-maskWidth))<<(offset);
		*(start->byte)=(setOrReset)?start->byte[0]|mask:(*start->byte)&~mask;
	}
}

size_t LD_fuzzyFind_allocateSize(int alhpabetSize,int m,int n) {
	size_t md8=2+m/8+(m%8!=0)?1:0;
	size_t lettersSize=md8*alhpabetSize;
	//times 6 is VP,VN,DO,HP,HN,TC
	return md8*6+lettersSize;
}

int* LD_fuzzyFind(char* memLoc,LD_fuzzyFindChar* T,LD_fuzzyFindChar* P,int alphabetSize,int m,int n) {
	size_t md8=m/8+(m%8!=0)?1:0;
	size_t lettersSize=md8*alphabetSize;
	LD_fuzzyFinderData data={
		.PMl=malloc(lettersSize),
		.VN=malloc(3),
		.VP=malloc(3),
		.DO=malloc(3),
		.HN=malloc(3),
		.HP=malloc(3),
		.TC=malloc(3),
	};
	memset(data.PMl,0,lettersSize);
	char* PMx(char letter) {
		return data.PMl+((int)letter)*md8;
	}
	printBinary(PMx(1),1);
	memset(data.VP,0,md8+2);
	bset_bitPointer a={
		.whatBit=0,
		.byte=data.VP
	};
	bset_setOrResetXbitsStartingAt(&a,m,true);
	for(int i=0;i!=m+1;i++) {
		PMx(P[i])[0]|=1<<(i-1);
	}
	memset(data.VN,0,md8);
	memset(data.TC,0,md8);
	int currentDistance=m;
	for(int j=1;j!=n+1;j++) {
		char* TCa=malloc(md8);
		memset(TCa,0,md8);
		char* temp1=malloc(md8);
		memset(temp1,0,md8);
		char* temp2=malloc(md8);
		memset(temp2,0,md8);
		consec_invert(temp2,data.TC,md8); //(~TC)
		consec_and(temp2,PMx(T[j]),temp2,md8); //(~TC)&PMj
		consec_bshiftL1(temp2,temp2,md8); //((~TC)&PMj)<<1
		consec_and(temp2,PMx(T[j-1]),temp2,md8);//(((~TC)&PMj)<<1)&PM[T[j-1]]
		consec_or(TCa,PMx(T[j]),temp2,md8); //PM[T[j]]|((((~TC)&PMj)<<1)&PM[T[j-1]])
		printf("TCa:\n");
		printBinary(TCa,md8);
		
		consec_and(temp2,TCa,data.VP,md8);
		consec_add(temp2,temp2,data.VP,md8);
		consec_xor(temp2,data.VP,temp2,md8);
		consec_or(temp2,data.VN,temp2,md8);;
		unsigned char* D0a=malloc(md8);
		memset(D0a,0,md8);
		consec_or(D0a,TCa,temp2,md8);
		printf("d0:\n");
		printBinary(D0a,md8);
		
		unsigned char* HPa=malloc(md8);
		memset(HPa,0,md8);
		consec_or(temp2,D0a,data.VP,md8);
		consec_invert(temp2,temp2,md8);;
		consec_or(HPa,data.VN,temp2,md8);
		printf("Hpa:\n");
		printBinary(HPa,md8);
		
		unsigned char* HNa=malloc(md8);
		memset(HNa,0,md8);
		consec_and(HNa,data.VP,D0a,md8);
		printf("HNa:\n");
		printBinary(HNa,md8);
		unsigned char* VPa=malloc(md8);
		memset(VPa,0,md8);
		consec_bshiftL1(temp2,HPa,md8);
		consec_or(temp1,D0a,temp2,md8);;
		consec_invert(temp1,temp1,md8);
		consec_bshiftL1(temp2,HNa,md8);
		consec_or(VPa,temp2,temp1,md8);
		printf("VPa:\n");
		printBinary(VPa,md8);
		unsigned char* VNa=malloc(md8);
		memset(VNa,0,md8);
		consec_bshiftL1(temp2,HPa,md8);
		consec_and(VNa,temp2,D0a,md8);
		
		//check for dist
		if(0!=((1<<((m-1)%8))&(HPa[(m-1)/8])))
			currentDistance++;
		if(0!=((1<<((m-1)%8))&(HNa[(m-1)/8])))
			currentDistance--;
		printf("Edit Distance:%i\n",currentDistance);
		memcpy(data.TC,TCa,md8);
		memcpy(data.VP,VPa,md8);
		memcpy(data.VN,VNa,md8);
	}
	return NULL;
}
void fuzzyTest(char* t,char* p,int alphabetSize,unsigned int m,unsigned int n) {
	char alphabet[alphabetSize];
	memset(alphabet,0,alphabetSize);
	for(int i=0;i!=m+1;i++) {
		alphabet[p[i]]|=1<<(i-1);
	}
	char VP=(0xff)>>(8-m);
	char VN=0;
	int currDist=m;
	char TC=0;
	for(int j=1;j!=n+1;j++) {
		char TCa=alphabet[t[j]]|((((~TC)&alphabet[t[j]])<<1)&alphabet[t[j-1]]);
		printf("OTCA:\n");
		printBinary(&TCa,1);
		char D0=(((TCa&VP)+VP)^VP)|TCa|VN;
		printf("OD0:\n");
		printBinary(&D0,1);
		char HPa=VN|~(D0|VP);
		printf("HPa:\n");
		printBinary(&HPa,1);
		char HNa=(D0&VP);
		printf("HNa:\n");
		printBinary(&HNa,1);
		char VPa=(HNa<<1)|~(D0|(HPa<<1));
		
		printf("VPa:\n");
		printBinary(&VPa,1);
		char VNa=(HPa<<1)&D0;
		if(0!=(HPa&(1<<(m-1))))
			currDist++;
		if(0!=(HNa&(1<<(m-1))))
			currDist--;
		printf("CurrDist:%i\n",currDist);
		VP=VPa;
		VN=VNa;
		TC=TCa;
	}
}
int main(int argc,char** argv) {
	size_t size=LD_fuzzyFind_allocateSize(4,3,3);
	char* allocation=malloc(size);
	char cat[]={0,1,3,2,3,1};
	char act[]={0,1,2,3,3,3};
	fuzzyTest(cat,act,4,5,5);
	printf("==============");
	LD_fuzzyFind(NULL,cat,act,4,5,5);
	return (EXIT_SUCCESS);
}

