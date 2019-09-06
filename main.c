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
	printf("OR:\n");
	printBinary(start,n);
	printBinary(start2,n);
	for(int i=0;i!=n;i++)
		dest[i]=start[i]|start2[i];
	printBinary(dest,n);
}
void consec_bshiftL1(char* dest,char* start1,int n) {
	printf("bSHIFT1:\n");
	printBinary(start1,n);
	unsigned int temp=0;
	for(int i=0;i!=n;i++) {
		temp=(start1[i]<<1)|(temp&1);
		dest[i]=temp;
		temp>>=8;
	}
	//dest[n]=temp;
	printBinary(dest,n);
}
void consec_add(unsigned char* dest,unsigned char* start1,unsigned char* start2,int n) {
	unsigned int A=0;
	printf("ADD:\n");
	printBinary(start1,n);
	printBinary(start2,n);
	for(int i=0;i!=n;i++) {
		A+=(unsigned int)(start1[i])+(unsigned int)(start2[i]);
		dest[i]=(unsigned char)(A&0xff);
		A>>=8;
	}
	dest[n]=(unsigned char)A;
	printBinary(dest,n+1);
}
void consec_and(char* dest,char* start1,char* start2,int n) {
	printf("AND:\n");
	printBinary(start1,n);
	printBinary(start2,n);
	for(int i=0;i!=n;i++)
		dest[i]=start1[i]&start2[i];
	printBinary(dest,n);
}
void consec_xor(char* dest,char* start1,char* start2,int n) {
	printf("XOR:\n");
	printBinary(start1,n);
	printBinary(start2,n);
	for(int i=0;i!=n;i++)
		dest[i]=start1[i]^start2[i];
	printBinary(dest,n);
}
void cap(char* loc,int bits) {
	loc[0]&=(0xff);
}
void consec_invert(char* dest,char* start1,int n) {
	printf("INVERT:\n");
	printBinary(start1,n);
	for(int i=0;i!=n;i++)
		dest[i]=~start1[i];
	printBinary(dest,n);
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
	md8+=2;
	size_t lettersSize=md8*alphabetSize;
	LD_fuzzyFinderData data={
		.PMl=0+memLoc,
		.VN=lettersSize+memLoc,
		.VP=lettersSize+md8+memLoc,
		.DO=lettersSize+md8*2+memLoc,
		.HN=lettersSize+md8*3+memLoc,
		.HP=lettersSize+md8*4+memLoc,
		.TC=lettersSize+md8*5+memLoc,
	};
	memset(data.PMl,0,lettersSize);
	md8-=2;
	char* PMx(LD_fuzzyFindChar letter) {
		printf("LETTER:%i\n",letter);
		printBinary(data.PMl+((2+md8)*(letter)),1);
		return data.PMl+((2+md8)*(letter));
		
	}
	for(int i=1;i!=m+1;i++) {
		PMx(P[i])[0]|=1<<(i-1);
		printBinary(PMx(P[i]),1);
	}
	printBinary(PMx(1),1);
	memset(data.VP,0,md8+2);
	bset_bitPointer a={
		.whatBit=0,
		.byte=data.VP
	};
	bset_setOrResetXbitsStartingAt(&a,m,true);
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
		printf("1\n");
		consec_invert(temp2,data.TC,md8); //(~TC)
		cap(temp2,m);
		consec_and(temp2,PMx(T[j]),temp2,md8); //(~TC)&PMj
		cap(temp2,m);
		consec_bshiftL1(temp2,temp2,md8); //((~TC)&PMj)<<1
		cap(temp2,m);
		consec_and(temp2,PMx(T[j-1]),temp2,md8);//(((~TC)&PMj)<<1)&PM[T[j-1]]
		cap(temp2,m);
		consec_or(TCa,PMx(T[j]),temp2,md8); //PM[T[j]]|((((~TC)&PMj)<<1)&PM[T[j-1]])
		cap(TCa,m);
		
		printf("2\n");
		consec_and(temp2,TCa,data.VP,md8);
		cap(temp2,m);
		consec_add(temp2,temp2,data.VP,md8);
		cap(temp2,m);
		consec_xor(temp2,data.VP,temp2,md8);
		cap(temp2,m);
		consec_or(temp2,data.VN,temp2,md8);
		cap(temp2,m);
		unsigned char* D0a=malloc(md8);
		memset(D0a,0,md8);
		consec_or(D0a,TCa,temp2,md8);
		cap(D0a,m);
		unsigned char* HPa=malloc(md8);
		memset(HPa,0,md8);
		consec_or(temp2,D0a,data.VP,md8);
		cap(temp2,m);
		consec_invert(temp2,temp2,md8);
		cap(temp2,m);
		consec_or(HPa,data.VN,temp2,md8);
		cap(HPa,m);
		
		unsigned char* HNa=malloc(md8);
		memset(HNa,0,md8);
		consec_and(HNa,data.VP,D0a,md8);
		cap(HNa,m);
		unsigned char* VPa=malloc(md8);
		memset(VPa,0,md8);
		consec_bshiftL1(temp2,HPa,md8);
		cap(temp2,m);
		consec_or(temp1,D0a,temp2,md8);
		cap(temp1,m);
		consec_invert(temp1,temp1,md8);
		cap(temp1,m);
		consec_bshiftL1(temp1,HNa,md8);
		cap(temp1,m);
		consec_or(VPa,temp2,temp1,md8);
		cap(VPa,m);
		
		unsigned char* VNa=malloc(md8);
		memset(VNa,0,md8);
		consec_bshiftL1(temp2,HPa,md8);
		cap(temp2,m);
		consec_and(VNa,temp2,D0a,md8);
		cap(VNa,m);
		
		//check for dist
		if(0!=((1<<((m-1)%8))&(HPa[(m-1)/8])))
			currentDistance++;
		if(0!=((1<<((m-1)%8))&(HNa[(m-1)/8])))
			currentDistance--;
		printf("Edit Distance:%i\n",currentDistance);
		//update values
		printBinary(PMx(1),1);
		memcpy(data.TC,TCa,md8);
		memcpy(data.DO,D0a,md8);
		memcpy(data.HP,HPa,md8);
		memcpy(data.HN,HNa,md8);
		memcpy(data.VP,VPa,md8);
		memcpy(data.VN,VNa,md8);
	}
}
int main(int argc,char** argv) {
	size_t size=LD_fuzzyFind_allocateSize(4,3,3);
	char* allocation=alloca(size);
	char cat[]={3,3,3,3};
	char act[]={3,3,3,3};;
	char test=0b011101;
	printBinary(&test,1);
	LD_fuzzyFind(allocation,cat,act,4,3,3);
	return (EXIT_SUCCESS);
}

