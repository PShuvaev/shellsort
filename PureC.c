#include <stdlib.h>
#include <stdio.h>

typedef struct Pair {
  int index;
  int value;
} Pair;

Pair* mkPairs(int* group, int groupLen, int* arr){
  Pair* newArr = malloc(sizeof(Pair) * groupLen);
  for(int i = 0; i < groupLen; i++){
    Pair p;
    p.index = group[i];
    p.value = arr[p.index];
    newArr[i] = p;
  }
  return newArr;
}

int* group(int startIndex, int step, int len, int* size){
  int val = startIndex;
  *size = 0;
  for(; val < len; val += step) (*size)++;
  
  int* arr = malloc(sizeof(int) * (*size));
  
  int i = 0;
  for(val = startIndex; val < len; val += step) arr[i++] = val;

  return arr;
}

void print(int* arr, int len){
  for(int i = 0; i < len; i++){
    printf(" %d", arr[i]);
  }
  printf("\n");
}
void printPairs(Pair* arr, int len){
  for(int i = 0; i < len; i++){
    printf(" (%d->%d)", arr[i].index, arr[i].value);
  }
  printf("\n");
}

int ceilPowerOfTwo(int n){
  if((n & (n-1)) == 0) return n;

  int i = 0;
  for(; n > 0; i++){
    n = n >> 1;
  }
  
  return 1 << i;
}

Pair* mergePairs(Pair* pairs, int len){
  Pair* mergedArr = malloc(sizeof(Pair)*len);
  
  if(len == 1) {
    mergedArr[0] = pairs[0];
    return mergedArr;
  }
  
  int evenI = 0;
  int oddI = 1;
  int i = 0;
  do {
    if(evenI < len && (oddI >= len || pairs[evenI].value < pairs[oddI].value)){
      mergedArr[i] = pairs[evenI];
      evenI += 2;
    } else {
      mergedArr[i] = pairs[oddI];
      oddI += 2;
    }
    i++;
  } while(i < len);
  
  return mergedArr;
}


int* loadArrayFromFile(char* fileName, int* size){
  FILE* f = fopen(fileName, "r");
  
  *size = 0;
  
  int number = 0;
  while(fscanf(f, "%d", &number) > 0 ) (*size)++;
  fseek(f, 0, SEEK_SET);
  
  int* arr = malloc(sizeof(int)* (*size));
  for(int i = 0; i < *size; i++){
    int status = fscanf(f, "%d", &(arr[i]));
  }

  fclose(f);
  return arr;
}

void main(){
  int len = 0;
  int* arr = loadArrayFromFile("data", &len);
    
  int n = len / 2;
  int extN = ceilPowerOfTwo(n);
  
  for(int j = extN; j > 0; j = j / 2){
    for(int i = 0; i < j; i++){
      int gsize = 0;
      int *g = group(i, j, len, &gsize);

      Pair* pairs = mkPairs(g, gsize, arr);
      Pair* sortedPairs = mergePairs(pairs, gsize);
      
      for(int k = 0; k < gsize; k++){
        arr[pairs[k].index] = sortedPairs[k].value;
      }
      
      free(g);
      free(pairs);
      free(sortedPairs);
    }
  }
  
  print(arr, len);
}