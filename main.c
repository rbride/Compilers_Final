#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "gc.h"

typedef uint64_t SNAKEVAL;

extern SNAKEVAL our_code_starts_here(uint64_t* HEAP, uint64_t size) asm("?our_code_starts_here");
extern void error(uint64_t code, SNAKEVAL val) asm("?error");
extern SNAKEVAL set_stack_bottom(uint64_t* stack_bottom) asm("?set_stack_bottom");
extern SNAKEVAL print(SNAKEVAL val) asm("?print");
extern SNAKEVAL input() asm("?input");
extern SNAKEVAL printStack(SNAKEVAL val, uint64_t* rsp, uint64_t* rbp, uint64_t args) asm("?print_stack");
extern SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) asm("?equal");
extern uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t amount_needed, uint64_t* first_frame, uint64_t* stack_top) asm("?try_gc");
extern uint64_t* HEAP_END asm("?HEAP_END");
extern uint64_t* HEAP asm("?HEAP");

const uint64_t NUM_TAG_MASK     = 0x0000000000000001;
const uint64_t BOOL_TAG_MASK    = 0x0000000000000007;
const uint64_t TUPLE_TAG_MASK   = 0x0000000000000007;
const uint64_t CLOSURE_TAG_MASK = 0x0000000000000007;
const uint64_t NUM_TAG          = 0x0000000000000000;
const uint64_t BOOL_TAG         = 0x0000000000000007;
const uint64_t TUPLE_TAG        = 0x0000000000000001;
const uint64_t CLOSURE_TAG      = 0x0000000000000005;
const uint64_t FORWARD_TAG      = 0x0000000000000003;
const uint64_t BOOL_TRUE        = 0xFFFFFFFFFFFFFFFF;
const uint64_t BOOL_FALSE       = 0x7FFFFFFFFFFFFFFF;
const uint64_t NIL              = ((uint64_t)NULL | TUPLE_TAG);

const uint64_t ERR_COMP_NOT_NUM     = 1;
const uint64_t ERR_ARITH_NOT_NUM    = 2;
const uint64_t ERR_LOGIC_NOT_BOOL   = 3;
const uint64_t ERR_IF_NOT_BOOL      = 4;
const uint64_t ERR_OVERFLOW         = 5;
const uint64_t ERR_GET_NOT_TUPLE    = 6;
const uint64_t ERR_GET_LOW_INDEX    = 7;
const uint64_t ERR_GET_HIGH_INDEX   = 8;
const uint64_t ERR_GET_NOT_NUM      = 9;
const uint64_t ERR_NIL_DEREF        = 10;
const uint64_t ERR_OUT_OF_MEMORY    = 11;
const uint64_t ERR_SET_NOT_TUPLE    = 12;
const uint64_t ERR_SET_LOW_INDEX    = 13;
const uint64_t ERR_SET_NOT_NUM      = 14;
const uint64_t ERR_SET_HIGH_INDEX   = 15;
const uint64_t ERR_CALL_NOT_CLOSURE = 16;
const uint64_t ERR_CALL_ARITY_ERR   = 17;
const uint64_t ERR_INDEX_NOT_NUMBER = 18;
const uint64_t ERR_LET_TUPLE_MISMATCH = 19;

const int64_t MAX_SNAKE_INT = 4611686018427387903;
const int64_t MIN_SNAKE_INT = -4611686018427387904;



size_t HEAP_SIZE;
uint64_t* STACK_BOTTOM;
uint64_t* HEAP;
uint64_t* HEAP_END;

SNAKEVAL set_stack_bottom(uint64_t* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
  return 0;
}

uint64_t* FROM_S;
uint64_t* FROM_E;
uint64_t* TO_S;
uint64_t* TO_E;

SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) {
  if (val1 == val2) { return BOOL_TRUE; }
  if (val1 == NIL || val2 == NIL) { return BOOL_FALSE; }
  if ((val1 & TUPLE_TAG_MASK) == TUPLE_TAG && (val2 & TUPLE_TAG_MASK) == TUPLE_TAG) {
    uint64_t *tup1 = (uint64_t*)(val1 - TUPLE_TAG);
    uint64_t *tup2 = (uint64_t*)(val2 - TUPLE_TAG);
    if (tup1[0] != tup2[0]) { return BOOL_FALSE; }
    for (uint64_t i = 1; i <= tup1[0] / 2; i++) {
      if (equal(tup1[i], tup2[i]) == BOOL_FALSE)
        return BOOL_FALSE;
    }
    return BOOL_TRUE;
  }
  return BOOL_FALSE;
}

uint64_t tupleCounter = 0;
void printHelp(FILE *out, SNAKEVAL val) {
  if (val == NIL) {
    fprintf(out, "nil");
  }
  else if((val & NUM_TAG_MASK) == NUM_TAG) {
    fprintf(out, "%lld", ((int64_t)val) >> 1); // deliberately int64, so that it's signed
  }
  else if(val == BOOL_TRUE) {
    fprintf(out, "true");
  }
  else if(val == BOOL_FALSE) {
    fprintf(out, "false");
  }
  else if ((val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    uint64_t* addr = (uint64_t*)(val - CLOSURE_TAG);
    fprintf(out, "[%p - 5] ==> <function arity %lld, closed %lld, fn-ptr %p>",
            (uint64_t*)val, addr[0]/2, addr[2], (uint64_t*)addr[1]);
    /* fprintf(out, "\nClosed-over values:\n"); */
    /* for (uint64_t i = 0; i < addr[1] / 2; i++) { */
    /*   if (i > 0) { fprintf(out, "\n"); } */
    /*   if ((addr[3 + i] & TUPLE_TAG_MASK) == 5) { */
    /*     fprintf(out, "<closure %p>", (uint64_t*)addr[3 + i]); */
    /*   } else { */
    /*     printHelp(out, addr[3 + i]); */
    /*   } */
    /* } */
  }
  // else if ((val & TUPLE_TAG_MASK) == 3) { 
  //    fprintf(out, "forwarding pointer at %#018lx, forwarding to ", (uint64_t)(&val)); 
  //    fflush(out); 
  //    fprintf(out, "%#018lx\n", (uint64_t*)(val - 3)); 
  //    fflush(out); 
  //    return; 
  //  } 
  else if ((val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    uint64_t* addr = (uint64_t*)(val - TUPLE_TAG);
    // Check whether we've visited this tuple already
    if ((*addr & 0x8000000000000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFFFFFFFFFF));
      return;
    }
    /* if (!(addr >= FROM_S && addr < FROM_E) && !(addr >= TO_S && addr < TO_E)) { */
    /*   fprintf(out, "DANGLING POINTER %p", addr); */
    /*   return; */
    /* } */
    // Mark this tuple: save its length locally, then mark it
    uint64_t len = addr[0];
    if (len & 0x1) { // actually, it's a forwarding pointer
    //  fprintf(out, "forwarding to %#018lx", (uint64_t*)(len - 3));
     fprintf(out, "forwarding pointer at %#018llx, forwarding to %#018llx", (uint64_t)(addr), (uint64_t)(len-3)); 
      return;
    } else {
      len /= 2; // length is encoded
    }
    /* fprintf(out, "Heap is:\n"); */
    /* naive_print_heap(HEAP, HEAP_END); */
    /* fprintf(out, "%p-->(len=%d)", (int*)(val - 1), len / 2); */
    /* fflush(out); */
    *(addr) = 0x8000000000000000 | (++tupleCounter);
    fprintf(out, "(");
    for (uint64_t i = 1; i <= len; i++) {
      if (i > 1) fprintf(out, ", ");
      printHelp(out, addr[i]);
    }
    if (len == 1) fprintf(out, ",");
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = len * 2; // length is encoded
  }
  else {
    fprintf(out, "Unknown value: %#018llx", val);
  }
}


SNAKEVAL printStack(SNAKEVAL val, uint64_t* rsp, uint64_t* rbp, uint64_t args) {
  printf("RSP: %#018llx\t==>  ", (uint64_t)rsp); fflush(stdout);
  printHelp(stdout, *rsp); fflush(stdout);
  printf("\nRBP: %#018llx\t==>  ", (uint64_t)rbp); fflush(stdout);
  printHelp(stdout, *rbp); fflush(stdout);
  printf("\n(difference: %lld)\n", (uint64_t)(rsp - rbp)); fflush(stdout);
  printf("Requested return val: %#018llx\t==> ", (uint64_t)val); fflush(stdout);
  printHelp(stdout, val); fflush(stdout);
  printf("\n"); fflush(stdout);
  printf("Num args: %llu\n", args);

  uint64_t* origRsp = rsp;
  
  if (rsp > rbp) {
    printf("Error: RSP and RBP are not properly oriented\n"); fflush(stdout);
  } else {
    for (uint64_t* cur = rsp; cur < STACK_BOTTOM + 3; cur++) {
      if (cur == STACK_BOTTOM) {
        printf("BOT %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == rbp) {
        printf("RBP %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == origRsp) {
        printf("    %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur); fflush(stdout);
      } else if (cur == rbp + 1) {
        printf("    %#018llx: %#018llx\t==>  saved ret\n", (uint64_t)cur, *cur); fflush(stdout);
        rsp = rbp + 2;
        rbp = (uint64_t*)(*rbp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#018llx: %#018llx\t==>  heap\n", (uint64_t)cur, *cur); fflush(stdout);
      } else {
        printf("    %#018llx: %#018llx\t==>  ", (uint64_t)cur, *cur); fflush(stdout);
        printHelp(stdout, *cur); fflush(stdout);
        printf("\n"); fflush(stdout);
      }
    }
  }
  return val;
}

SNAKEVAL input() {
  char line[60];
  char *end;
  size_t len = 0;

  scanf("%58s", line);

  int64_t num = strtol(line, &end, 10);
  if (*end != '\0') {
    if (strcmp(end, "false") == 0) {
      return BOOL_FALSE;
    }
    else if (strcmp(end, "true") == 0) {
      return BOOL_TRUE;
    } else {
      fprintf(stderr, "Invalid input to program %c\n", *end);
      exit(1);
    }
  }
  if (num > MAX_SNAKE_INT) {
    // fprintf(stderr, "Number %ld is not supported is this language, over max", num);
    error(5, (SNAKEVAL)(num << 1));
  } else if (num < MIN_SNAKE_INT) {
    // fprintf(stderr, "Number %ld is not supported is this language, under min", num);
    error(5, (SNAKEVAL)(num << 1));
  }
  // printf("%ld", num);
  return (SNAKEVAL)(num << 1);
}

SNAKEVAL print(SNAKEVAL val) {
  printHelp(stdout, val);
  printf("\n");
  fflush(stdout);
  return val;
}

void error(uint64_t code, SNAKEVAL val) {
  switch (code) {
  case ERR_COMP_NOT_NUM:
    fprintf(stderr, "Error: comparison expected a number, got "); printHelp(stderr, val);
    break;
  case ERR_ARITH_NOT_NUM:
    fprintf(stderr, "Error: arithmetic expected a number, got "); printHelp(stderr, val);
    break;
  case ERR_LOGIC_NOT_BOOL:
    fprintf(stderr, "Error: logic expected a boolean, got "); printHelp(stderr, val);
    break;
  case ERR_IF_NOT_BOOL:
    fprintf(stderr, "Error: if expected a boolean, got "); printHelp(stderr, val);
    break;
  case ERR_OVERFLOW:
    fprintf(stderr, "Error: Integer overflowed with value, got "); printHelp(stderr, val);
    break;
  case ERR_GET_NOT_TUPLE:
    fprintf(stderr, "Error: get expected tuple, got "); printHelp(stderr, val);
    break;
  case ERR_GET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to get, got %lld\n", (uint64_t)val);
    break;
  case ERR_GET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to get, got %lld\n", (uint64_t)val);
    break;
  case ERR_GET_NOT_NUM:
    fprintf(stderr, "Error: get expected numeric index, got "); printHelp(stderr, val);
    break;
  case ERR_NIL_DEREF:
    fprintf(stderr, "Error: tried to access component of nil\n");
    break;
  case ERR_OUT_OF_MEMORY:
    fprintf(stderr, "Error: out of memory\n");
    break;
  case ERR_SET_NOT_TUPLE:
    fprintf(stderr, "Error: set expected tuple\n");
    break;
  case ERR_SET_LOW_INDEX:
    fprintf(stderr, "Error: index too small to set\n");
    break;
  case ERR_SET_HIGH_INDEX:
    fprintf(stderr, "Error: index too large to set\n");
    break;
  case ERR_SET_NOT_NUM:
    fprintf(stderr, "Error: set expected numeric index, got "); printHelp(stderr, val);
    break;
  case ERR_CALL_NOT_CLOSURE:
    fprintf(stderr, "Error: tried to call a non-closure value: "); printHelp(stderr, val);
    break;
  case ERR_CALL_ARITY_ERR:
    fprintf(stderr, "Error: arity mismatch in call\n");
    break;
  case ERR_INDEX_NOT_NUMBER:
    fprintf(stderr, "Error: index is not a number: "); printHelp(stderr, val);
    break;
  case ERR_LET_TUPLE_MISMATCH:
    fprintf(stderr, "Error: let bindings is less arity than tuple arity: "); printHelp(stderr, val);
    break;
  default:
    fprintf(stderr, "Error: Unknown error code: %lld, val: ", code); printHelp(stderr, val);
  }
  fprintf(stderr, "\n%p ==> ", (uint64_t*)val);
  printHelp(stderr, val);
  fprintf(stderr, "\n");
  fflush(stderr);
  // naive_print_heap(HEAP, HEAP_END);
  fflush(stdout);
  free(HEAP);
  exit(code);
}


/*
  Try to reserve the desired number of bytes of memory, and free garbage if
  needed.  Fail (and exit the program) if there is insufficient memory.  Does 
  not actually allocate the desired number of bytes of memory; the caller 
  will do that.

  Arguments:

    uint64_t* alloc_ptr - the current top of the heap (which we store in R15), where
                          the next allocation should occur, if possible
    uint64_t bytes_needed - the number of bytes of memory we want to allocate
                            (including padding)
    uint64_t* cur_frame - the base pointer of the topmost stack frame of our code
                          (i.e., RBP)
    uint64_t* cur_stack_top - the stack pointer of the topmost stack frame of our
                              code (i.e., RSP)

  Returns:
    The new top of the heap (i.e. the new value of R15) after garbage collection.  
    Does not actually allocate bytes_needed space.

  Side effect:
    Also updates HEAP_END to point to the new end of the heap, if it's changed
*/
uint64_t* try_gc(uint64_t* alloc_ptr, uint64_t bytes_needed, uint64_t* cur_frame, uint64_t* cur_stack_top) {
  uint64_t* new_heap = (uint64_t*)calloc(HEAP_SIZE + 15, sizeof(uint64_t));
  uint64_t* old_heap = HEAP;
  uint64_t* old_heap_end = HEAP_END;

  uint64_t* new_r15 = (uint64_t*)(((uint64_t)new_heap + 15) & ~0xF);
  uint64_t* new_heap_end = new_r15 + HEAP_SIZE;

  FROM_S = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  FROM_E = HEAP_END;
  TO_S = new_r15;
  TO_E = new_heap_end;

  // printf("FROM_S = %p, FROM_E = %p, TO_S = %p, TO_E = %p\n", FROM_S, FROM_E, TO_S, TO_E);
  // naive_print_heap(FROM_S, FROM_E);
  // printStack(BOOL_TRUE, cur_stack_top, cur_frame, 0);
  // fflush(stdout);
  // printf("Trying to allocate %ld bytes.\n", bytes_needed);

  // Abort early, if we can't allocate a new to-space
  if (new_heap == NULL) {
    fprintf(stderr, "Out of memory: could not allocate a new semispace for garbage collection\n");
    fflush(stderr);
    if (old_heap != NULL) free(old_heap);
    exit(ERR_OUT_OF_MEMORY);
  }
  
  new_r15 = gc(STACK_BOTTOM, cur_frame, cur_stack_top, FROM_S, HEAP_END, new_r15);
  HEAP = new_heap;
  HEAP_END = new_heap_end;
  free(old_heap);

  // Note: strict greater-than is correct here: if new_r15 + (bytes_needed / 8) == HEAP_END,
  // that does not mean we're *using* the byte at HEAP_END, but rather that it would be the
  // next free byte, which is still ok and not a heap-overflow.
  if (bytes_needed / sizeof(uint64_t) > HEAP_SIZE) {
    fprintf(stderr, "Allocation error: needed %lld words, but the heap is only %ld words\n",
            bytes_needed / sizeof(uint64_t), HEAP_SIZE);
    fflush(stderr);
    if (new_heap != NULL) free(new_heap);
    exit(ERR_OUT_OF_MEMORY);
  } else if((new_r15 + (bytes_needed / sizeof(uint64_t))) > HEAP_END) {
    fprintf(stderr, "Out of memory: needed %lld words, but only %ld remain after collection\n",
            bytes_needed / sizeof(uint64_t), (HEAP_END - new_r15));
    fflush(stderr);
    if (new_heap != NULL) free(new_heap);
    exit(ERR_OUT_OF_MEMORY);
  } else {
    fprintf(stderr, "new_r15 = %p\n", new_r15); 
    // naive_print_heap(HEAP, HEAP_END);
    return new_r15;
  }
}

/*   NOTE: HEAP vs HEAP_SIZE vs HEAP_END:
    1) HEAP_SIZE: Simply refers to the size of our heap. E.g. if HEAP_SIZE = 10000, means heap can store maximum 10000 words.
    2) HEAP: The actual heap space that we allocate using calloc. The value is the INITIAL heap pointer.
    2*) aligned: This is the "refined" heap pointer we get to make the heap pointer be 16 byte aligned.
    3) HEAP_END: the memory address that marks the end point of the heap.

    *** Key conclusion: HEAP_END is a memory address of the heap. HEAP or aligned are also memory address of the heap.

    Heap space left initially: HEAP_END - HEAP = HEAPSIZE * 8 bytes.
    Heap space left during runtime: HEAP_END - heap_ptr <= HEAPSIZE * 8 bytes

    HEAP_END stays constant throughout the program's execution.

    Heap pointer is dynamic throughout the program's execution. The smallest value it can have is HEAP (initial heap ptr),
    and the maximum value of Heap pointer allowed is HEAP_END, in which case the heap space left is 0 words.  
    
    e.g) HEAP = 0x110008000, aligned = 0x110008000, HEAP_END = 0x1100cb500
    */
int main(int argc, char** argv) {
  HEAP_SIZE = 100000;
  if (argc > 1) { HEAP_SIZE = atoi(argv[1]); }
  if (HEAP_SIZE < 0 || HEAP_SIZE > 1000000) { HEAP_SIZE = 0; }
  HEAP = (uint64_t*)calloc(HEAP_SIZE + 15, sizeof(uint64_t));

  uint64_t* aligned = (uint64_t*)(((uint64_t)HEAP + 15) & ~0xF);
  HEAP_END = aligned + HEAP_SIZE;
  /* printf("HEAP = %p, aligned = %p, HEAP_END = %p\n", HEAP, aligned, HEAP_END); */
  SNAKEVAL result = our_code_starts_here(aligned, HEAP_SIZE);
  /* smarter_print_heap(aligned, HEAP_END, TO_S, TO_E); */
  print(result);

  free(HEAP);
  return 0;
}

