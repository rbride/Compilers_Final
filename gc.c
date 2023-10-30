#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "gc.h"

typedef uint64_t SNAKEVAL;

void printHelp(FILE* out, SNAKEVAL val);
extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t FORWARD_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t* STACK_BOTTOM;
extern uint64_t* FROM_S;
extern uint64_t* FROM_E;
extern uint64_t* TO_S;
extern uint64_t* TO_E;


void naive_print_heap(uint64_t* heap, uint64_t* heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for(uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %llu/%p: %p (%llu)\n", i, (heap + i), (uint64_t*)(*(heap + i)), *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(uint64_t* from_start, uint64_t* from_end, uint64_t* to_start, uint64_t* to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
}

/*
  Copies a Garter value from the given address to the new heap, 
  but only if the value is heap-allocated and needs copying.

  Arguments:
    garter_val_addr: the *address* of some Garter value, which contains a Garter value,
                     i.e. a tagged word.  
                     It may or may not be a pointer to a heap-allocated value...
    heap_top: the location at which to begin copying, if any copying is needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old location 
    with a forwarding pointer to its new location
 */
uint64_t* copy_if_needed(uint64_t* garter_val_addr, uint64_t* heap_top) {
  uint64_t garter_val = *garter_val_addr;
  if ((garter_val & NUM_TAG_MASK) == 0x0 || (garter_val & CLOSURE_TAG_MASK) == 0x7) {
    // printf("Garter val %#018lx, at %#018lx, is not a closure or tuple.\n", garter_val, (uint64_t)garter_val_addr); 
    return heap_top;
  }
  uint64_t *heap_thing_addr;
  uint64_t heap_thing;

  if ((garter_val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    // printf("Garter val %ld, at %#018lx, is a closure.\n", garter_val, (uint64_t)garter_val_addr); 
    heap_thing_addr = (uint64_t*)(garter_val & ~CLOSURE_TAG_MASK);
    heap_thing = *heap_thing_addr;

    if ((heap_thing & CLOSURE_TAG_MASK) == FORWARD_TAG) {
      *garter_val_addr = (uint64_t)((heap_thing & ~CLOSURE_TAG_MASK) | CLOSURE_TAG);
      return heap_top;
    }

    // 3rd slot is num frees 5th slot and above are frees to recur on
    uint64_t num_frees = heap_thing_addr[2];
    uint64_t num_copies = num_frees + 4;
    // printf("number of slots to be copied %ld.\n", num_copies); 
    int i;

    // 1. Copy full contents from heap_thing to heap_top
    for (i = 0; i < num_copies; i++) {
      heap_top[i] = heap_thing_addr[i]; 
      // printf("Heap val set %#018lx, at %#018lx.\n", heap_top[i], (uint64_t)(&heap_top[i])); 
    }

    // 2. Update val at garter_val_addr with value of heap_top
    *garter_val_addr = (uint64_t)heap_top | CLOSURE_TAG;
    // printf("Garter_val_addr val set %#018lx, at %#018lx.\n", *garter_val_addr, (uint64_t)(garter_val_addr)); 

    // 3. Replace value at heap_thing_addr with forwarding pointer to heap_top
    *heap_thing_addr = (uint64_t)(heap_top) | FORWARD_TAG;
    // printf("heap_thing_addr val set %#018lx, at %#018lx.\n", *heap_thing_addr, (uint64_t)(heap_thing_addr)); 
    
    // 4. increment heap_top to record the allocation
    uint64_t* new_heap_top = heap_top + num_copies;
    uint64_t* aligned = (uint64_t*)(((uint64_t)new_heap_top + 15) & ~0xF);

    // 5. For each field within closure, call copy_if_needed, be careful about using return value of calls correctly
    for (i = 0; i < num_frees; i++) {
      aligned = copy_if_needed(heap_top + i + 4, aligned);
    }
    
    // 6. Return CURRENT heap top
    return aligned;

  } else if ((garter_val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    // printf("---------Start Copying------------\n");
    // printf("Garter val %#018lx, at %#018lx, is a tuple.\n", garter_val, (uint64_t)garter_val_addr); 
    // first slot if num values
    heap_thing_addr = (uint64_t*)(garter_val & ~CLOSURE_TAG_MASK);
    heap_thing = *heap_thing_addr;

    if ((heap_thing & TUPLE_TAG_MASK) == FORWARD_TAG) {
      *garter_val_addr = (uint64_t)((heap_thing & ~TUPLE_TAG_MASK) | TUPLE_TAG);
      return heap_top;
    }

    uint64_t len = heap_thing_addr[0];
    len /= 2;
    int i;

    uint64_t num_copies = len + 1;
    // printf("number of slots to be copied %ld.\n", num_copies); 

    // 1. Copy full contents from heap_thing to heap_top
    for (i = 0; i < num_copies; i++) {
      heap_top[i] = heap_thing_addr[i];
      // printf("Heap val set %#018lx, at %#018lx.\n", heap_top[i], (uint64_t)(&heap_top[i])); 
    }

    // 2. Update val at garter_val_addr with value of heap_top
    *garter_val_addr = (uint64_t)heap_top | TUPLE_TAG;
    // printf("Garter_val_addr val set %#018lx, at %#018lx.\n", *garter_val_addr, (uint64_t)(garter_val_addr)); 

    // 3. Replace value at heap_thing_addr with forwarding pointer to heap_top
    *heap_thing_addr = (uint64_t)heap_top | FORWARD_TAG;
    // printf("heap_thing_addr val set %#018lx, at %#018lx.\n", *heap_thing_addr, (uint64_t)(heap_thing_addr)); 
    
    // 4. increment heap_top to record the allocation
    uint64_t* new_heap_top = heap_top + num_copies;
    uint64_t* aligned = (uint64_t*)(((uint64_t)new_heap_top + 15) & ~0xF);
    // printf("Old heap top: %#018lx, New heap top: %#018lx, after moving %ld words forward\n", (uint64_t)heap_top, (uint64_t)aligned, num_copies);

    // 5. For each field within tuple, call copy_if_needed, be careful about using return value of calls correctly
    for (i = 0; i < len; i++) {
      // printf("Recurring on %#018lx\n", (uint64_t)(heap_thing_addr + i + 1));
      aligned = copy_if_needed(heap_top + i + 1, aligned);
    }
    // printf("--------------------Done copying------------------\n");
    
    // 6. Return CURRENT heap top
    return aligned; 

  } else {
    return heap_top;
    // fprintf(stderr, "Copy_if_needed: unexpected value: %#018lx \n", garter_val);
    // exit(1);
  }

}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost Garter frame
    top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being compacted
    to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
uint64_t* gc(uint64_t* bottom_frame, uint64_t* top_frame, uint64_t* top_stack, uint64_t* from_start, uint64_t* from_end, uint64_t* to_start) {

  uint64_t* old_top_frame = top_frame;
  do {
    for (uint64_t* cur_word = top_stack /* maybe need a +1 here? */; cur_word < top_frame; cur_word++) {
      // printf("Looking at %#018lx\n", (uint64_t)cur_word);
      to_start = copy_if_needed(cur_word, to_start);
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack frame,
     * [top_frame + 8] is the return address, and
     * [top_frame + 16] is therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;
    old_top_frame = top_frame;
    top_frame = (uint64_t*)(*top_frame);
  } while (old_top_frame <= bottom_frame); // Use the old stack frame to decide if there's more GC'ing to do

  // after copying and GC'ing all the stack frames, return the new allocation starting point
  return to_start;       
}

