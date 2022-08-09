#include "stdint.h"

#define VCUR 0x0C
#define VMAX 0x0F

static  uint8_t     curvas        =    0;
static  uint8_t*    pmotor        = 0xF0;
static  uint8_t*    pbocina       = 0xF1;
static     void*  (*pirq_handler) = 0x00;

void irq_handler() {
   register uint8_t vel1;
   register uint8_t vel2;
   register uint8_t bocina;

   curvas = curvas + 1;
   uint8_t curvas_temp;
   curvas_temp = curvas;
   curvas_temp = curvas_temp >> 6; // Z := (curvas_temp >> 0 == 0)
   if(curvas_temp == 0 /* Flag Z */) goto skip_bocina;
   curvas = 0;
   bocina += 1;
   *pbocina = bocina;
skip_bocina:
   SWAP(vel1, vel2);
}

void main() {
   *pirq_handler = irq_handler;
   register uint8_t vel1 = VMAX;
   register uint8_t vel2 = VCUR;
   register uint8_t bocina = 2;
loop:
   *pmotor = vel1;
   goto loop;
}
