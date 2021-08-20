/* Copyright (c) 2021 Ji Zhu
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include "ver_ex.h"

/* Define the function for the library base version and as the unspecified base one */
__asm__(".symver print_example0, print_example@");
void print_example0(){
	printf("You are in function: %s\n", __FUNCTION__);
}

/* Define the function for the old library version 1.0 */
__asm__(".symver print_example1, print_example@LIBVER_EX_1.0");
void print_example1(){
	printf("You are in function: %s\n", __FUNCTION__);
}

/* Define the function for the new library version 2.0 and as the default one */
__asm__(".symver print_example2, print_example@@LIBVER_EX_2.0");
void print_example2(){
	printf("You are in function: %s\n", __FUNCTION__);
}
