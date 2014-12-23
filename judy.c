#include <stdio.h>
#include <Judy.h>

#define MAXLINE 1000000                 // max string (line) length

uint8_t   inp_line[MAXLINE];            // input line
uint8_t   Index[MAXLINE];               // string to insert

int     // Usage:  JudySort < file_to_sort
main()
{
    int i;
    int j;
    int ign_next_word;
    Pvoid_t   PJArray = (PWord_t)NULL;  // Judy array.
    PWord_t   PValue;                   // Judy array element.
    Word_t    Bytes;                    // size of JudySL array.

    while (fgets(inp_line, MAXLINE, stdin) != (char *)NULL)
    {
        j = 0;
        ign_next_word = 0;
        for (i = 0; inp_line[i] != 0; i++) {
            int b = inp_line[i];
            if (    (b >= 'a' && b <= 'z') ||
                    (b >= 'A' && b <= 'Z') ||
                    b >= 128) {
                // On a good character, add it to the Index.
                Index[j] = b;
                j++;
                if (j >= MAXLINE) {
                    printf("Failed: Line longer than MAXLINE!");
                    exit(1);
                }
            } else {
                if (j > 0) {
                    // On a bad character,
                    // add the Index if there is one waiting,
                    // unless we should ignore the current word.
                    if (ign_next_word == 0 &&
                            // Disqualifying suffixes.
                            b != '/' && b != '=' &&
                            b != '|') {
                        // null-terminate Index string and add to judy-array
                        Index[j] = 0;
                        JSLI(PValue, PJArray, Index);
                        if (PValue == PJERR) {
                            printf("Malloc failed -- get more ram\n");
                            exit(1);
                        }
                        ++(*PValue);  // count instances of string
                    }
                    j = 0;
                }
                // Disqualifying prefixes.
                if (    b == '&' || b == '[' ||
                        b == '|' || b == '#' ||
                        b == '?' || b == ':' ||
                        b == ';' || b == '/') {
                    ign_next_word = 1;
                } else {
                    ign_next_word = 0;
                }
            }
        }
    }
    Index[0] = '\0';                    // start with smallest string.
    JSLF(PValue, PJArray, Index);       // get first string
    while (PValue != NULL)
    {
        printf("%s\t%ld\n", Index, *PValue);
        JSLN(PValue, PJArray, Index);   // get next string
    }
    JSLFA(Bytes, PJArray);              // free array

    fprintf(stderr, "The JudySL array used %lu bytes of memory\n", Bytes);
    return (0);
}
