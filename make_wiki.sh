#!/bin/bash

ch1_files=`ls -v 1*.scm`
ch2_files=`ls -v 2*.scm`
ch3_files=`ls -v 3*.scm`
ch4_files=`ls -v 4*.scm`
ch5_files=`ls -v 5*.scm`
extra=`ls [a-z]*.scm`

rm -f sicp_ex_*.wiki


# $1=wiki filename
# $2=source filenames
generate_wiki ()
{
    for filename in $2
    do
        echo "generate "$1" for "$filename
        echo "== "$filename" ==" >> $1
        echo "" >> $1
        echo "" >> $1
        echo "{{{" >> $1
        cat $filename >> $1
        echo "}}}" >> $1
        echo "" >> $1
        echo "" >> $1
    done
}


generate_wiki sicp_ex_ch1.wiki "$ch1_files"
generate_wiki sicp_ex_ch2.wiki "$ch2_files"
generate_wiki sicp_ex_ch3.wiki "$ch3_files"
generate_wiki sicp_ex_ch4.wiki "$ch4_files"
generate_wiki sicp_ex_ch5.wiki "$ch5_files"
generate_wiki sicp_ex_ch5.wiki "$extra"
