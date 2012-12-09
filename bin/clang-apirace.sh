#!/bin/sh

[[ $0 == /* ]] && echo "$0" || echo "${PWD}/${0#./}"

CXX="/usr/local/bin/clang++"
OPT="/usr/local/bin/opt"
APIRACE_SO="$HOME/src/APIRace/lib/APIRaceInstr.so"
APIRACE_LIB="-lpthread -ldl $HOME/src/APIRace/lib/libtsan.a"
CXX_APIRACE_FLAGS="-fPIE -fno-builtin -Wall -fno-inline-functions"
# -D__STDC_LIMIT_MACROS
CXX_FLAGS="-D__STDC_LIMIT_MACROS -DMOZ_WIDGET_GTK2"
ISOBJ="false"
OUTPUT=""
INPUT=""
VERBOSE="true"
SHOWSTAT="true"
REDIRECT="/dev/null"

while [ $# -gt 0 ]; do
    case "$1" in
    -v)
        VERBOSE="true"
        shift
        ;;
    -vv)
        SHOWSTAT="true"
        shift
        ;;
    -c) 
        ISOBJ="true"
        shift
        ;;
    -o) 
        OUTPUT="$2"
        shift 2
        ;;
    -*)
        CXX_FLAGS="$CXX_FLAGS $1"
        shift
        ;;
    *)
        EXT=`echo $1 | sed 's/.*\.//'`
        if [ "$EXT" = "cpp" -o "$EXT" = "cc" -o "$EXT" = "C" -o "$EXT" = "c" ]; then
            INPUT="$INPUT $1"
            #echo "INPUT = $INPUT"
        else
            CXX_FLAGS="$CXX_FLAGS $1"
            #echo "$1 --> F"
        fi
        shift
        ;;
    esac
done

NFILES=`echo "$INPUT" | wc -w`

if [ "$ISOBJ" = "true" -a "$NFILES" = "1" ]; then
    if [ -z "$OUTPUT" ]; then
        OUTPUT=`echo $INPUT | sed 's/\(.*\.\)cpp\|cc\|C/\1o/'`
    fi
    if [ "$SHOWSTAT" = "true" ]; then
        REDIRECT=`echo $INPUT | sed 's/\(.*\.\)cpp\|cc\|C/\1stat/'`
    fi

    CMD="$CXX $CXX_APIRACE_FLAGS $CXX_FLAGS -emit-llvm -c $INPUT -o $INPUT.bc"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD"
    fi
    $CMD

    CMD="$OPT -load=$APIRACE_SO -apirace $INPUT.bc -o $INPUT.instr.bc"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD > $REDIRECT 2 > & 1"
    fi
    $CMD > $REDIRECT 2>&1

    CMD="$CXX $CXX_APIRACE_FLAGS $CXX_FLAGS -c $INPUT.instr.bc -o $OUTPUT"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD"
    fi
    $CMD

elif [ "$ISOBJ" = "false" -a -n "$OUTPUT" ]; then

    CMD="$CXX $CXX_APIRACE_FLAGS -pie $CXX_FLAGS $INPUT -o $OUTPUT $APIRACE_LIB"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD"
    fi
    $CMD

else 
    
    echo "wrong call to clang-apirace.sh !!!!!!!!!!!!!!"
    echo "INPUT = $INPUT"
    echo "OUTPUT = $OUTPUT"
    echo "CFLAGS = $CXX_FLAGS"
    echo "ISOBJ = $ISOBJ"
    return 1
fi


