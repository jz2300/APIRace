#!/bin/sh

[[ $0 == /* ]] && echo "$0" || echo "${PWD}/${0#./}"

CXX="/usr/local/bin/clang++"
OPT="/usr/local/bin/opt"
APIRACE_SO="$HOME/src/APIRace/lib/APIRaceInstr.so"
APIRACE_LIB="-lpthread -ldl $HOME/src/APIRace/lib/libtsan-mod.a"

CXX_APIRACE_FLAGS="-fPIE -Wall -fno-builtin" ###-Wall -fno-inline-functions"
CXX_OUTPUT_FLAGS="-pie"
ISOBJ="false"
OUTPUT=""
INPUT=""

VERBOSE="true"
SHOWSTAT="true"
REDIRECT="/dev/null"
DOINSTR="true"
OPTIMIZE=""

LINE="$@"

while [ $# -gt 0 ]; do
    case "$1" in
    -O0)
        OPTIMIZE="-O0"
        shift
        ;;
    -Os|O2|O3|O4)
        OPTIMIZE="-O1"
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

if [ "$DOINSTR" = "false" ]; then
    CXX_APIRACE_FLAGS=""
    CXX_OUTPUT_FLAGS=""
    APIRACE_LIB=""
fi

if [ -z "$OPTIMIZE" ]; then
    OPTIMIZE="-O1"
fi
CXX_FLAGS="$OPTIMIZE $CXX_FLAGS"

if [ "$ISOBJ" = "true" -a "$NFILES" = "1" ]; then
    if [ -z "$OUTPUT" ]; then
        OUTPUT=`echo $INPUT | sed 's/\(.*\.\)\(cc\|cpp\|C\)/\1o/'`
    fi
    if [ "$SHOWSTAT" = "true" ]; then
        REDIRECT=`echo $INPUT | sed 's/\(.*\.\)\(cc\|cpp\|C\)/\1api\.stat/'`
    fi

    CMD="$CXX $CXX_APIRACE_FLAGS $CXX_FLAGS -emit-llvm -c $INPUT -o $INPUT.bc"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD "
    fi
    $CMD

    if [ "$DOINSTR" = "true" ]; then
        CMD="$OPT -load=$APIRACE_SO -apirace $INPUT.bc -o $INPUT.instr.bc"
        if [ "$VERBOSE" = "true" ]; then
            echo "EXEC: $CMD > $REDIRECT 2 > & 1"
        fi
        $CMD > $REDIRECT 2>&1
    else 
        CMD="mv $INPUT.bc $INPUT.instr.bc"
        if [ "$VERBOSE" = "true" ]; then
            echo "EXEC: $CMD "
        fi
        $CMD
    fi

    CMD="$CXX $CXX_APIRACE_FLAGS $CXX_FLAGS -c $INPUT.instr.bc -o $OUTPUT"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD"
    fi
    $CMD

elif [ -n "$OUTPUT" ]; then 
    CMD="$CXX $CXX_APIRACE_FLAGS $CXX_OUTPUT_FLAGS $CXX_FLAGS $INPUT $APIRACE_LIB -o $OUTPUT"
    if [ "$VERBOSE" = "true" ]; then
        echo "EXEC: $CMD"
    fi
    $CMD
else 
    CMD="$CXX $LINE"
    ##echo "EXEC: $CMD"
    $CMD
fi


