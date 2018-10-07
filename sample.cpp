/*
Sample code for reading and writing binary traces.

Usage:
sample [--write] <filename>

With --write, writes a trace to <filename> while printing "ground truth" to
stdout. Otherwise, read a trace from <filename> and print the results.

This can be used as a self-test with:
sample --write trace.out > expected && diff expected <(sample trace.out)

If reading and writing are working correctly, both invocations of sample
will produce the same output.
*/

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
#include <fstream>

#include "TypeDescription.hpp"
#include "TraceBufferSize.hpp"
#include "TraceVarInfo.hpp"
#include "TracePoint.hpp"
#include "Trace.hpp"

void write_test_trace(const char *filename)
{
    Names names = {
        "int",
        "*char",
        "char",
        "float",
        "double",
        "unsigned int",
        "string",
        "i",
        "ptr",
        "c",
        "f",
        "d",
        "u",
    };

    TypeDescriptions types = {
        TypeDescription(0, SIGNED, sizeof(int)),
        TypeDescription(1, POINTER, sizeof(char *)),
        TypeDescription(2, SIGNED, sizeof(char)),
        TypeDescription(3, FLOAT, sizeof(float)),
        TypeDescription(4, FLOAT, sizeof(double)),
        TypeDescription(5, UNSIGNED, sizeof(unsigned int)),
        TypeDescription(6, BLOB, 0),
    };

    std::vector<TracePoint> points;

    printf("names:\n");
    for (uint32_t i = 0; i < names.size(); i++) {
        printf("  %s\n", names[i].c_str());
    }
    printf("\ntypes:\n");
    for (uint32_t i = 0; i < types.size(); i++) {
        const TypeDescription & type = types[i];
        printf("  %s: %u, %u bytes\n",
               names[type.getNameIndex()].c_str(),
               type.getTypeFormat(),
               type.getSize());
    }
    printf("\n");

    const char *chars = "hello, world";
    for (int i = 0; i < 10; i++) {
        VarValue val;
        const char *ptr = chars + i;
        char c = *ptr;
        float f = 0.1 * i;
        double d = 0.2 * i;
        unsigned int u = 2 * i;
        uint64_t aux = i * 100;
        TraceBufferSize bufferSize(0xff + i, 10 * i);

        TraceBufferSizes bufferSizes;
        FlyweightTraceVarInfos vars;
        Aux auxs;

        bufferSizes.push_back(bufferSize);

        val.s = i;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[0].getTypeFormat(),
                                         sizeof(i)),
                           7, 0, 0, 0));
        val.ptr = (void*) ptr;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[1].getTypeFormat(),
                                         sizeof(ptr)),
                           8, 1, 0, 0));
        val.s = c;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[2].getTypeFormat(),
                                         sizeof(c)),
                           9, 2, 0, 0));
        val.f = f;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[3].getTypeFormat(),
                                         sizeof(f)),
                           10, 3, 0, 0));
        val.d = d;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[4].getTypeFormat(),
                                         sizeof(d)),
                           11, 4, 0, 0));
        val.u = u;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[5].getTypeFormat(),
                                         sizeof(u)),
                           12, 5, 0, 0));
        val.ptr = (void*) ptr;
        vars.push_back(FlyweightTraceVarInfo(
                           TraceVarValue(val,
                                         types[6].getTypeFormat(),
                                         strlen(ptr)),
                           8, 6, 0, 0));

        auxs.push_back(aux);

        points.push_back(TracePoint(i, 100 + i, bufferSizes, vars, auxs));

        printf("ID: %d\n", 100 + i);
        printf("i: int, %lu bytes = %d\n", sizeof(i), i);
        printf("ptr: *char, %lu bytes = %lx\n", sizeof(ptr), (size_t)ptr);
        printf("c: char, %lu bytes = %u '%c'\n", sizeof(c), c, c);
        printf("f: float, %lu bytes = %g\n", sizeof(f), f);
        printf("d: double, %lu bytes = %g\n", sizeof(d), d);
        printf("u: unsigned int, %lu bytes = %u\n", sizeof(u), u);
        printf("ptr: string, %lu bytes = blob: '%s'\n", strlen(ptr), ptr);
        printf("buffer size: %x . %u\n", (0xff + i), 10 * i);
        printf("uint: %lu bytes = %lu\n", sizeof(aux), aux);
        printf("\n");
    }

    Trace trace(names, types, points);
    std::ofstream out(filename, std::ios::out | std::ios::binary);
    out << trace;
    out.close();
}

void read_trace(const char *filename)
{
    std::ifstream in(filename, std::ios::out | std::ios::binary);
    Trace trace(in);

    printf("names:\n");
    for (auto & name : trace.getNames()) {
        printf("  %s\n", name.c_str());
    }
    printf("\ntypes:\n");
    for (auto & type : trace.getTypes()) {
        printf("  %s: %u, %u bytes\n",
               trace.getNames()[type.getNameIndex()].c_str(),
               type.getTypeFormat(),
               type.getSize());
    }
    printf("\n");

    for (auto & tracePoint : trace.getPoints()) {
        printf("ID: %lu\n", tracePoint.getStatement());

        for (auto & var : tracePoint.getVars()) {
            const TypeDescription & type =
                trace.getTypes()[var.get().getTypeIndex()];
            const TraceVarValue & traceVarValue = var.get().getTraceVarValue();

            printf("%s: %s, %u bytes = ",
                   trace.getNames()[var.get().getNameIndex()].c_str(),
                   trace.getNames()[type.getNameIndex()].c_str(),
                   traceVarValue.getSize());

            switch (traceVarValue.getTypeFormat()) {
            case UNSIGNED:
                printf("%lu", traceVarValue.getValue().u);
                if (traceVarValue.getSize() == 1)
                    printf(" '%c'", (unsigned char)traceVarValue.getValue().u);
                break;
            case SIGNED:
                printf("%ld", traceVarValue.getValue().s);
                if (traceVarValue.getSize() == 1)
                    printf(" '%c'", (char)traceVarValue.getValue().u);
                break;
            case POINTER:
                printf("%lx", traceVarValue.getValue().u);
                break;
            case FLOAT:
                if (traceVarValue.getSize() == 4)
                    printf("%g", traceVarValue.getValue().f);
                else
                    printf("%g", traceVarValue.getValue().d);
                break;
            case BLOB:
                printf("blob: '%.*s'",
                       traceVarValue.getSize(),
                       (const char *)traceVarValue.getValue().ptr);
                break;
            default:
                printf("<unrecognized format %u>",
                       traceVarValue.getTypeFormat());
                break;
            }

            printf("\n");
        }

        for (auto & bufferSize : tracePoint.getBufferSizes()) {
            printf("buffer size: %lx . %lu\n",
                   bufferSize.getAddress(), bufferSize.getSize());
        }

        for (auto & aux : tracePoint.getAux()) {
            printf("uint: %lu bytes = %lu\n", sizeof(aux), aux);
        }

        printf("\n");
    }
}

int main(int argc, char **argv)
{
    if (!strcmp(argv[1], "--write")) {
        write_test_trace(argv[2]);
    }
    else {
        read_trace(argv[1]);
    }

    return 0;
}
