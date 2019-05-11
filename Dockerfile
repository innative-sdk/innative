# Use builder image to build and do initial config
FROM alpine:latest as builder
ADD . /
RUN chmod 644 /include/innative/
RUN chmod 644 /scripts/
RUN chmod 644 /wasm_malloc.c
RUN chmod 755 ./build-llvm.sh
RUN apk add --no-cache git
RUN apk add --no-cache clang alpine-sdk
RUN apk add --no-cache cmake
RUN apk add --no-cache python3
RUN apk add --no-cache zlib-dev
RUN /build-llvm.sh
RUN chmod 644 /spec/test/core/
RUN make

FROM alpine:latest
RUN apk add --no-cache libstdc++
RUN apk add --no-cache libgcc
COPY --from=builder /bin/innative-cmd /innative-cmd
RUN /innative-cmd -i
rm /innative-cmd
COPY --from=builder ./include/ /usr/include/
COPY --from=builder ./scripts/ /usr/scripts/
COPY --from=builder ./wasm_malloc.c /usr/wasm_malloc.c
COPY --from=builder /spec/test/core/ /usr/spec/test/core/