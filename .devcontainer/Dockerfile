FROM mcr.microsoft.com/devcontainers/base:bullseye

RUN apt-get update && apt-get install -y \
    build-essential nasm curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 gcc-x86-64-linux-gnu \
    && rm -rf /var/lib/apt/lists/*
