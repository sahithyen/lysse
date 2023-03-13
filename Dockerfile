FROM ubuntu
COPY lysse.bin /bin/
RUN chmod +x /bin/lysse.bin
CMD ["/bin/lysse.bin"]
