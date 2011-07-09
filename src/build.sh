#!/usr/bin/env sh

erlc -pa . qrly_api.erl qrly_html.erl qrly_xml.erl
mv *.beam ../ebin
