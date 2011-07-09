#!/usr/bin/env sh

erlc -pa . qrly_api.erl qrly_html.erl qrly_xml.erl
erlc qrly_lexer.xrl && erlc qrly_lexer.erl && rm qrly_lexer.erl
mv *.beam ../ebin
