#!/usr/bin/env sh

erlc -pa . qrly.erl qrly_html.erl qrly_xml.erl
erlc qrly_lexer.xrl && erlc qrly_lexer.erl && rm qrly_lexer.erl
erlc qrly_parser.xrl && erlc qrly_parser.erl && rm qrly_parser.erl

mv *.beam ../ebin
