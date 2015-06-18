#!/bin/bash
	echo $[`cat build` + 1] > build
	./rebar -j 3 compile

