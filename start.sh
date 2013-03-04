#!/bin/bash
erl -pa ebin deps/*/ebin -s erlpocket -s reloader
