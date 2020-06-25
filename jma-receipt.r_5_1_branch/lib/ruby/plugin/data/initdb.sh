#!/bin/sh

if psql -lqt | cut -d \| -f 1 | grep -w testplugin; then
  psql -c 'drop database testplugin' template1
fi

createdb -lC -Ttemplate0 -EEUC-JP testplugin
psql testplugin -f tbl_plugin.sql
