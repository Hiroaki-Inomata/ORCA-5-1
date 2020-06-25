#!/bin/bash

LOG="/var/log/jma-receipt/P26kyoto.log"

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '府内分）        ' ; " `
echo ${SQLDATA} > ${LOG}

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '府外分）        ' ; " `
echo ${SQLDATA} >> ${LOG}

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '' ; " `
echo ${SQLDATA} >> ${LOG}

