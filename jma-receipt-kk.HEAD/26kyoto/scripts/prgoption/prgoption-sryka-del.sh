#!/bin/bash

LOG="/var/log/jma-receipt/P26kyoto.log"

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '����ʬ��        ' ; " `
echo ${SQLDATA} > ${LOG}

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '�ܳ�ʬ��        ' ; " `
echo ${SQLDATA} >> ${LOG}

SQLDATA=`psql -c "delete from tbl_prgoption where kbncd = '' ; " `
echo ${SQLDATA} >> ${LOG}

