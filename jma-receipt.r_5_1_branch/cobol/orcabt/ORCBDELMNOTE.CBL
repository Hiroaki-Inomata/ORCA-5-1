      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBDELMNOTE.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ģ�ǡ�������
      *  ����ݡ��ͥ��̾  : ������Ģ�ǡ��������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/12/17    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-MNOTE-H         PIC 9(01).
           03  FLG-MNOTE-B         PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MNOTE-DELYMD    PIC X(08).
           03  WRK-DELYMD          PIC X(08).
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-SAVE//.
      *
       01  CONST-AREA.
           03  CONST-MNOTE         PIC X(08)   VALUE "MNOTE".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
      *
      *    ������Ģ��Ǽ����
       01  MNOTE-H-REC.
           COPY    "CPMNOTE-H.INC".
      *
      *    ������Ģ��Ǽ����
       01  MNOTE-B-REC.
           COPY    "CPMNOTE-B.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ������Ģ���極��
           COPY    "CPORCSMNOTE.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
           COPY    "CPCOMMAND-PARAM.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           .
      *
       000-PROC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       ORCSMNOTEAREA
                                       ASS-SAVE
      *
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO                        SPA-TERMID
                                       SPA-HOSPNUM
                                       SMNOTE-MODE
                                       SMNOTE-TBL-KEY
                                       SMNOTE-RENNUM
                                       SMNOTE-TBL-GROUP
                                       SMNOTE-SHORI-RENNUM
                                       SMNOTE-SHELLID
                                       SMNOTE-SRYYM
                                       SMNOTE-SRYYMD
                                       SMNOTE-PUTFLG
                                       SMNOTE-NYUGAIKBN
                                       SMNOTE-PTID
                                       SMNOTE-DRCD
                                       SMNOTE-TO-FOLDER
                                       SMNOTE-TO-DATA
                                       SMNOTE-CODE-TYPE
                                       SMNOTE-TITLE
                                       ASS-SAVE
                                       SMNOTE-TBL-UUID
                                       SMNOTE-GYOUMU-CD
                                       SMNOTE-LOCATION-CD
                                       SMNOTE-DATA-VERSION
                                       SPA-OPID
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SMCNDATE-YMD    TO  SPA-SRYYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    SMCNDATE-YMD    TO  LNK-DAY6-KIJUN
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           COMPUTE LNK-DAY6-ZOGEN
               =   3
               *   -1
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN TO  WRK-MNOTE-DELYMD
      *
           PERFORM 900-MNOTE-H-KEY10-SEL-SEC
           IF    ( MNOTE-H-HOSPNUM     =   SPA-HOSPNUM )
            AND  ( MNOTE-H-TBL-KEY     =   SMNOTE-TBL-KEY )
            AND  ( MNOTE-H-RENNUM      =   SMNOTE-RENNUM )
            AND  ( MNOTE-H-TBL-GROUP   =   SMNOTE-TBL-GROUP )
            AND  ( MNOTE-H-TBL-UUID    =   SMNOTE-TBL-UUID )
            AND  ( MNOTE-H-SHORI-RENNUM =  SMNOTE-SHORI-RENNUM )
               CONTINUE
           ELSE
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
               MOVE    WRK-MNOTE-DELYMD TO  WRK-DELYMD
               PERFORM 2001-MNOTE-DEL-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ףãӣֳ�Ǽ����������
      *****************************************************************
       2001-MNOTE-DEL-SEC          SECTION.
      *
           INITIALIZE                      MNOTE-H-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-H-TBL-KEY
           MOVE    WRK-DELYMD          TO  MNOTE-H-SRYYMD
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           INITIALIZE                      MNOTE-B-REC
           MOVE    SPA-HOSPNUM         TO  MNOTE-B-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-B-TBL-KEY
           MOVE    WRK-DELYMD          TO  MNOTE-B-SRYYMD
           MOVE    MNOTE-B-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_b"       TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       2001-MNOTE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ�����������(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ���ףãӣֳ�Ǽ������������
      *****************************************************************
       900-MNOTE-H-KEY10-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-MNOTE-H
      *
           INITIALIZE                      MNOTE-H-REC
      *
           MOVE    SPA-HOSPNUM         TO  MNOTE-H-HOSPNUM
           MOVE    SMNOTE-TBL-KEY      TO  MNOTE-H-TBL-KEY
           MOVE    SPA-SRYYMD          TO  MNOTE-H-CREYMD
           MOVE    MNOTE-H-REC         TO  MCPDATA-REC
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MNOTE-H-REC
           ELSE
               MOVE    1               TO  FLG-MNOTE-H
               INITIALIZE                  MNOTE-H-REC
           END-IF
      *
           MOVE    "tbl_mnote_h"       TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-MNOTE-H-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���(FETCH�Ϥ��ʤ�)
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ạ̃ϣӣţãգңӣϣҽ���
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡����ǽ���
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
