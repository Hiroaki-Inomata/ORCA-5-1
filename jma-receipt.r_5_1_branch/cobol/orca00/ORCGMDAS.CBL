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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGMDAS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��˥塼
      *  ����ݡ��ͥ��̾  : ����ʣͣģ��ӡ�
      *  ������            : 
      *  09/11/04    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
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
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "M00COMMON-SPA".
      *    �����ΰ�
           COPY    "M01SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-USER            PIC 9(01).
           03  FLG-JOBLOG          PIC 9(01).
           03  FLG-DASCONFIG       PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-CONS-AREA.
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PROFILE".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
       01  SHELL-AREA.
           05  SHELL-NAME          PIC X(14)   VALUE   "das-profile.sh".
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����֥���
       01  JOBLOG-REC.
           COPY    "CPJOBLOG.INC".
      *
      *    �����뵯ư
           COPY    "CPSHELLTBL.INC".
      *
      *    ��������
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      * 
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"     ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               MOVE    SPACE           TO  SPA-ERRCD
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    "NEW"           TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 320-MID1-SET-SEC
               WHEN    "MID2"
                   PERFORM 320-MID2-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣͣɣģ��ˣϣ˽���
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           MOVE    SPACE           TO  SPA-MIDCD
           IF      SPA-MID1-FLG        =   "OK"
      *        �����ǡ�������Ϥ���
               PERFORM 2120-DASCONFIG-WRITE-SEC
      *        �������ư����
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 212-SHELL-OUT-SEC
               END-IF
           END-IF
      *
           .
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣͣɣģ��ˣϣ˽���
      *****************************************************************
       320-MID2-SET-SEC            SECTION.
      *
           IF      SPA-MIDCD       NOT =   "0000"
               GO  TO  320-MID2-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
      *
           MOVE    "M98"           TO  SPA-SAKIPG
           MOVE    "MDAS"          TO  SPA-MOTOPG
      *
           MOVE    "M98"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       320-MID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ������Ͻ���
      *****************************************************************
       2120-DASCONFIG-WRITE-SEC        SECTION.
      *
           IF      FLG-DASCONFIG       =   ZERO
               INITIALIZE                      DASCF-REC
               MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
               MOVE    DASCF-REC           TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    SPA-MUID-NAI-SINSEIYMD
                                       TO  DASCF-SINSEIYMD
           MOVE    SPA-MUID-KANANAME   TO  DASCF-KANANAME
           MOVE    SPA-MUID-NAME       TO  DASCF-NAME
           MOVE    SPA-MUID-EMAIL      TO  DASCF-EMAIL
           MOVE    SPA-MUID-HOSPSBT    TO  DASCF-HOSPSBT1
           MOVE    SPA-MUID-HOSPSBT1   TO  DASCF-HOSPSBT2
           MOVE    SPA-MUID-BEDSUIPPAN TO  DASCF-BEDSUIPPAN
           MOVE    SPA-MUID-BEDSURYOYO TO  DASCF-BEDSURYOYO
           MOVE    SPA-MUID-BEDSUSEISIN
                                       TO  DASCF-BEDSUSEISIN
           MOVE    SPA-MUID-BEDSUKEKKAKU
                                       TO  DASCF-BEDSUKEKKAKU
           MOVE    SPA-MUID-BEDSU      TO  DASCF-BEDSU
           MOVE    SPA-MUID-SETTI      TO  DASCF-SETTI
           MOVE    SPA-MUID-SYUKA      TO  DASCF-SYUKA
           MOVE    SPA-MUID-CIIKI      TO  DASCF-CHIIKI
           MOVE    SPA-MUID-DPCKBN     TO  DASCF-DPCKBN
           MOVE    SPA-MUID-ZAITAKU    TO  DASCF-ZAITAKU
           MOVE    SPA-MUID-NAI-KAIINYM
                                       TO  DASCF-KAIINYM
           MOVE    SPA-MUID-POST       TO  DASCF-POST
           MOVE    SPA-MUID-USER       TO  DASCF-USERID
           MOVE    SPA-MUID-PASSWORD   TO  DASCF-PASSWORD
           MOVE    "0"                 TO  DASCF-AGREEMENT
           MOVE    SPA-SYSYMD          TO  DASCF-CREYMD
      *
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       2120-DASCONFIG-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ������Ͻ���
      *****************************************************************
       212-SHELL-OUT-SEC           SECTION.
      *
      *    �����뵯ư����
           MOVE    SPACE           TO  SHELLTBL
           MOVE    SHELL-NAME      TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID      TO  SHELLTBL-ARG2
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  MDAS-DURATION
               MOVE    "1000"          TO  SPA-MIDCD
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       212-SHELL-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����
      *****************************************************************
       213-JYOKYO-SEC              SECTION.
      *
           PERFORM 900-JOBLOG-READ-SEC
      *
           IF      FLG-JOBLOG          =   1
               MOVE    5               TO  MDAS-DURATION
               MOVE    SPACE           TO  SPA-MIDCD
           ELSE
               MOVE    SPACE           TO  SPA-MIDCD
               MOVE    ZERO            TO  MDAS-DURATION
               IF      JOBLOG-ERRCD        =   "000"
                                           OR  "401"
                                           OR  "403"
                                           OR  "404"
                                           OR  "999"
                   STRING  "0"                 DELIMITED BY SIZE
                           JOBLOG-ERRCD        DELIMITED BY SIZE
                           INTO                SPA-MIDCD
                   END-STRING
               END-IF
           END-IF
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 220-MID2SET-SEC
           END-IF
      *
           .
       213-JYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE    "CURRENT"       TO  MCP-PUTTYPE
               MOVE    "MDAS"          TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "9000"
                    MOVE    "����ե�����ΣϣУţΤ˼��Ԥ��ޤ�����"
                                           TO  SPA-ERRMSG
                WHEN    "9001"
                    MOVE    "����ե�����ν��Ϥ˼��Ԥ��ޤ�����"
                                           TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE          TO  SPA-ERRMSG
            END-EVALUATE.
      *
           MOVE    SPACE           TO  MERR
           MOVE    SPA-ERRCD       TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MERR-ERRMSG
      *
           MOVE    "B01"           TO  MCP-WIDGET.
           MOVE    "MDAS"          TO  SPA-MOTOPG.
           MOVE    "MERR"          TO  SPA-SAKIPG.
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "MERR"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        Ʊ�դ��ʤ�
               WHEN    "CLICKED"   ALSO    "B05"
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   PERFORM 210-FILECHECK-SEC
      *        Ʊ�դ���
               WHEN    "CLICKED"   ALSO    "B08"
      *2012.10 �����٥��ʰ��������
      *        ��å��������������������������Ʊ�ͤˤʤꤹ�ޤ�
                   IF      SPA-MOTOPG      =   "M00"
                        MOVE    "9102"          TO  SPA-MIDCD
                   END-IF
      *
                   MOVE    "OK"            TO  SPA-MID1-FLG
                   PERFORM 210-BACK
      *        �����ޡ����٥��
               WHEN    "CLICKED"   ALSO    "pandatimer1"
                   PERFORM 213-JYOKYO-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
      *2012.10 �����٥��ʰ��������
           IF      SPA-MOTOPG      =   "M00"
               MOVE    "M00"           TO  SPA-SAKIPG
               MOVE    "M00"           TO  MCP-WINDOW
           ELSE
               MOVE    "M98"           TO  SPA-SAKIPG
               MOVE    "M98"           TO  MCP-WINDOW
           END-IF
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե���������å�����
      *****************************************************************
       210-FILECHECK-SEC           SECTION.
      *
           INITIALIZE                  SPA-MUID-AREA
           MOVE    ZERO            TO  FLG-OK
      *
           PERFORM 900-DASCONFIG-READ-SEC
           IF      FLG-DASCONFIG       =   ZERO
               PERFORM 3120-CONFIG-SPA-SET-SEC
           END-IF
      *
           MOVE    SPA-HOSPID      TO  SPA-MUID-HOSPID
           MOVE    SPA-SYSYMDWH    TO  SPA-MUID-SINSEIYMD
           MOVE    SPA-SYSYMD      TO  SPA-MUID-NAI-SINSEIYMD
      *
      *    �������Ƥʤ�������
           IF      FLG-OK              =   ZERO
               MOVE    SPACE           TO  SPA-MID1-FLG
               PERFORM 210-BACK
           ELSE
               PERFORM 220-MID1SET-SEC
           END-IF
      *
           .
       210-FILECHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ǡ����ӣУ����åȽ���
      *****************************************************************
       3120-CONFIG-SPA-SET-SEC         SECTION.
      *
           MOVE    DASCF-KANANAME      TO  SPA-MUID-KANANAME
           MOVE    DASCF-NAME          TO  SPA-MUID-NAME
           MOVE    DASCF-EMAIL         TO  SPA-MUID-EMAIL
           MOVE    DASCF-HOSPSBT1      TO  SPA-MUID-HOSPSBT
           MOVE    DASCF-HOSPSBT2      TO  SPA-MUID-HOSPSBT1
           MOVE    DASCF-BEDSUIPPAN    TO  SPA-MUID-BEDSUIPPAN
           MOVE    DASCF-BEDSURYOYO    TO  SPA-MUID-BEDSURYOYO
           MOVE    DASCF-BEDSUSEISIN   TO  SPA-MUID-BEDSUSEISIN
           MOVE    DASCF-BEDSUKEKKAKU  TO  SPA-MUID-BEDSUKEKKAKU
           MOVE    DASCF-BEDSU         TO  SPA-MUID-BEDSU
           MOVE    DASCF-SETTI         TO  SPA-MUID-SETTI
           MOVE    DASCF-SYUKA         TO  SPA-MUID-SYUKA
           MOVE    DASCF-CHIIKI        TO  SPA-MUID-CIIKI
           MOVE    DASCF-DPCKBN        TO  SPA-MUID-DPCKBN
           MOVE    DASCF-ZAITAKU       TO  SPA-MUID-ZAITAKU
           MOVE    DASCF-KAIINYM       TO  SPA-MUID-KAIINYM
           MOVE    DASCF-POST          TO  SPA-MUID-POST
           MOVE    DASCF-USERID        TO  SPA-MUID-USER
           MOVE    DASCF-PASSWORD      TO  SPA-MUID-PASSWORD
           IF      DASCF-SENDRESULT    =   "000"
               MOVE    1                   TO  FLG-OK
           END-IF
           .
       3120-CONFIG-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ��å�����ɽ������
      *****************************************************************
       220-MID1SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID1
      *
           INITIALIZE                  MID1
           MOVE    "1001"          TO  MID1-ID1CODE
           MOVE
           "��������ʻ��ä��ʤ��ˤ���Ͽ�������ˤ��ޤ�����"
                                   TO  MID1-ID1MSG
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID1SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ��å�����ɽ������
      *****************************************************************
       220-MID2SET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MID2
      *
           INITIALIZE                  MID2
           MOVE    SPA-MIDCD       TO  MID2-ID1CODE
      *
           EVALUATE    SPA-MIDCD
               WHEN    "1000"
                   MOVE
       "��Ͽ�����Ǥ������������ԤäƤ���ܥ���򥯥�å����Ʋ�������"
                                       TO  MID2-ID1MSG
               WHEN    "0000"
                   MOVE    "�����������Ͽ�������ˤ��ޤ�����"
                                       TO  MID2-ID1MSG
               WHEN    "0401"
                   MOVE    "���������ֹ�ޤ��ϥѥ���ɤ������Ǥ���"
                                       TO  MID2-ID1MSG
               WHEN    "0403"
                   MOVE    "���ŵ��أɣĤ������Ǥ���"
                                       TO  MID2-ID1MSG
               WHEN    "0404"
                   MOVE    "�ե����뤬���Ĥ���ޤ���"
                                       TO  MID2-ID1MSG
               WHEN    "0999"
                   MOVE    "���������Ф����Ĥ���ޤ���"
                                       TO  MID2-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "MDAS"          TO  SPA-MOTOPG
           MOVE    "MID2"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MID2"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       220-MID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ɹ�
      *****************************************************************
       900-DASCONFIG-READ-SEC          SECTION.
      *
           INITIALIZE                      DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONFIG
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONFIG
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONFIG
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DASCONFIG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����֥����ɤ߹���
      *****************************************************************
       900-JOBLOG-READ-SEC         SECTION.
      *
           INITIALIZE                  JOBLOG-REC
           MOVE    SPA-HOSPNUM     TO  JOBLOG-HOSPNUM
           MOVE    "das-profile"   TO  JOBLOG-PROGNAME
           MOVE    JOBLOG-REC      TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_joblog"    TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-JOBLOG
                   MOVE    MCPDATA-REC     TO  JOBLOG-REC
               ELSE
                   MOVE    1               TO  FLG-JOBLOG
               END-IF
           ELSE
               MOVE    1               TO  FLG-JOBLOG
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_joblog"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
            .
       900-JOBLOG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *