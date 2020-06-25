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
       PROGRAM-ID.             ORCBDR02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ͽ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/03/02    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  04.08.01    NACL-����     14/08/21  ����ե�����ǥ��쥯�ȥ�����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-KENSU               PIC 9(05).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-DENPPRTYMD     PIC X(08).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-TUKI                PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ͽ��ޥ���
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    ͽ�󥳥��ȥޥ���
       01  YYKCOM-REC.
           COPY    "CPYYKCOM.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-DENPPRTYMD
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBDR02"      TO  JOB-PGID
           MOVE    "ͽ�������"  TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    3               TO  JOB-STOPFLG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    �ѥ�᥿�Խ�����
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᥿�Խ�����
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    ��������
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    WRK-SYMD2       TO  WRK-SYMD(3:)
           ADD     2000            TO  WRK-SYY
      *
      *    �о�ǯ���Խ�
           MOVE    WRK-PARA-DENPPRTYMD
                                   TO  WRK-SYSYMD
      *
      *    �о�ǯ������ǯ����礭����票�顼
           IF      WRK-SYSYY   >   WRK-SYY
               MOVE    "���������顼"  TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        �о�ǯ������ǯ��Ʊ�����
               IF      WRK-SYSYY   =   WRK-SYY
      *            �оݷ���߷��꾮�������
                   IF      WRK-SYSMM   <   WRK-SMM
                       COMPUTE WRK-TUKI    =   WRK-SMM -    WRK-SYSMM
      *                �оݷ���������Ǥ���Х��顼
                       IF      WRK-TUKI    <=  2
                           MOVE    "���������顼"  TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   ELSE
                       MOVE    "���������顼"  TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               ELSE
      *            �оݷ���߷����礭�����
                   IF      WRK-SYSMM   >=  WRK-SMM
                       COMPUTE WRK-TUKI    =   WRK-SYSMM   -   WRK-SMM
      *                �оݷ���������Ǥ���Х��顼
                       IF      WRK-TUKI    >=  10
                           MOVE    "���������顼"  TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-YYK-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-YYK     =   1
               PERFORM 900-YYK-DBDELETE-SEC
               PERFORM 900-YYKCOM-DBDELETE-SEC
               PERFORM 900-YYK-DBFETCH-SEC
           END-PERFORM
      *
           PERFORM 900-YYK-DBCLOSE-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-KENSU       =   ZERO 
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               STRING  "���ｪλ��"    DELIMITED BY SIZE
                       CNT-KENSU       DELIMITED BY SIZE
                       "����"        DELIMITED BY SIZE
                               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBDR02 DEL "     CNT-KENSU
           DISPLAY "*** ORCBDR02 END ***"
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    0               TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͽ���ɤ߹��߳��Ͻ���
      *****************************************************************
       900-YYK-DBSELECT-SEC        SECTION.
      *
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM     TO  YYK-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYMD
                                   TO  YYK-YYKYMD
           MOVE    YYK-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-YYK-DBFETCH-SEC
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           .
       900-YYK-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͽ���ɤ߹��߽���
      *****************************************************************
       900-YYK-DBFETCH-SEC         SECTION.
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  YYK-REC
               MOVE    ZERO            TO  FLG-YYK
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           .
       900-YYK-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͽ���ɤ߹��߽�λ����
      *****************************************************************
       900-YYK-DBCLOSE-SEC         SECTION.
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-YYK-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͽ��������
      *****************************************************************
       900-YYK-DBDELETE-SEC        SECTION.
      *
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "del"           TO  MCP-PATHNAME
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           COMPUTE CNT-KENSU   =   CNT-KENSU   +   1
      *
           .
       900-YYK-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ͽ�󥳥��Ⱥ������
      *****************************************************************
       900-YYKCOM-DBDELETE-SEC     SECTION.
      *
           INITIALIZE                  YYKCOM-REC
           MOVE    YYK-HOSPNUM     TO  YYKCOM-HOSPNUM
           MOVE    YYK-SRYNAIYO    TO  YYKCOM-SRYNAIYO
           MOVE    YYK-DRCD        TO  YYKCOM-DRCD
           MOVE    YYK-YYKYMD      TO  YYKCOM-YYKYMD
           MOVE    YYK-KEYYYKTIME  TO  YYKCOM-KEYYYKTIME
           MOVE    YYK-YYKID       TO  YYKCOM-YYKID
           MOVE    YYKCOM-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_yykcom"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-YYKCOM-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.