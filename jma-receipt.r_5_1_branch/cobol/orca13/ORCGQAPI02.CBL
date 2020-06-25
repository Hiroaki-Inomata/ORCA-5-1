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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGQAPI02.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : ���ԾȲ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  16/10/31    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
           COPY    "Q01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
      *
       01  REQ-AREA.
           03  REQ-BASEYMD             PIC X(10).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �Ȳ񥵥֣��ʥ���ܥܥå������ֵѡ�
           COPY    "CPQSUB03.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPFINDINFV3REQ.INC".
      *    API XML�񤭹����������
           COPY    "CPFINDINFV3RES.INC".
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  REQ-AREA
      *
           MOVE    SPACE           TO  SPA-Q01FREE
           INITIALIZE                  SPA-Q01FREE
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-FINDINFV3REQ
                                       XML-FINDINFV3RES
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
                                           SPA-SRYYMD
      *
      *    ���ŵ��ؼ����ֹ楻�å� 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    �ӣУ���������
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  SPA-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  SPA-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  SPA-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  SPA-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  SPA-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  SPA-ERRCD
               END-EVALUATE
      *
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML������Ф�
           PERFORM 900-XML-READ-SEC
           IF    ( SPA-ERRCD           NOT =   SPACE )
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    FINDINFREQ-REQNUM
           WHEN    "01"
      *
               PERFORM 2001-INPUT-CHK-SEC
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 2003-RESPONSE-EDIT-SEC
               END-IF
           WHEN    OTHER
               MOVE    "0091"          TO  SPA-ERRCD
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ϲ��ܥ����å�����
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BASEYMD-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ����������å�
      *****************************************************************
       2001-BASEYMD-CHK-SEC            SECTION.
      *
           IF    ( FINDINFREQ-BASEYMD  =   SPACE )
               MOVE    SPA-SYSYMD      TO  REQ-BASEYMD
           ELSE
               MOVE    FINDINFREQ-BASEYMD
                                       TO WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0210"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  REQ-BASEYMD
               END-IF
           END-IF
      *
           .
       2001-BASEYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           INITIALIZE                      QSUB03-LNK
           MOVE    SPACE               TO  QSUB03-REQUEST
           MOVE    "99999999"          TO  QSUB03-STSRYYMD
           MOVE    SPA-SYSYMD          TO  QSUB03-EDSRYYMD
      *
           MOVE    REQ-BASEYMD         TO  QSUB03-BASEYMD
           CALL    "ORCGQSUB03"    USING   QSUB03-LNK
                                           SPA-AREA
      *
      *    ���֣�����ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-CONDITION1LST-MAX )
      *
               MOVE    QSUB03-CONDITION1L (IDXL)
                                   TO  FINDINFRES-CONDITION1L (IDXL)
               MOVE    QSUB03-CONDITION1MEIL (IDXL)
                                   TO  FINDINFRES-CONDITION1MEIL (IDXL)
      *
           END-PERFORM
      *
      *    ���֣�����ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-CONDITION2LST-MAX )
      *
               MOVE    QSUB03-CONDITION2L (IDXL)
                                   TO  FINDINFRES-CONDITION2L (IDXL)
               MOVE    QSUB03-CONDITION2MEIL (IDXL)
                                   TO  FINDINFRES-CONDITION2MEIL (IDXL)
      *
           END-PERFORM
      *
      *    ���֣�����ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-CONDITION3LST-MAX )
      *
               MOVE    QSUB03-CONDITION3L (IDXL)
                                   TO  FINDINFRES-CONDITION3L (IDXL)
               MOVE    QSUB03-CONDITION3MEIL (IDXL)
                                   TO  FINDINFRES-CONDITION3MEIL (IDXL)
      *
           END-PERFORM
      *
      *    ���ȥ���ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-GENMENLST-MAX )
      *
               MOVE    QSUB03-GENMENL (IDXL)
                                   TO  FINDINFRES-GENMENL (IDXL)
               MOVE    QSUB03-GENMENMEIL (IDXL)
                                   TO  FINDINFRES-GENMENMEIL (IDXL)
      *
           END-PERFORM
      *
      *    �õ����ॳ��ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-TOKKIINFOLST-MAX )
      *
               MOVE    QSUB03-TOKKIINFOL (IDXL)
                                   TO  FINDINFRES-TOKKIINFOL (IDXL)
               MOVE    QSUB03-TOKKIINFOMEIL (IDXL)
                                   TO  FINDINFRES-TOKKIINFOMEIL (IDXL)
      *
           END-PERFORM
      *
      *    �ݸ��μ��ॳ��ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-HKNNUMLST-MAX )
      *
               MOVE    QSUB03-HKNNUML (IDXL)
                                   TO  FINDINFRES-HKNNUML (IDXL)
               MOVE    QSUB03-HKNNUMMEIL (IDXL)
                                   TO  FINDINFRES-HKNNUMMEIL (IDXL)
      *
           END-PERFORM
      *
      *    ����μ��ॳ��ܽ������
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    >   QSUB03-KOHNUMLST-MAX )
      *
               MOVE    QSUB03-KOHNUML (IDXL)
                                   TO  FINDINFRES-KOHNUML (IDXL)
               MOVE    QSUB03-KOHNUMMEIL (IDXL)
                                   TO  FINDINFRES-KOHNUMMEIL (IDXL)
               MOVE    QSUB03-KOHNUM-TYPEL (IDXL)
                                   TO  FINDINFRES-KOHNUM-TYPEL (IDXL)
      *
           END-PERFORM
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGQERRMSG"   USING
                                   SPA-AREA
                                   SPA-Q01FREE
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( QAPI02-BODY     NOT =   LOW-VALUE )
               DISPLAY "findinfv3 object is not low_value"
           END-IF
      *
           INITIALIZE                  XML-APIREQ
           MOVE    "xml_findinfv3req" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    QAPI02-BODY     TO  APIREQ-OBJECT
           MOVE    ZERO            TO  APIREQ-MODE
           MOVE    "findinfv3req"  TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC          =   ZERO  OR  1 )
               MOVE    APIREQ-PATIENTINFOREQ
                                   TO  FINDINFREQ-FINDV2REQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"       TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE       TO  QAPI02-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML�����񤭽Ф�
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    FINDINFREQ-REQNUM            TO  FINDINFRES-REQNUM
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDINFRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  FINDINFRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  FINDINFRES-API-RESULT
           ELSE
               MOVE    "E"         TO  FINDINFRES-API-RESULT
               MOVE    SPA-ERRCD   TO  FINDINFRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-Q01-ERRMSG  TO  FINDINFRES-API-RESULT-MSG
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_findinfv3res"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    FINDINFRES-FINDINFV2RES
                                       TO  APIREQ-PATIENTINFOREQ
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "findinfv3res"      TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT  TO  QAPI02-BODY
               MOVE    "application/xml"
                                      TO  QAPI02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����ཪλ�����ʥ��顼����
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   "0099" )
               MOVE    404             TO  QAPI02-HTTPSTATUS
           ELSE
               PERFORM 900-XML-WRITE-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    �أͣ��ɹ�����
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    �أͣ̽������
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.