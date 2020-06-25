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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC12.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��̾��Ͽ
      *  ����ݡ��ͥ��̾  : �����̾�����ʣã�����
      *  ������            : 
      *  06/11/02    NACL��ƣ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/11/05  ��­�������б�
      ****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *   
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ��̾��Ͽ�Ѷ��̥ѥ�᥿
           COPY    "C01COMMON-SPA".
      *
      *    ��̾��Ͽ�Ѳ��̣ӣУ�
           COPY    "C02SCR-SPA".
           02  SPA-C02-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-C02-PTBYO-//.
           02  WRK-C02-HAISIYMD-OCC.
               03  WRK-C02-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-C02-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-C02-PTBYO-//.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
      *
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-NUM             PIC ZZZZ9.
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(04).
               05  WRK-KBNMEI      PIC X(42).
               05  WRK-KBNMEITAN   PIC X(10).                            
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-STR             PIC 9(05).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-SEIGYO          PIC X(01). 
      * 
           03  WRK-C12-BYO-ROW     PIC S9(09). 
      *
           03  WRK-CIDMSG          PIC X(80).
      *
       01  WRK-HEN-AREA.
           03  WRK-NENGO           PIC X(04).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-CONS-C12-MAX        PIC 9(03)   VALUE   200.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ��̾��Ω����
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "CERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C12    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ��ǧ���̣����
           IF      SPA-MOTOPG(1:4)     =   "CID4"
               IF      SPA-CIDCD4      =   "0101"
                   MOVE    SPACE           TO  SPA-CIDCD4
                   IF      SPA-CID4-FLG        =   "OK"
                       PERFORM 480-DELETE-SEC
      *
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    1               TO  SPA-GMN-C12-PAGE
                           PERFORM 3101-DELETE-PTBYOMEI-HENSYU-SEC
                       ELSE
                           GO  TO  300-SCREEN-EXT
                       END-IF
                   END-IF
                   MOVE    1                   TO  SPA-GMN-C12-CUR  
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    �����̾�����Խ�����     
           INITIALIZE              SPA-C12
      *
           MOVE    1               TO  SPA-GMN-C12-PAGE
      *
           PERFORM 3101-DELETE-PTBYOMEI-HENSYU-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̾�ޥ��������� �Խ�����
      *****************************************************************
       3101-DELETE-PTBYOMEI-HENSYU-SEC     SECTION.
      *                              
            INITIALIZE                 SPA-GMN-C12-TBL
                                       SPA-GMN-C12-MAX
      *
      *    �����Ǥ�ɽ�����볫�Ϸ��
           COMPUTE WRK-STR         = ( SPA-GMN-C12-PAGE  -  1 )
                                   *   WRK-CONS-C12-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C12-MAX
      *
           MOVE    "tbl_ptbyomei"  TO  WRK-MCP-TABLE
           MOVE    "key31"         TO  WRK-MCP-PATHNAME
      *
           INITIALIZE                  PTBYOMEI-REC
      *****MOVE    SPA-HOSPID      TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID
                                   TO  PTBYO-PTID
           MOVE    SPA-HOSPNUM     TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL ( IDX             >=  WRK-CONS-C12-MAX )
                   OR    ( FLG-PTBYOMEI    =   1                )
      *
      *        �����Ǥ�ɽ������ǡ������ɤ���
               ADD     1               TO  IDX2
               IF      IDX2            <   WRK-STR
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX
      *
                   MOVE    IDX2            TO  SPA-GMN-C12-TBANGO  (IDX)
                   MOVE    PTBYO-SRYKA     TO  WRK-KBN
                   PERFORM 5001-SRYKA-HENSYU-SEC
                   MOVE    WRK-KBNMEI      TO  SPA-GMN-C12-TSRYKA  (IDX)
                   INITIALIZE              ORCSMKBYOMEIAREA
                   MOVE    PTBYO-BYOMEI
                                       TO  SMKBYO-BYOMEI
                   MOVE    PTBYO-HOSOKU-COMMENT
                                       TO  SMKBYO-HOSOKU-COMMENT
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
                   MOVE    SMKBYO-BYOMEI
                                       TO  SPA-GMN-C12-TBYOMEI (IDX)
                   MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-C12-TSRYYMD (IDX)
                   IF      PTBYO-TENKIYMD  NOT =   SPACE
                       MOVE    PTBYO-TENKIYMD  TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO
                                           SPA-GMN-C12-TTENKIYMD(IDX)
                   END-IF
                   MOVE    PTBYO-TENKIKBN  TO  WRK-KBN
                   PERFORM 5001-TENKIKBN-HENSYU-SEC
                   MOVE    WRK-KBNMEI      TO
                                           SPA-GMN-C12-TTENKIKBN (IDX)
                   MOVE    PTBYO-UPYMD     TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-C12-TDLTYMD (IDX)
                   MOVE    PTBYO-DLT-OPID  TO
                                           SPA-GMN-C12-TDLT-OPID (IDX)
                   MOVE    IDX             TO  SPA-GMN-C12-MAX
               END-IF
      *
               MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-C12-MAX )
           AND   ( FLG-PTBYOMEI        =   ZERO             )
               MOVE    1                   TO  SPA-GMN-C12-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C12-NEXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-C12-CUR     
           .
       3101-DELETE-PTBYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE           TO  SPA-C01KYOUTU 
                   MOVE    SPA-C01KYOUTU   TO  SPA-WORK
                   PERFORM 210-BACK
      *    ����
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    ����
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *    �������
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-DELETE-GID-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C12"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C12-PAGE    =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C12-PAGE    =   SPA-GMN-C12-PAGE    -   1
      *
           PERFORM 3101-DELETE-PTBYOMEI-HENSYU-SEC
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C12-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C12-PAGE
      *
      *
           PERFORM 3101-DELETE-PTBYOMEI-HENSYU-SEC
      *
           MOVE    1                   TO  SPA-GMN-C12-CUR     
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������ɥ�å���������
      *****************************************************************
       480-DELETE-GID-SEC          SECTION.
      *
           IF      SPA-GMN-C12-MAX >   ZERO
               MOVE    "0101"              TO  SPA-CIDCD4
           ELSE
               MOVE    1                   TO  SPA-GMN-C12-CUR
           END-IF
          .
       480-DELETE-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       480-DELETE-SEC              SECTION.
      *
           INITIALIZE                      PTBYO-KEY
      *****MOVE    SPA-HOSPID          TO  PTBYO-HOSPID
           MOVE    SPA-NAI-C02-PTID    TO  PTBYO-PTID
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "del31"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "PTBYOMEI-KEY =>" PTBYO-KEY 
           END-IF
          .
       480-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų��Խ�����
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-BSRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C02-BSRYKAL (IDY)
                       MOVE    SPA-GMN-C02-BSRYKAMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-BSRYKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ž����ʬ�Խ�����
      *****************************************************************
       5001-TENKIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN  =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-C02-TENKIKBN-MAX
                   IF      WRK-KBN (1:1)
                                      =   SPA-GMN-C02-TENKIKBNL (IDY)
                       MOVE    SPA-GMN-C02-TENKIKBNMEIL (IDY)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C02-TENKIKBN-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5001-TENKIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    SPACE               TO  WRK-NENGO
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD2-G  TO  WRK-NENGO
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD4      NOT =   SPACE
               PERFORM 520-CIDSET4-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C12"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
           INITIALIZE                  C12
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C12-MAX
               MOVE    SPA-GMN-C12-TBANGO    (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C12-TBANGO    (IDX)
               MOVE    SPA-GMN-C12-TBYOMEI    (IDX)
                                           TO  C12-TBYOMEI   (IDX)
               MOVE    SPA-GMN-C12-TSRYKA     (IDX)
                                           TO  C12-TSRYKA    (IDX)
               MOVE    SPA-GMN-C12-TSRYYMD    (IDX)
                                           TO  C12-TSRYYMD   (IDX)
               MOVE    SPA-GMN-C12-TTENKIKBN  (IDX)
                                           TO  C12-TTENKIKBN (IDX)
               MOVE    SPA-GMN-C12-TTENKIYMD (IDX)
                                           TO  C12-TTENKIYMD (IDX)
               MOVE    SPA-GMN-C12-TDLTYMD    (IDX)
                                           TO  C12-TDLTYMD   (IDX)
               MOVE    SPA-GMN-C12-TDLT-OPID  (IDX)
                                           TO  C12-TDLT-OPID (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C12-MAX     TO  C12-BYO-COUNT
           MOVE    ZERO                TO  C12-BYO-ROW
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-C12-CUR
             WHEN    1
                     MOVE  "B01"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "��������ǥ��顼�ˤʤ�ޤ���"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  CERR
           MOVE    SPA-ERRCD            TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  CERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C12"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ����ɽ������
      *****************************************************************
       520-CIDSET4-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID4-FLG
      *
      *
           MOVE    SPACE               TO  CID4
           INITIALIZE                      CID4
           MOVE    SPA-CIDCD4          TO  CID4-ID1CODE
           EVALUATE    SPA-CIDCD4
               WHEN    "0101"
                   MOVE    "���٤Ƥκ����̾�������ޤ���"
                                           TO  CID4-ID1MSG
           END-EVALUATE
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C12"               TO  SPA-MOTOPG
           MOVE    "CID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID4"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET4-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̾�ޥ������ɹ�
      *****************************************************************
       900-PTBYO-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.