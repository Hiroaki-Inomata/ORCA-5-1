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
       PROGRAM-ID.                 ORCGC501.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻����������
      *  ����ݡ��ͥ��̾  : ���������ʣã�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/06/15    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
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
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "C50COMMON-SPA".
      *
           COPY    "C50SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
      *
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-C50IDMSG        PIC X(80).
      *
           03  WRK-COMMENT         PIC X(2400).
      *    �����ꥹ�Ȱ���
           03  WRK-C501-BUNREI-ROW     PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
     *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    �쥻�ץȥ�����
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    ����
       01  EXAMPLE-REC.
           COPY    "CPEXAMPLE.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "C50.INC".
           COPY    "C501.INC".
           COPY    "C50ERR.INC".
           COPY    "C50ID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-C50
           MOVE    SPA-WORK        TO  SPA-C50KYOUTU
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
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C50KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C50         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "C50ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   ZERO
      *            �����Խ�
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE               TO  LNK-KYOUTU
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "C501"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *            ��ǧ���̤��
               WHEN    "C50ID1"
                   PERFORM 3003-C50ID1-SET-SEC
                   MOVE    SPACE               TO  SPA-C50IDCD
               WHEN    OTHER
                   PERFORM                     310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-C501-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     ��ǧ���̤��ν���
      *****************************************************************
       3003-C50ID1-SET-SEC                  SECTION.
      *
           EVALUATE    SPA-C50IDCD
               WHEN    "0104"
      *            ��������γ�ǧ
                   IF      SPA-C50ID1-FLG        =   "OK"
                       PERFORM 2401-COMMENT-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-C501-CUR
                   END-IF
      *
           END-EVALUATE
           .
       3003-C50ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
            INITIALIZE                  SPA-C501-AREA
      *
            MOVE    1                  TO  SPA-C501-PAGE
      *     ʸ���Խ�
            PERFORM 3101-BUNREI-HEN-SEC
      *
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ʸ���Խ�����
      *****************************************************************
       3101-BUNREI-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-C501-BUNNREI-G
           MOVE    ZERO                TO  SPA-C501-BUNREIMAX
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-C501-NEXT
           IF      SPA-C501-PAGE       =   ZERO
               MOVE    1                   TO  SPA-C501-PAGE
           END-IF
           COMPUTE IDX-S               =  (SPA-C501-PAGE  -  1 )
                                       *   100
                                       +   1
           PERFORM
                   UNTIL      (FLG-EXAMPLE        =   1 )
                           OR (SPA-C501-NEXT      =   1 )
               ADD     1                   TO  IDX2
               IF      IDX2                >=  IDX-S
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    1               TO  SPA-C501-NEXT
                   ELSE
                       MOVE    IDX             TO  SPA-C501-BUNREIMAX
                       MOVE    EXAMPLE-RENNUM  TO  SPA-C501-TRENNUM
                                                                  (IDX)
      **               MOVE    EXAMPLE-COMMENT TO  SPA-C501-TCOMMENT
      **                                                          (IDX)
                       PERFORM 31011-COMMENT-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           END-PERFORM
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  SPA-C501-SELNUM
      *
           .
       3101-BUNREI-HEN-EXT.
           EXIT.
      *H29.5
      *****************************************************************
      *    �����ȥ����å�����
      *****************************************************************
       31011-COMMENT-HEN-SEC            SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    400                 TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    EXAMPLE-COMMENT     TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-C501-TCOMMENT(IDX)
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-C501-TCOMMENT(IDX)
           END-IF
           .
      *
       31011-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    �����ꥹ�Ȱ�����¸
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-C501-BUNREI-ROW
           ELSE
               MOVE    C501-BUNREI-ROW     TO  WRK-C501-BUNREI-ROW
           END-IF
      *
      *
           MOVE    SPACE               TO  C501
           INITIALIZE                      C501
      *
      *    �����ꥹ�Ȥΰ��ֻ���
           MOVE    ZERO                TO  C501-BUNREI-ROW
           MOVE    ZERO                TO  C501-BUNREI-ROWATTR
           IF      WRK-C501-BUNREI-ROW NOT =   ZERO
               MOVE    WRK-C501-BUNREI-ROW TO  C501-BUNREI-ROW
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-C501-BUNREIMAX
               MOVE    SPA-C501-TRENNUM (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  C501-TRENNUM (IDX)
               MOVE    SPA-C501-TCOMMENT(IDX)  TO  C501-TCOMMENT(IDX)
      *
               IF      SPA-C501-SELNUM     =   SPA-C501-TRENNUM (IDX)
                   MOVE    "T"             TO  C501-BUNREI-SELECT (IDX)
                   MOVE    IDX             TO  C501-BUNREI-ROW
                   MOVE    2               TO  C501-BUNREI-ROWATTR
               ELSE
                   MOVE    "F"             TO  C501-BUNREI-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-C501-BUNREIMAX  TO  C501-BUNREI-COUNT
      *
           MOVE    SPA-C501-SELNUM     TO  C501-SELNUM
      *    ���Ǥ���
           IF      SPA-C501-NEXT        =   1
               IF      SPA-C501-PAGE        =   1
                   MOVE    "�μ���ͭ���"       TO  C501-NEXT-VALUE
               ELSE
                   MOVE    "�����ǡ�����ͭ���" TO  C501-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  C501-NEXT-STYLE
           ELSE
               IF      SPA-C501-PAGE       >   1
                   MOVE    "������ͭ���"      TO  C501-NEXT-VALUE
                   MOVE    "red"               TO  C501-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  C501-NEXT-VALUE
                   MOVE    "black"             TO  C501-NEXT-STYLE
               END-IF
           END-IF
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
      *
           EVALUATE    SPA-C501-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ���
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 240-COMMENT-DEL-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    1               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    2               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 290-BACK-PAGE-SEC
      *        ������
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 291-NEXT-PAGE-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �����ֹ�
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        ��������
               WHEN    ANY             ALSO    "BUNREILIST"
                   PERFORM 440-BUNREILIST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ����
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-C501-CUR
      *
           MOVE    C501-SELNUM         TO  SPA-C501-SELNUM
           MOVE    ZERO                TO  SPA-C501-TBL
      *
           IF      SPA-C501-SELNUM     =   ZERO
               GO      TO      420-SELNUM-EXT
           END-IF
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-C501-BUNREIMAX
              IF      SPA-C501-TRENNUM  (IDX)  =   SPA-C501-SELNUM
                  MOVE    IDX                      TO  SPA-C501-TBL
              END-IF
           END-PERFORM
           IF      SPA-C501-TBL        =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      420-SELNUM-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-C501-CUR
      *
           .
       420-SELNUM-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       440-BUNREILIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-C501-BUNREIMAX
              IF      C501-BUNREI-SELECT (IDX)   =  "T"
                  IF      SPA-C501-TRENNUM  (IDX)  =   SPA-C501-SELNUM
                      MOVE    SPA-C501-TRENNUM  (IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-C501-TRENNUM  (IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-C501-SELNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-C501-SELNUM
           END-IF
      *
           MOVE    SPA-C501-SELNUM     TO  C501-SELNUM
           PERFORM 420-SELNUM-SEC
      *
           .
       440-BUNREILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ������
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
           IF      SPA-C501-SELNUM     =   ZERO
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *
           MOVE    SPA-C501-SELNUM     TO  C501-SELNUM
           PERFORM 420-SELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *    ����̵ͭ
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-C501-TRENNUM (SPA-C501-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-EXAMPLE         =   ZERO
               IF      FLG-OK              =   1
      *            �������ɲ�
                   MOVE    SPACE               TO  WRK-COMMENT
                   STRING  SPA-GMN-COMMENT     DELIMITED   BY  SPACE
                           EXAMPLE-COMMENT     DELIMITED   BY  SPACE
                           INTO                WRK-COMMENT
                   END-STRING
      *
                   INITIALIZE                  ORCXKANACONVAREA
                   MOVE    2400                TO  KANACONV-LEN
                   MOVE    5000                TO  KANACONV-STLEN
                   MOVE    0                   TO  KANACONV-CONV-FLAG
                   MOVE    0                   TO  KANACONV-CHAR-TYPE
                   MOVE    WRK-COMMENT         TO  KANACONV-INDATA
                   CALL    "kanaconv"          USING ORCXKANACONVAREA
                   MOVE    KANACONV-OUTDATA    TO  SPA-GMN-COMMENT
                   MOVE    ZERO                TO  SPA-NAI-COMMAX
               ELSE
      *            �������ִ���
                   MOVE    EXAMPLE-COMMENT     TO  SPA-GMN-COMMENT
                   MOVE    SPA-C501-TRENNUM (SPA-C501-TBL)
                                               TO  SPA-GMN-BUNNUM
               END-IF
               PERFORM 210-BACK
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ⱥ��������
      *****************************************************************
       240-COMMENT-DEL-SEC                   SECTION.
      *
           IF      SPA-C501-TBL        =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    "0104"              TO  SPA-C50IDCD
           END-IF
      *
           .
       240-COMMENT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ⱥ������
      *****************************************************************
       2401-COMMENT-DELSYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-C501-TRENNUM (SPA-C501-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "C501 EXAMPLE DEL ERR:" MCP-RC "."
           END-IF
      *
      *     ʸ���Խ�
            PERFORM 3101-BUNREI-HEN-SEC
      *
            MOVE    1                  TO  SPA-C501-CUR
           .
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       290-BACK-PAGE-SEC               SECTION.
      *
           IF      SPA-C501-PAGE       >   1
               COMPUTE SPA-C501-PAGE       =   SPA-C501-PAGE
                                           -   1
      *        ʸ���Խ�
               PERFORM 3101-BUNREI-HEN-SEC
           ELSE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
           .
      *
       290-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       291-NEXT-PAGE-SEC               SECTION.
      *
           IF      SPA-C501-NEXT       =   1
               COMPUTE SPA-C501-PAGE       =   SPA-C501-PAGE
                                           +   1
      *        ʸ���Խ�
               PERFORM 3101-BUNREI-HEN-SEC
           ELSE
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
           .
      *
       291-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C50"               TO  SPA-SAKIPG
           MOVE    "C501"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "C50"               TO  MCP-WINDOW
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
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-C50IDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "C501"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����ֹ����ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�ݸ��ȹ礻���򥨥顼"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "�����ֹ�����򤷤Ʋ�����"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "���ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "���ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  C50ERR
           MOVE    SPA-ERRCD           TO  C50ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  C50ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C501"              TO  SPA-MOTOPG
           MOVE    "C50ERR"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C50ERR"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-C50IDCD
               WHEN    "0104"
                   MOVE    "���򤵤�Ƥ���ʸ��������ޤ���"
                                       TO  WRK-C50IDMSG
                   MOVE    "��������Ǥ�����"
                                       TO  WRK-C50IDMSG(35:)
               WHEN    OTHER
                   MOVE    SPA-C50IDCD
                                       TO  WRK-C50IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-C50ID1-FLG
      *
           MOVE    SPACE               TO  C50ID1
           INITIALIZE                      C50ID1
           MOVE    SPA-C50IDCD         TO  C50ID1-ID1CODE
           MOVE    WRK-C50IDMSG        TO  C50ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C501"              TO  SPA-MOTOPG
           MOVE    "C50ID1"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C50ID1"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɹ�
      *****************************************************************
       930-EXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  EXAMPLE-REC
               MOVE    ZERO                TO  FLG-EXAMPLE
           ELSE
               INITIALIZE                      EXAMPLE-REC
               MOVE    1                   TO  FLG-EXAMPLE
           END-IF
      *
           .
       930-EXAMPLE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *