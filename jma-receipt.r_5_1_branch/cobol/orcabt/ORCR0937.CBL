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
       PROGRAM-ID.             ORCR0937.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ������ʼ����ա�
      *  ����ݡ��ͥ��̾  : ���̳�����������ٽ�ե��������
      *                      ���Ŧ�����Խ��ʼ����աˡʿ��ͼ��ʣϣã��ͼ��ˡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/07/27    NaCl-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE86-FILE     ASSIGN  RECE86PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE86-KEY
                                   FILE    STATUS  IS  STS-RECE86.
      *
           SELECT  RECE87-FILE     ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE86-FILE.
       01  RECE86-REC.
           COPY    "CPRCF0861.INC" REPLACING  //RECE861//
                                   BY         //RECE86//.
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE86//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE87//.
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE87          PIC X(02).
           03  STS-RECE86          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE86          PIC 9(01).
           03  FLG-RECE87          PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-HENSYU2         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE87          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *    �ް����Կ�
           03  WRK-TBLMAX          PIC 9(03).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-KINGAKU         PIC 9(09)V9(02).
           03  WRK-ZAIKAISU        PIC 9(04).
      *
           03  WRK-ST              PIC 9(03).
      *
      *    �ޡ����Ź԰٥ơ��֥�
       01  TBL-ZAI-AREA.
           03  TBL-ZAI-OCC                     OCCURS 100.
               05  TBL-ZAI-NAME                PIC X(32).
               05  TBL-ZAI-KISAIFLG            PIC 9(01).
               05  TBL-ZAI-YENFLG              PIC 9(01).
               05  TBL-ZAI-TEN                 PIC 9(07).
               05  TBL-ZAI-KAISU               PIC 9(03).
      *
       01  WRK-RECE86-REC.
           COPY    "CPRCF0861.INC" REPLACING  //RECE861//
                                   BY         //WRK-RECE86//.
      *
      *    ������
       01  CONST-AREA.
      *    Ŭ�������ʸ�������������
           03  CONS-TEKIYOU2-LEN           PIC 9(02)   VALUE   32.
      *
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
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
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
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
           INITIALIZE                  RECEERR
      *
      *    �ե�����ϣУţν���
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0937"          TO  JOB-PGID
               MOVE    "���Ŧ�����Խ��ʼ����աʣϣã��ͼ��ˡ�"
                                           TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           PERFORM 990-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����ϣУţν���
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE820"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE86PARA
           INITIALIZE                      RECE86PARA
           MOVE    "RECE861"           TO  RECE86PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE86PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE86PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE87PARA
           INITIALIZE                      RECE87PARA
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE86PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE86PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE87PARA
      *
           OPEN    INPUT               RECE82-FILE
      *
           OPEN    INPUT               RECE86-FILE
      *
           OPEN    I-O                 RECE87-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO       TO     WRK-LINENUM
      *
           MOVE    ZERO       TO     WRK-ZAINUM
           MOVE    ZERO       TO     WRK-TBLMAX
                                     WRK-KINGAKU
                                     WRK-ZAIKAISU
           INITIALIZE                TBL-ZAI-AREA
      *
      *    �ѥ�᡼�������ե��������Խ�
           INITIALIZE                      RECE86-REC
           MOVE    RECE82-HOSPNUM      TO  RECE86-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE86-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE86-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE86-PTID
           MOVE    RECE82-RECEKA       TO  RECE86-RECEKA
           MOVE    RECE82-HKNID        TO  RECE86-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE86-TEKSTYMD
      *
           START   RECE86-FILE
               KEY IS    >=   RECE86-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE86
               NOT INVALID
                   PERFORM 990-RECE86-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1                )
               OR     ( FLG-RECE86           =   1                )
               OR     ( RECE82-HOSPNUM   NOT =   RECE86-HOSPNUM   )
               OR     ( RECE82-SRYYM     NOT =   RECE86-SRYYM     )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE86-NYUGAIKBN )
               OR     ( RECE82-PTID      NOT =   RECE86-PTID      )
               OR     ( RECE82-RECEKA    NOT =   RECE86-RECEKA    )
               OR     ( RECE82-HKNID     NOT =   RECE86-HKNID     )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE86-TEKSTYMD  )
      *
                   IF      RECE86-ZAINUM   NOT =   WRK-ZAINUM
                      IF      WRK-ZAINUM   NOT =   ZERO
                          PERFORM 2002-RECE87-OUT-SEC
                      END-IF
                      MOVE    RECE86-REC     TO    WRK-RECE86-REC
                      MOVE    RECE86-ZAINUM  TO    WRK-ZAINUM
                      MOVE    ZERO           TO    WRK-TBLMAX
                                                   WRK-KINGAKU
                                                   WRK-ZAIKAISU
                      INITIALIZE                   TBL-ZAI-AREA
                   END-IF
      *
                   ADD     RECE86-TEN        TO    WRK-KINGAKU
                   MOVE    RECE86-ZAIKAISU   TO    WRK-ZAIKAISU
      *
      *            �����Խ������ٷ���׻�
                   PERFORM 2003-MEISAI-KETA-HEN-SEC
      *
                   PERFORM 990-RECE86-NEXT-SEC
      *
      *            ��������������
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
           END-PERFORM
      *
           IF      FLG-END         =   ZERO
      *
               IF    WRK-ZAINUM       NOT =   ZERO
               AND   TBL-ZAI-NAME(1)  NOT =   SPACE
                     PERFORM   2002-RECE87-OUT-SEC
               END-IF
      *
               PERFORM 990-RECE82-READ-SEC
      *
      *        ��������������
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȣ������Ͻ���
      *****************************************************************
       2002-RECE87-OUT-SEC    SECTION.
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL    IDX        >   WRK-TBLMAX
                        OR     IDX        >   100
                        OR     FLG-END    =     1
      *
             ADD     1                     TO  WRK-LINENUM
             MOVE    SPACE                 TO  RECE87-REC
             INITIALIZE                        RECE87-REC
      *
             MOVE    WRK-RECE86-HOSPNUM    TO  RECE87-HOSPNUM
             MOVE    WRK-RECE86-SRYYM      TO  RECE87-SRYYM
             MOVE    WRK-RECE86-NYUGAIKBN  TO  RECE87-NYUGAIKBN
             MOVE    WRK-RECE86-PTID       TO  RECE87-PTID
             MOVE    WRK-RECE86-RECEKA     TO  RECE87-RECEKA
             MOVE    WRK-RECE86-HKNID      TO  RECE87-HKNID
             MOVE    WRK-RECE86-TEKSTYMD   TO  RECE87-TEKSTYMD
             MOVE    WRK-RECE86-SRYKBN     TO  RECE87-SRYKBN
             MOVE    SPACE                 TO  RECE87-ZAISKBKBN
             MOVE    ZERO                  TO  RECE87-ZAISKBBAN
             MOVE    ZERO                  TO  RECE87-TENTEKIKBN
             MOVE    ZERO                  TO  RECE87-TENTEKIBAN
             MOVE    ZERO                  TO  RECE87-KNSJISKBN
             MOVE    SPACE                 TO  RECE87-SRYSYUKBN
             MOVE    ZERO                  TO  RECE87-DAY
             IF    WRK-RECE86-SRYKBN  = "11"
               MOVE  ZERO                  TO  RECE87-CDKBN-SYO
               MOVE  ZERO                  TO  RECE87-CDKBN-BU
               MOVE  SPACE                 TO  RECE87-CDKBN-KBN
               MOVE  ZERO                  TO  RECE87-CDKBN-KBNNUM
               MOVE  ZERO                  TO  RECE87-CDKBN-KBNNUM-EDA
               MOVE  ZERO                  TO  RECE87-CDKBN-KOUBAN
               MOVE  ZERO                  TO  RECE87-KOUHYOJUNNUM
             ELSE
               MOVE  9                     TO  RECE87-CDKBN-SYO
               MOVE  99                    TO  RECE87-CDKBN-BU
               MOVE  "9"                   TO  RECE87-CDKBN-KBN
               MOVE  999                   TO  RECE87-CDKBN-KBNNUM
               MOVE  99                    TO  RECE87-CDKBN-KBNNUM-EDA
               MOVE  99                    TO  RECE87-CDKBN-KOUBAN
               MOVE  999999999             TO  RECE87-KOUHYOJUNNUM
             END-IF
             MOVE    WRK-RECE86-NYUTENTTLKBN
                                           TO  RECE87-NYUTENTTLKBN
             MOVE    WRK-RECE86-SRYCD1     TO  RECE87-SRYCD1
             MOVE    ZERO                  TO  RECE87-ZAITEN
             MOVE    WRK-RECE86-ZAINUM     TO  RECE87-ZAINUM
             MOVE    WRK-LINENUM           TO  RECE87-LINENUM
      *
             MOVE    WRK-TBLMAX            TO  RECE87-ZAIMEISAISU
             MOVE    WRK-RECE86-PTNUM      TO  RECE87-PTNUM
             MOVE    WRK-RECE86-SRYKA      TO  RECE87-SRYKA
             MOVE    TBL-ZAI-NAME(IDX)     TO  RECE87-TEKIYO-INF
      *
             MOVE    TBL-ZAI-KISAIFLG(IDX) TO  RECE87-TEKIYO2-KISAIFLG
             MOVE    TBL-ZAI-YENFLG(IDX)   TO  RECE87-TEKIYO2-YENFLG
             MOVE    TBL-ZAI-TEN(IDX)      TO  RECE87-TEKIYO2-TEN
             MOVE    TBL-ZAI-KAISU(IDX)    TO  RECE87-TEKIYO2-KAISU
      *
             MOVE    SPACE                 TO  RECE87-JIHOKBN
      *
             READ    RECE87-FILE
               INVALID
                   MOVE    1               TO  FLG-RECE87
               NOT INVALID
                   MOVE    ZERO            TO  FLG-RECE87
             END-READ
             IF      FLG-RECE87            =   1
                 WRITE   RECE87-REC
                 IF      STS-RECE87          NOT =   ZERO
                     DISPLAY "*** ORCR0937 RECE87 WRITE ERR  ***"
                     MOVE    1                 TO  FLG-END
                 ELSE
                     ADD     1                 TO  CNT-RECE87
                 END-IF
             ELSE
                 DISPLAY "*** ORCR0937 RECE87 KEY CYOFUKU  ***"
             END-IF
      *
           END-PERFORM
      *
           .
       2002-RECE87-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ������ٷ���׻�
      *****************************************************************
       2003-MEISAI-KETA-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  WRK-ST
      *
           MOVE    ZERO                TO  FLG-HENSYU
                                           FLG-HENSYU2
      *
           PERFORM   UNTIL   FLG-HENSYU  =   1
      *
               ADD     1               TO  WRK-TBLMAX
               IF  WRK-TBLMAX          >   100
                   DISPLAY "*** ORCR0937 INDEX[100] OVER  ***"
               END-IF
      *
               IF    WRK-TBLMAX    =    1
               AND   FLG-HENSYU2   =    ZERO
                   EVALUATE  RECE86-SRYCD1(1:5)
                     WHEN   "09591"
                       MOVE  "��"  TO  TBL-ZAI-NAME(WRK-TBLMAX)(1:2)
                     WHEN   "09592"
                       MOVE  "��"  TO  TBL-ZAI-NAME(WRK-TBLMAX)(1:2)
                     WHEN    OTHER
                       MOVE  "��"  TO  TBL-ZAI-NAME(WRK-TBLMAX)(1:2)
                   END-EVALUATE
                   MOVE      1     TO  FLG-HENSYU2
               END-IF
               MOVE  RECE86-NAME(WRK-ST:)  TO
                                     TBL-ZAI-NAME(WRK-TBLMAX)(3:)
      *
               COMPUTE  WRK-ST  =  WRK-ST  +  CONS-TEKIYOU2-LEN  - 2
      *
      *        ��ۤ��Խ����ʤ��Ȥ��ޤ���������ʸ�������뤫
               IF      RECE86-NAME (WRK-ST:)   =   SPACE
                  MOVE    1             TO  FLG-HENSYU
                  IF   (RECE86-ZAILAST     =   1   )
                  AND  (WRK-KINGAKU    NOT =   ZERO)
                  AND  (WRK-ZAIKAISU   NOT =   ZERO)
                    MOVE  1             TO  TBL-ZAI-KISAIFLG(WRK-TBLMAX)
                    MOVE  1             TO  TBL-ZAI-YENFLG(WRK-TBLMAX)
                    MOVE  WRK-KINGAKU   TO  TBL-ZAI-TEN(WRK-TBLMAX)
                    MOVE  WRK-ZAIKAISU  TO  TBL-ZAI-KAISU(WRK-TBLMAX)
                  END-IF
               ELSE
                  MOVE    "��"    TO    TBL-ZAI-NAME(WRK-TBLMAX)(31:2)
                  COMPUTE  WRK-ST  =  WRK-ST  -  2
               END-IF
           END-PERFORM
      *
           .
       2003-MEISAI-KETA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ����ִ�����λ����
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������߽���
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "��������ߤ���ޤ���"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ����ִ�����λ����
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
                       PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE82-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE86-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ���ƥå״�����λ����
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE82  IN      " CNT-RECE82
           DISPLAY "RECE87  ADDCNT  " CNT-RECE87
           DISPLAY "*** ORCR0937 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �쥻�ץȣ����ǡ����縡������
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   ADD     1           TO  CNT-RECE82
                                           CNT-IN-CANCEL
      *
                   DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                   IF    (WRK-PARA-RECEKBN       =  "4")
                   AND   (WRK-PARA-JIBAIPRTKBN   =  "2")
                       CONTINUE
                   ELSE
                       GO  TO  990-RECE82-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �쥻�ץȣ����ǡ����ɤ�
      ****************************************************************
       990-RECE86-NEXT-SEC           SECTION.
      *
           READ    RECE86-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE86
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE86
                   ADD     1         TO  CNT-IN-CANCEL
           END-READ
      *
           .
       990-RECE86-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.