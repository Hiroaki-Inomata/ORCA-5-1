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
       PROGRAM-ID.             ORCR0940.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�������ϫ�ҡ������ա�
      *  ����ݡ��ͥ��̾  : ���̳��ϫ�Ҽ��������ٽ�ե��������
      *                      Ŧ�����¤��ؤ���ϫ�ҡ������ա�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/06/01    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  ��߽����ɲ�
      *  01.00.01    NACL-ƣ��    04/10/13  ��߽������ɲ�
      *  01.00.02    NACL-����    05/05/26  ��ѡ����ε��ܽ��ѹ��б�
      *  01.00.03    NACL-����    05/08/31  ���ֹ��Խ�����
      *  01.00.04    NACL-����    05.09.06  �����ս����ͼ��б�
      *  01.00.05    NACL-����    05/11/01  �ʸ��������������б�
      *  03.01.01    NACL-����    06/07/14  ���Ŷ�ʬ�֤������ե饰���Խ�
      *
      *  03.05.01    NACL-����    07/06/01  ���롼�׿����б�
      *  03.05.02    NACL-ƣ��    07/06/01  realtime preview �����ɲ�
      *  04.02.01    NACL-����    08/03/24  ʿ������ǯ��������б�
      *                                     �ʿ��Ŷ�ʬ���������������б���
      *
      *  04.03.01    NACL-����    08/07/09  �����ε��ܽ��ѹ������ս��
      *  04.04.01    NACL-����    10/03/29  �����б�
      *                                     �ʲ������ǣ����ܰʹ߸����Խ���
      *
      *  04.05.01    NACL-����    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *  04.05.02    NACL-����    10/01/25  �����б�
      *
      *  04.07.01    NACL-����    13/12/03  �����ա�ϫ�ҽ��ˤΥ쥻�ץ��ͼ�
      *                                     �ɲ��б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *  04.08.03    NACL-����    16/08/23  �������б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE86-FILE     ASSIGN  RECE86PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE86-KEY
                                   FILE    STATUS  IS  STS-RECE86.
           SELECT  RECE87-FILE    ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
           SELECT  RECE87X-FILE   ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE87X-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE86-FILE.
       01  RECE86-REC. 
           COPY    "CPRCF086.INC".
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
      *
       FD  RECE87X-FILE.
       01  RECE87X-REC. 
           COPY    "CPRCF087.INC"  REPLACING   //RECE87//
                                   BY          //RECE87X//.
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
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
           03  STS-RECE86                              PIC X(02).
           03  STS-RECE87                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
           03  CNT-IN-CANCEL                           PIC 9(06).
           03  CNT-OUT                                 PIC 9(06).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM                      PIC  X(06).
               05  WRK-PARA-TERMID                     PIC  X(16).
               05  WRK-PARA-SYSYMD                     PIC  X(08).
               05  WRK-PARA-RECEKBN                    PIC  X(01).
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN                    PIC  X(01).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
           03  WRK-RECEERR             PIC X(200). 
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
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *     
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
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
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
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
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       RECEERR
      *
           MOVE    ZERO                TO  FLG-END
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE86PARA
           INITIALIZE                      RECE86PARA
           MOVE    "RECE860"           TO  RECE86PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE86PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE86PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE87PARA
           INITIALIZE                      RECE87PARA
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE86PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE86PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE87PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    "ORCR0940"      TO  JOB-PGID
               MOVE    "Ŧ�����¤��ؤ���ϫ�ҡ������ա�"
                                           TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF 
      *
           OPEN    OUTPUT              RECE87X-FILE
           CLOSE                       RECE87X-FILE
      *
           OPEN    INPUT               RECE86-FILE
           OPEN    I-O                 RECE87-FILE
      *
           IF      FLG-END             =   ZERO
               PERFORM    900-RECE86-READ-SEC
           END-IF
      *
          .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM     210-HENSYU-SEC
      *
           PERFORM     900-RECE86-READ-SEC
      *
      *    ��������������
           IF      CNT-IN-CANCEL       =   50
               MOVE    ZERO                TO  CNT-IN-CANCEL
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       210-HENSYU-SEC                   SECTION.
      *
           INITIALIZE                           RECE87-REC
           MOVE    RECE86-HOSPNUM       TO      RECE87-HOSPNUM
           MOVE    RECE86-SRYYM         TO      RECE87-SRYYM
           MOVE    RECE86-NYUGAIKBN     TO      RECE87-NYUGAIKBN
           MOVE    RECE86-PTID          TO      RECE87-PTID
           MOVE    RECE86-RECEKA        TO      RECE87-RECEKA
           MOVE    RECE86-HKNID         TO      RECE87-HKNID
           MOVE    RECE86-TEKSTYMD      TO      RECE87-TEKSTYMD
           MOVE    RECE86-ZAINUM        TO      RECE87-ZAINUM
           IF      RECE86-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE87-ZAINUM
           END-IF
           IF      RECE86-SRYKBN        =       "60" OR "64"
              IF   RECE86-KNSJISKBN     =       2
                   MOVE    5            TO      RECE87-KNSJISKBN
              ELSE
                  IF   (RECE86-CDKBN-KBNNUM >=        0)   AND
                       (RECE86-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   (RECE86-CDKBN-KBNNUM >=      400)   AND
                       (RECE86-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   (RECE86-CDKBN-KBNNUM >=      200)   AND
                       (RECE86-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE87-KNSJISKBN
                  END-IF
                  IF   RECE86-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE87-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE86-SAFLG   NOT =  SPACE)
              OR   (RECE86-ZAIKBN  NOT =  ZERO )
                   MOVE    9            TO      RECE87-KNSJISKBN
              END-IF
           END-IF     
      ***  MOVE    RECE86-KNSJISKBN     TO      RECE87-KNSJISKBN
           MOVE    RECE86-LINENUM       TO      RECE87-LINENUM
           MOVE    RECE86-ZAIMEISAISU   TO      RECE87-ZAIMEISAISU
           MOVE    RECE86-PTNUM         TO      RECE87-PTNUM
           MOVE    RECE86-SRYKA         TO      RECE87-SRYKA
           IF     (RECE86-SRYKBN          =  "99" )
           AND    (RECE86-SRYSYUKBN   NOT =  "991")
             IF     (WRK-PARA-RECEKBN       =  "4")
             AND    (WRK-PARA-JIBAIPRTKBN   =  "2")
               MOVE  "01"               TO      RECE87-SRYKBN
             ELSE
               MOVE  SPACE              TO      RECE87-SRYKBN
             END-IF
           ELSE
               MOVE  RECE86-SRYKBN      TO      RECE87-SRYKBN
           END-IF
           MOVE    RECE86-ZAISKBKBN     TO      RECE87-ZAISKBKBN
           MOVE    RECE86-ZAISKBBAN     TO      RECE87-ZAISKBBAN
           MOVE    RECE86-TENTEKIKBN    TO      RECE87-TENTEKIKBN
           MOVE    RECE86-TENTEKIBAN    TO      RECE87-TENTEKIBAN
           MOVE    RECE86-ASTER         TO      RECE87-ASTER 
           MOVE    RECE86-TEKIYO        TO      RECE87-TEKIYO
           IF      RECE86-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE86-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE86-ZAITEN   TO  RECE87-ZAITEN
               END-IF
               MOVE   RECE86-CDKBN-SYO     TO  RECE87-CDKBN-SYO
               MOVE   RECE86-CDKBN-BU      TO  RECE87-CDKBN-BU
               MOVE   RECE86-CDKBN-KBN     TO  RECE87-CDKBN-KBN
               MOVE   RECE86-CDKBN-KBNNUM  TO  RECE87-CDKBN-KBNNUM
               MOVE   RECE86-CDKBN-KBNNUM-EDA
                                           TO  RECE87-CDKBN-KBNNUM-EDA
               MOVE   RECE86-CDKBN-KOUBAN  TO  RECE87-CDKBN-KOUBAN
               MOVE   RECE86-KOUHYOJUNNUM  TO  RECE87-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE86-SRYSYUKBN   TO   RECE87-SRYSYUKBN
           MOVE   RECE86-DAY         TO   RECE87-DAY
      *
           MOVE   RECE86-OKIKAEFLG   TO   RECE87-OKIKAEFLG
      *
           MOVE   RECE86-ZAIKBN      TO   RECE87-ZAIKBN
      *
           MOVE   RECE86-DAY2        TO   RECE87-DAY2
      *
           MOVE   RECE86-JIHOKBN     TO   RECE87-JIHOKBN
      *
           MOVE   RECE86-TEKIYO2-INF TO   RECE87-TEKIYO2-INF
      *
           IF     RECE86-SRYKBN  NOT =  "95" AND "96"
               PERFORM     2101-RECE87-WRITE-SEC
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ������������ʣƣ���������
      *****************************************************************
       2101-RECE87-WRITE-SEC                SECTION.
      *
           WRITE   RECE87-REC
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE87-WRITE-EXT.
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
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
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
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
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
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE86-FILE
                   RECE87-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ���ƥå״�����λ����
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE86-IN  :" CNT-IN   
           DISPLAY "RECE87-OUT :" CNT-OUT   
           DISPLAY "*** ORCR0940 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��֥ե�����ңţ��Ľ���
      *****************************************************************
       900-RECE86-READ-SEC            SECTION.
      *
           READ    RECE86-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE86  PTNUM=" RECE86-PTNUM
      *
           END-READ
           .
       900-RECE86-READ-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.