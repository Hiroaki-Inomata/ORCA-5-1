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
       PROGRAM-ID.             ORCR0470.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץȡ�ϫ�ҡ������ա�
      *  ����ݡ��ͥ��̾  : ���̳��ϫ�Ҽ��������ٽ�ե��������
      *                      Ŧ�����¤��ؤ���ϫ�ҡ������ա�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC�ݾ�Ʀ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.09.20  �����ΤȤ���ɽ���ֹ���Խ����ʤ�
      *                                     �����ΤȤ�ZAITEN���Խ�����
      *  01.00.02    NACL-����    02.11.26  ���ǥ����ɤο��Ŷ�ʬ�Խ�
      *  01.00.03    NACL-����    03.03.03  ���ֹ�彤���б�
      *  01.00.04    NACL-����    03.03.06  ��ƿǤΥ������Խ��б�
      *  ��߽����ɲ�
      *  01.01.01    NACL-ƣ��    04/10/13  ��߽������ɲ�
      *  01.01.02    NACL-����    05/05/26  ��ѡ����ε��ܽ��ѹ��б�
      *  01.01.03    NACL-����    05/08/31  ���ֹ��Խ�����
      *  01.01.04    NACL-����    05.09.06  �����ս����ͼ��б�
      *  01.01.05    NACL-����    05/11/01  �ʸ��������������б�
      *  03.01.01    NACL-����    06/07/14  ���Ŷ�ʬ�֤������ե饰���Խ�
      *
      *  03.05.01    NACL-����    07/05/25  ���롼�׿����б�
      *  03.05.02    NACL-ƣ��    07/06/01  realtime preview �����ɲ�
      *  04.02.01    NACL-����    08/03/24  ʿ������ǯ��������б�
      *                                     �ʿ��Ŷ�ʬ���������������б���
      *  04.02.02    NACL-����    08/04/21  ��������¾���ܽ��б�
      *
      *  04.03.01    NACL-����    08/07/09  �����ε��ܽ��ѹ������ս��
      *  04.04.01    NACL-����    10/03/29  �����б�
      *                                     �ʲ������ǣ����ܰʹ߸����Խ���
      *
      *  04.05.01    NACL-����    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *  04.05.02    NACL-����    10/01/21  �����б�
      *
      *  04.07.01    NACL-����    13/12/03  �����ա�ϫ�ҽ��ˤΥ쥻�ץ��ͼ�
      *                                     �ɲ��б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
           SELECT  RECE47-FILE    ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
           SELECT  RECE47X-FILE   ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE47X-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE46-FILE.
       01  RECE46-REC. 
           COPY    "CPRCF046.INC".
      *
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
      *
       FD  RECE47X-FILE.
       01  RECE47X-REC. 
           COPY    "CPRCF047.INC"  REPLACING   //RECE47//
                                   BY          //RECE47X//.
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE46//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE46                              PIC X(02).
           03  STS-RECE47                              PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-SAISHIN                             PIC 9(01).
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
                                       FLG-AREA
                                       SPA-AREA
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE46PARA
           INITIALIZE                      RECE46PARA
           MOVE    "RECE460"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE46PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE47PARA
           INITIALIZE                      RECE47PARA
           MOVE    "RECE470"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE46PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE47PARA
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 100-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0470"      TO  JOB-PGID
               MOVE    "Ŧ�����¤��ؤ���ϫ�ҡ������ա�"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE47X-FILE
           CLOSE                       RECE47X-FILE
      *
           OPEN    INPUT               RECE46-FILE
           OPEN    I-O                 RECE47-FILE
      *
           PERFORM    900-RECE46-READ-SEC
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
           PERFORM     900-RECE46-READ-SEC
      *
      *    ��������������
           IF      CNT-IN-CANCEL   =   50
               MOVE    ZERO            TO  CNT-IN-CANCEL
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
           INITIALIZE                           RECE47-REC
           MOVE    RECE46-HOSPNUM       TO      RECE47-HOSPNUM
           MOVE    RECE46-SRYYM         TO      RECE47-SRYYM
           MOVE    RECE46-NYUGAIKBN     TO      RECE47-NYUGAIKBN
           MOVE    RECE46-PTID          TO      RECE47-PTID
           MOVE    RECE46-RECEKA        TO      RECE47-RECEKA
           MOVE    RECE46-HKNID         TO      RECE47-HKNID
           MOVE    RECE46-TEKSTYMD      TO      RECE47-TEKSTYMD
           MOVE    RECE46-ZAINUM        TO      RECE47-ZAINUM
           IF      RECE46-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE47-ZAINUM
           END-IF
           IF      RECE46-SRYKBN        =       "60" OR "64"
              IF   RECE46-KNSJISKBN     =       2
                   MOVE    5            TO      RECE47-KNSJISKBN
              ELSE
                  IF   (RECE46-CDKBN-KBNNUM >=        0)   AND
                       (RECE46-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   (RECE46-CDKBN-KBNNUM >=      400)   AND
                       (RECE46-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   (RECE46-CDKBN-KBNNUM >=      200)   AND
                       (RECE46-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE47-KNSJISKBN
                  END-IF
                  IF   RECE46-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE47-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE46-SAFLG   NOT =   SPACE )
              OR   (RECE46-ZAIKBN      =   1 OR 2)
                   MOVE    9            TO      RECE47-KNSJISKBN
              END-IF
           END-IF     
      ***  MOVE    RECE46-KNSJISKBN     TO      RECE47-KNSJISKBN
           MOVE    RECE46-LINENUM       TO      RECE47-LINENUM
           MOVE    RECE46-ZAIMEISAISU   TO      RECE47-ZAIMEISAISU
           MOVE    RECE46-PTNUM         TO      RECE47-PTNUM
           MOVE    RECE46-SRYKA         TO      RECE47-SRYKA
           IF     (RECE46-SRYKBN          =  "99" )
           AND    (RECE46-SRYSYUKBN   NOT =  "991")
             IF     (WRK-PARA-RECEKBN       =  "4")
             AND    (WRK-PARA-JIBAIPRTKBN   =  "2")
               MOVE  "01"               TO      RECE47-SRYKBN
             ELSE
               MOVE  SPACE              TO      RECE47-SRYKBN
             END-IF
           ELSE
               MOVE  RECE46-SRYKBN      TO      RECE47-SRYKBN
           END-IF
           MOVE    RECE46-ASTER         TO      RECE47-ASTER 
           MOVE    RECE46-TEKIYO        TO      RECE47-TEKIYO
           IF      RECE46-SRYKBN        =       "12"
               IF      RECE46-ASTER         =       "��"
                   IF      RECE46-TEKIYO(1:24)  =
                                        "�ƿǡʿ��Ž�ˡʣ����ܡ�"
                       MOVE   1             TO      FLG-SAISHIN
                   END-IF
               END-IF
               IF      RECE46-TEKIYO(49:2)  =   SPACE
                   CONTINUE
               ELSE     
                  IF      FLG-SAISHIN          =      1
                      MOVE  ZERO               TO   FLG-SAISHIN
                  END-IF
               END-IF
           END-IF     
           IF      RECE46-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE46-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE46-ZAITEN   TO  RECE47-ZAITEN
               END-IF
               MOVE   RECE46-CDKBN-SYO     TO  RECE47-CDKBN-SYO
               MOVE   RECE46-CDKBN-BU      TO  RECE47-CDKBN-BU
               MOVE   RECE46-CDKBN-KBN     TO  RECE47-CDKBN-KBN
               MOVE   RECE46-CDKBN-KBNNUM  TO  RECE47-CDKBN-KBNNUM
               MOVE   RECE46-CDKBN-KBNNUM-EDA
                                           TO  RECE47-CDKBN-KBNNUM-EDA
               MOVE   RECE46-CDKBN-KOUBAN  TO  RECE47-CDKBN-KOUBAN
               MOVE   RECE46-KOUHYOJUNNUM  TO  RECE47-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE46-SRYSYUKBN   TO   RECE47-SRYSYUKBN
           MOVE   RECE46-DAY         TO   RECE47-DAY
      *
           MOVE   RECE46-OKIKAEFLG   TO   RECE47-OKIKAEFLG
      *
           MOVE   RECE46-SYOHOUFLG   TO   RECE47-SYOHOUFLG
      *
           MOVE   RECE46-ZAIKBN      TO   RECE47-ZAIKBN
      *
           MOVE   RECE46-DAY2        TO   RECE47-DAY2
      *
           MOVE   RECE46-JIHOKBN     TO   RECE47-JIHOKBN
      *
           MOVE   RECE46-TEKIYO2-INF TO   RECE47-TEKIYO2-INF
      *
           IF     RECE46-SRYKBN  NOT =  "95" AND "96"
               PERFORM     2101-RECE47-WRITE-SEC
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ������������ʣƣ���������
      *****************************************************************
       2101-RECE47-WRITE-SEC                SECTION.
      *
           WRITE   RECE47-REC
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE47-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE46-FILE
                   RECE47-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ���ƥå״�����λ����
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE46-IN   :" CNT-IN   
           DISPLAY "RECE47-OUT :" CNT-OUT   
           DISPLAY "*** ORCR0470 END ***"
           .
       300-TERM-EXT.
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
      *            ����ִ������Ͻ���
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
      *    ��֥ե�����ңţ��Ľ���
      *****************************************************************
       900-RECE46-READ-SEC            SECTION.
      *
           READ    RECE46-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE46  PTNUM=" RECE46-PTNUM
      *
           END-READ
           .
       900-RECE46-READ-EXT.
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