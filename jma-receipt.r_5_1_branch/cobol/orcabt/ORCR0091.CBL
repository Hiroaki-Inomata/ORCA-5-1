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
       PROGRAM-ID.             ORCR0091.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC�ݾ�Ʀ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *                                     KEY��Ĺ������Τǰ��ŵ��أɣġ�
      *                                     ����ǯ�����
      *  01.00.02    MCC-��Ʀ��   02.01.09  ���Ŷ�ʬ�᣹��(��������)�λ�
      *                                     Ŭ�������Ƭ�˰�������褦�ѹ�
      *  01.00.03    MCC-��Ʀ��   02.02.20  ���Ŷ�ʬ�ᣲ��(����)�λ�
      *                                     ���Ͻ�ǰ�������褦�˽���
      *  01.00.04    MCC-��Ʀ��   02.02.22  ���Ŷ�ʬ�᣹���ȣ���(����)�λ�
      *                                     Ŭ����ΰ������ʤ�
      *  01.00.05    MCC-��Ʀ��   02.03.19  ���Ŷ�ʬ�ᣱ����Ŧ�פΰ����о�
      *                                     �˽�����Ʊ���ƿ��б���
      *  01.00.06    MCC-��Ʀ��   02.04.20  ���Ŷ�ʬ�᣸����Ŧ����������
      *                                     �ѹ�
      *  01.00.07    NACL-ƣ��    02.09.04  �����ΤȤ���ɽ���ֹ���Խ����ʤ�
      *                                     �����ΤȤ�ZAITEN���Խ�����
      *  01.00.08    NACL-ƣ��    02.09.06  ��ǤΤȤ��Խ�����
      *  01.00.09    NACL-��Ʀ��  02.09.28  ������Ŧ������������
      *  01.00.10    NACL-ƣ��    02.11.07  ���ŵ��أɣġ�����ǯ����ɲ�
      *  01.00.11    NACL-����    02.11.26  ���ǥ����ɤο��Ŷ�ʬ�Խ�
      *  01.00.12    NACL-ƣ��    03.03.12  �����ʬ���ɲ�
      *  01.00.13    NACL-����    03.03.03  ���ֹ�彤���б�
      *  01.00.14    NACL-����    03.03.06  ��ƿǤΥ������Խ��б�
      *  01.00.15    NACL-����    03.11.26  ����쥻�ץȤμ���ʬ
      *                                     Ŧ���󵭺��б�
      *  ��߽����ɲ�
      *  01.01.01    NACL-ƣ��    04/09/28  ��߽������ɲ�
      *  01.01.02    NACL-����    05/01/06  ����쥻�ץȤν��Ͼ�､��
      *  01.01.03    NACL-����    05/02/28  �ʥ��˵����б�
      *
      *  02.06.01    NACL-����    05/05/26  ��ѡ����ε��ܽ��ѹ��б�
      *  02.06.02    NACL-����    05/05/30  realtime preview �����ɲ�
      *  02.06.03    NACL-����    05/08/31  ���ֹ��Խ�����
      *  02.06.04    NACL-����    05/11/01  �ʸ��������������б�
      *  02.09.01    NACL-����    06/04/13  ��������ǯ��������б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.02.01    NACL-����    08/03/24  ʿ������ǯ��������б�
      *                                     �ʿ��Ŷ�ʬ���������������б���
      *  04.02.02    NACL-����    08/04/21  ��������¾���ܽ��б�
      *
      *  04.03.01    NACL-����    08/07/08  �Ƕ裱���Υ����ȤΤߤκ�
      *                                     �Խ��б�
      *  04.03.02    NACL-����    08/07/09  �����ε��ܽ��ѹ������ս��
      *  04.03.03    NACL-����    08/07/14  �����������ʱ������˵����б�
      *  04.04.01    NACL-����    10/03/29  �����б�
      *                                     �ʲ������ǣ����ܰʹ߸����Խ���
      *
      *  04.05.01    NACL-����    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *  04.05.02    NACL-����    10/01/12  �����б�
      *  04.05.03    NACL-����    11/06/20  ����ʬΥ��ʬ���ڤ��Խ�����
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-����    12/10/15  ���Ź԰����Ϥ��������ȤΤߤκޤ�
      *                                     �쥻�ץȵ����б�
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
           SELECT  RECE06-FILE     ASSIGN  RECE06PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE06-KEY
                                   FILE    STATUS  IS  STS-RECE06.
           SELECT  RECE061-FILE    ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE061-KEY
                                   FILE    STATUS  IS  STS-RECE061.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE06-FILE.
       01  RECE06-REC. 
           COPY    "CPRCF006.INC".
      *
       FD  RECE061-FILE.
       01  RECE061-REC. 
           COPY    "CPRCF0061.INC".
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE06PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE061PARA//.
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    �ե�����̾����
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECE06                              PIC X(02).
           03  STS-RECE061                             PIC X(02).
           03  STS-RECEERR                             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  SAISHIN-FLG                             PIC X(01).
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
               05  WRK-PARA-JOBID                      PIC  9(07).
               05  WRK-PARA-SHELLID                    PIC  X(08).
           03  WRK-PARA-HOSPNUM                        PIC  9(02).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    ���顼�ե�����̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME���BASENAME���ϰ���
       01  WRK-SGETTEMP-ST         PIC 9(05).
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
           COPY    "CPORCMCP.INC".
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
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE060"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE061"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE06PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE061PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        �ģ¥����ץ����
               PERFORM 100-DBOPEN-SEC
      *
      *        ���ƥå״������Ͻ���
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0091"      TO  JOB-PGID
               MOVE    "Ŧ�����Խ�"    TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        ��������������
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE061-FILE
           CLOSE                       RECE061-FILE
      *
           OPEN    INPUT               RECE06-FILE
           IF      STS-RECE06          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE06PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE06          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE061-FILE
      *
           PERFORM    900-RECE06-READ-SEC
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
           PERFORM     900-RECE06-READ-SEC
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
           INITIALIZE                           RECE061-REC
grpsys     MOVE    RECE06-KEY1          TO      RECE061-KEY1
           MOVE    RECE06-KOHBUNRI      TO      RECE061-KOHBUNRI
           MOVE    RECE06-ZAINUM        TO      RECE061-ZAINUM
           IF      RECE06-ZAINUM       =   99999981  OR  99999982  OR
                                           99999983  OR  99999984  OR
                                           99999985
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-ZAINUM       =   99999999
                 MOVE     ZERO          TO      RECE061-ZAINUM
           END-IF
           IF      RECE06-SRYKBN        =       "60" OR "64"
              IF   RECE06-KNSJISKBN     =       2
                   MOVE    5            TO      RECE061-KNSJISKBN
              ELSE
                  IF   (RECE06-CDKBN-KBNNUM >=        0)   AND
                       (RECE06-CDKBN-KBNNUM <=      199)
                          MOVE   1      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      400)   AND
                       (RECE06-CDKBN-KBNNUM <=      499)
                          MOVE   2      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   (RECE06-CDKBN-KBNNUM >=      200)   AND
                       (RECE06-CDKBN-KBNNUM <=      399)
                          MOVE   3      TO      RECE061-KNSJISKBN
                  END-IF
                  IF   RECE06-CDKBN-KBN      =      "9"
                          MOVE   4      TO      RECE061-KNSJISKBN
                  END-IF
              END-IF
              IF   (RECE06-SAFLG   NOT =   SPACE )
              OR   (RECE06-ZAIKBN      =   1 OR 2)
                   MOVE    9            TO      RECE061-KNSJISKBN
              END-IF
           END-IF     
      ***     MOVE    RECE06-KNSJISKBN     TO      RECE061-KNSJISKBN
           MOVE    RECE06-LINENUM       TO      RECE061-LINENUM
           MOVE    RECE06-ZAIMEISAISU   TO      RECE061-ZAIMEISAISU
           MOVE    RECE06-PTNUM         TO      RECE061-PTNUM
           MOVE    RECE06-SRYKA         TO      RECE061-SRYKA
           IF     (RECE06-SRYKBN          =  "99" )
           AND    (RECE06-SRYSYUKBN   NOT =  "991")
               MOVE  SPACE              TO      RECE061-SRYKBN
           ELSE
               MOVE  RECE06-SRYKBN      TO      RECE061-SRYKBN
           END-IF
           MOVE    RECE06-ASTER         TO      RECE061-ASTER
           MOVE    RECE06-TEKIYO        TO      RECE061-TEKIYO
           IF      RECE06-SRYKBN        =       "12"
            IF      RECE06-ASTER         =       "��"
              IF   RECE06-TEKIYO(1:24)  =
                   "�ƿǡʿ��Ž�ˡʣ����ܡ�"
                   MOVE   "1"           TO      SAISHIN-FLG
              END-IF
            END-IF
            IF   RECE06-TEKIYO(49:2)  =   SPACE
                 CONTINUE
            ELSE     
              IF  SAISHIN-FLG          =      "1"
                 MOVE      SPACE        TO      SAISHIN-FLG
              END-IF
            END-IF 
           END-IF 
           IF      RECE06-SRYKBN        NOT =   "21" AND "22" AND "23"
               IF   RECE06-SRYKBN       NOT =   "40" AND "60" AND "80"
                                                     AND "64"
                   MOVE    RECE06-ZAITEN   TO  RECE061-ZAITEN
               END-IF
               MOVE   RECE06-CDKBN-SYO     TO  RECE061-CDKBN-SYO
               MOVE   RECE06-CDKBN-BU      TO  RECE061-CDKBN-BU
               MOVE   RECE06-CDKBN-KBN     TO  RECE061-CDKBN-KBN
               MOVE   RECE06-CDKBN-KBNNUM  TO  RECE061-CDKBN-KBNNUM
               MOVE   RECE06-CDKBN-KBNNUM-EDA
                                           TO  RECE061-CDKBN-KBNNUM-EDA
               MOVE   RECE06-CDKBN-KOUBAN  TO  RECE061-CDKBN-KOUBAN
               MOVE   RECE06-KOUHYOJUNNUM  TO  RECE061-KOUHYOJUNNUM
           END-IF
      *
           MOVE   RECE06-SRYSYUKBN   TO   RECE061-SRYSYUKBN
           MOVE   RECE06-DAY         TO   RECE061-DAY
      *
           MOVE   RECE06-SYOHOUFLG   TO   RECE061-SYOHOUFLG
      *
           MOVE   RECE06-ZAIKBN      TO   RECE061-ZAIKBN
      *
           MOVE   RECE06-DAY2        TO   RECE061-DAY2
      *
           MOVE   RECE06-HKNCOMBI    TO   RECE061-HKNCOMBI
      *
           MOVE   RECE06-JIHOKBN     TO   RECE061-JIHOKBN
      *
           IF     RECE06-RECESYUBETU  =  8888
              IF     RECE06-ZAITEN  NOT =  ZERO
                  PERFORM     2101-RECE061-WRITE-SEC
              ELSE
                IF   RECE06-SRYKBN  NOT =  "95" AND "96"
                     PERFORM  2101-RECE061-WRITE-SEC
                END-IF
              END-IF
           ELSE
              IF     RECE06-SRYKBN  NOT =  "95" AND "96"
                  PERFORM     2101-RECE061-WRITE-SEC
              END-IF
           END-IF
           .
       210-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ������������ʣƣ���������
      *****************************************************************
       2101-RECE061-WRITE-SEC                SECTION.
      *
           WRITE   RECE061-REC
      *
           IF      STS-RECE061         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE061PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE061         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-OUT
      *
           .
       2101-RECE061-WRITE-EXT.
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
      *    ���顼����λ����
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե����륨�顼����
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "�ե�����ϣУţΥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "�ե������ɤ߹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "�ե�����񤭹��ߥ��顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "�ե����빹�����顼��FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0091 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
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
           CLOSE   RECE06-FILE
                   RECE061-FILE
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
           DISPLAY "RECE06  CNT-IN  :" CNT-IN
           DISPLAY "RECE061 CNT-OUT :" CNT-OUT
           DISPLAY "*** ORCR0091 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ե�����̾��������
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��֥ե�����ңţ��Ľ���
      *****************************************************************
       900-RECE06-READ-SEC            SECTION.
      *
           READ    RECE06-FILE   NEXT
               AT  END
                   MOVE    1     TO    FLG-END
               NOT  AT  END
                   IF      STS-RECE06          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE06PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE06      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   ADD     1     TO    CNT-IN
                                       CNT-IN-CANCEL
      *
                   DISPLAY "RECE06  PTID=" RECE06-PTID
           END-READ
           .
       900-RECE06-READ-EXT.
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
      *    �ģ¥����ץ����
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
      *    �ģ¥�����������
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