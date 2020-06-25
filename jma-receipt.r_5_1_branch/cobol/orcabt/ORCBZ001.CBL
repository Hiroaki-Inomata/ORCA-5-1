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
       PROGRAM-ID.             ORCBZ001.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ģɼ
      *  ����ݡ��ͥ��̾  : ���������ֹ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/06/19    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03.08.22  �����ֹ�����å��ɲ�
      * 03.05.00     NACL-¿��Ǽ  07/05/24  ���롼�׿����б�
      * 04.08.00     NACL-¿��Ǽ  14/07/17  ����ե������б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ���顼�ե����� ̾���ΰ� 
      *    COPY    "CPCOMMONDAT2.INC"
      *                            REPLACING  //RECE01PARA//
      *                            BY         //RECEERR//.
      *    03  FILLER              PIC X(04)   VALUE   ".dat".
      *
           COPY    "HCMZ001.INC".
      *
      *    ����ե������б�
           COPY    "CPRECEERR.INC".
      *
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-KOUSEI              PIC 9(01).
      *
           03  FLG-ERR                 PIC 9(01).
           03  FLG-ERR2                PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(03).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-BZ001               PIC 9(05).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
      **** 03  RECEERR                 PIC X(100).
           03  WRK-PARA-PTNUM          PIC X(20).
           03  WRK-PARA-PTNUM-END      PIC X(20).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-KOUSEI              PIC X(94).
           03  WRK-KETA                PIC 9(02).
           03  WRK-KETA2               PIC 9(02).
      *
           03  WRK-KETAHEN             PIC X(04).
      *
           03  WRK-PAGE                PIC ZZZZ9.
      *
           03  WRK-IN                  PIC X(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ���ŵ��ؾ���
           COPY    "CPSK1009.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ��������
      *01  PRTKANRI-REC.
      *    COPY    "CPPRTKANRI.INC".
      *
      *    �����ѥǡ���
      *01  PRTDATA-REC.
      *    COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���������ֹ極��
           COPY    "CPORCBZ001S.INC".
      *
      *    �����Ѵ�
           COPY    "CPORCSKANACHK.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ե��������ѥ�᥿
      **   COPY    "CPORCSFDEL.INC".
      *
      *    ����ե�����̾����
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
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
      *
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           MOVE    ZERO            TO  CNT-AREA
      *
           DISPLAY "*** ORCBZ001 START ***"
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
                                       WRK-PARA-PTNUM
                                       WRK-PARA-PTNUM-END
           END-UNSTRING
      *
      *    �ӣУ����å�
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ����ե�����̾����
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBZ001"          TO  JOB-PGID
           MOVE    "���������ֹ����"  TO  JOB-SHELLMSG
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *DDD MOVE    "RECEERR"       TO  RECEERR-FILE-ID
      *    MOVE    LNK-PRTKANRI-TERMID
      *                            TO  RECEERR-TERMID
      *DDD MOVE    SPA-HOSPNUM     TO  RECEERR-HOSPNUM
      *
      *    ����ͥ��å�
           PERFORM 110-SYOKI-SET-SEC
      *
      *    �����ֹ湽������
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"          TO  SYS-1009-KANRICD
           MOVE    "*"             TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
           END-IF
      *
      *    �����ֹ湽�����å�
           PERFORM 120-KOUSEI-SET-SEC
      *
           IF      FLG-KOUSEI  =   1
               MOVE    "�����ֹ�ι���������������ޤ���"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO             100-INIT-EXT
           END-IF
      *
           IF      FLG-ERR             =   1
               MOVE    "���ϴ����ֹ�η�����㤤�ޤ���"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO             100-INIT-EXT
           END-IF
           IF      FLG-ERR2            =   1
               MOVE    "��λ�����ֹ�η�����㤤�ޤ���"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO      TO             100-INIT-EXT
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ͥ��å�
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    �����ƥ����ե��å�
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "11"            TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    ȯ����
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYSYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ湽�����å�
      *****************************************************************
       120-KOUSEI-SET-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-KOUSEI
           MOVE    ZERO            TO  FLG-KOUSEI
           MOVE    ZERO            TO  WRK-KETA
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *    ��ͳ����
           WHEN    "1"
               EVALUATE    SYS-1009-JIYKSIKBN
               WHEN    "2"
      *            ���Խ�
                   MOVE    SYS-1009-JIYKSIKETA     TO  WRK-KETA
                   PERFORM 130-KETA-HEN-SEC
      *
                   STRING  "�����ֹ桧��ͳ��������������"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "���"          DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    OTHER
                   MOVE    1               TO  FLG-KOUSEI
               END-EVALUATE
      *    ɸ�๽��
           WHEN    "2"
      *        ���Խ�
               MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-KETA
               PERFORM 130-KETA-HEN-SEC
      *
               EVALUATE    SYS-1009-HJNKSIKBN
                   ALSO    SYS-1009-HJNKSINENKBN
                   ALSO    SYS-1009-HJNKSIRENNUMKBN
               WHEN    "1"     ALSO    "1"     ALSO    "1"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣳷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡܸ����ֹ�ʣ���ˡ���ǯ��Ϣ�ֹ�����"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "1"     ALSO    "1"     ALSO    "2"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣳷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡܸ����ֹ�ʣ����" 
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "1"     ALSO    "2"     ALSO    "1"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣴷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡܸ����ֹ�ʣ���ˡ���ǯ��Ϣ�ֹ�����"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "1"     ALSO    "2"     ALSO    "2"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣴷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡܸ����ֹ�ʣ����" 
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "2"     ALSO    "1"     ALSO    "1"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣳷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡ���ǯ��Ϣ�ֹ�����"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "2"     ALSO    "1"     ALSO    "2"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣳷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "���"          DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "2"     ALSO    "2"     ALSO    "1"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣴷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡ���ǯ��Ϣ�ֹ�����"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "2"     ALSO    "2"     ALSO    "2"
                   STRING  "�����ֹ桧ɸ�๽����"
                                           DELIMITED   BY  SIZE
                           "ǯ�����񣴷�ˡ�Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "���"          DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "3"     ALSO    ANY     ALSO    ANY
                   STRING  "�����ֹ桧ɸ�๽����Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "��ˡܸ����ֹ�ʣ����"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    "4"     ALSO    ANY     ALSO    ANY
                   STRING  "�����ֹ桧ɸ�๽����Ϣ�ֹ��"
                                           DELIMITED   BY  SIZE
                           WRK-KETAHEN     DELIMITED   BY  SPACE
                           "���"          DELIMITED   BY  SIZE
                                   INTO    WRK-KOUSEI
                   END-STRING
               WHEN    OTHER
                   MOVE    1           TO  FLG-KOUSEI
               END-EVALUATE
           WHEN    OTHER
               MOVE    1           TO  FLG-KOUSEI
           END-EVALUATE
      *
      *    ��������å�
           MOVE    ZERO                TO  WRK-KETA
           MOVE    ZERO                TO  WRK-KETA2
           MOVE    ZERO                TO  FLG-ERR
           MOVE    ZERO                TO  FLG-ERR2
           EVALUATE    SYS-1009-PTNUMKSIKBN
               WHEN    "1"
                   EVALUATE    SYS-1009-JIYKSIKBN
                       WHEN    "2"
      *                ���Խ�
                           MOVE    SYS-1009-JIYKSIKETA     TO  WRK-KETA
                   END-EVALUATE
               WHEN    "2"
                   MOVE    SYS-1009-HJNKSIRENNUMKETA   TO  WRK-KETA
                   IF      SYS-1009-HJNKSINENKBN   =   "1"
      *                ����
                       MOVE    3                  TO  WRK-KETA2
                   ELSE
      *                ����
                       MOVE    4                  TO  WRK-KETA2
                   END-IF
                   EVALUATE    SYS-1009-HJNKSIKBN
                       WHEN    "1"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   WRK-KETA2
                                               +   1
                       WHEN    "2"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   WRK-KETA2
                       WHEN    "3"
                           COMPUTE WRK-KETA    =   WRK-KETA
                                               +   1
                       WHEN    "4"
                           CONTINUE
                   END-EVALUATE
           END-EVALUATE
           IF      WRK-KETA            >   ZERO
               COMPUTE WRK-KETA2       =   WRK-KETA    +  1
      *        ���ϴ����ֹ�����å�
               IF      WRK-PARA-PTNUM  NOT =   SPACE
                   IF     (WRK-PARA-PTNUM(1:WRK-KETA)  NUMERIC )  AND
                          (WRK-PARA-PTNUM(WRK-KETA2:)  =   SPACE)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-IF
      *        ��λ�����ֹ�����å�
               IF      WRK-PARA-PTNUM-END  NOT =   SPACE
                   IF     (WRK-PARA-PTNUM-END(1:WRK-KETA)
                                                       NUMERIC )  AND
                          (WRK-PARA-PTNUM-END(WRK-KETA2:)  =   SPACE)
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-ERR2
                   END-IF
               END-IF
           END-IF
      *test
           DISPLAY "WRK-PARA-PTNUM:" WRK-PARA-PTNUM
                   ",WRK-PARA-PTNUM-END:" WRK-PARA-PTNUM-END
               ",WRK-KETA:" WRK-KETA ",FLG-ERR:" FLG-ERR
      *
           .
       120-KOUSEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Խ�
      *****************************************************************
       130-KETA-HEN-SEC            SECTION.
      *
           MOVE    SPACE           TO  WRK-KETAHEN
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
      *
           IF      WRK-KETA(1:1)   =   0
               MOVE    WRK-KETA(2:1)   TO  KANACHK-MAE-INPUT
           ELSE
               MOVE    WRK-KETA        TO  KANACHK-MAE-INPUT
           END-IF
      *
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF          KANACHK-RC          =   ZERO
               AND     KANACHK-MAX         >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KETAHEN
           END-IF
      *
           .
       130-KETA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORCBZ001SAREA
      *
           IF      WRK-PARA-PTNUM  NOT =   SPACE
               MOVE    WRK-PARA-PTNUM  TO  ORCBZ001S-PTNUM (1)
               MOVE    1               TO  ORCBZ001S-MAX-CNT
           END-IF
      *
           IF      WRK-PARA-PTNUM-END  NOT =   SPACE
               MOVE    WRK-PARA-PTNUM-END  TO  ORCBZ001S-LST-PTNUM
           END-IF
      *
           INITIALIZE                  HCMZ001
           MOVE    ZERO            TO  CNT-LINE
      *
      *TEST
           DISPLAY "FLG-END: " FLG-END 
                  ",ORCBZ001S-ENDFLG:" ORCBZ001S-ENDFLG
      *TEST
           PERFORM UNTIL ( FLG-END     =   1 )
                   OR    ( ORCBZ001S-ENDFLG    =   1 )
      *
           DISPLAY "ORCBZ001S START"
      *        ���������ֹ�����å�
               CALL    "ORCBZ001S"         USING
                                           SPA-AREA
                                           ORCBZ001SAREA
                                           SYS-1009-REC
               IF          ORCBZ001S-MAX-CNT   >   ZERO
      *            �����Խ�����
                   PERFORM 210-BODY-EDIT-SEC
               END-IF
           END-PERFORM
      *
           DISPLAY "CNT-LINE:"  CNT-LINE
      *
           IF          CNT-LINE    >   ZERO
      *        �إå�����
               PERFORM 210-HEAD-EDIT-SEC
      *        ��������
               PERFORM 400-PRINT-OUT-SEC
           END-IF
      *
             .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<�إå�����>����
      *****************************************************************
       210-HEAD-EDIT-SEC           SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
      *
           MOVE    WRK-KOUSEI      TO  HCMZ001-KOUSEI
           MOVE    WRK-HENYMDG     TO  HCMZ001-HAKKOYMD
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCMZ001-PAGE
      *
           .
       210-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ�Խ�<������>����
      *****************************************************************
       210-BODY-EDIT-SEC           SECTION.
      *
           PERFORM VARYING  IDX    FROM    1   BY  1
                   UNTIL    IDX    >   ORCBZ001S-MAX-CNT
               COMPUTE CNT-LINE        =   CNT-LINE    +   1
               COMPUTE CNT-BZ001       =   CNT-BZ001   +   1
      *
               IF          CNT-LINE    >   120
      *            �إå�����
                   PERFORM 210-HEAD-EDIT-SEC
      *            ��������
                   PERFORM 400-PRINT-OUT-SEC
                   INITIALIZE                  HCMZ001
                   MOVE    1               TO  CNT-LINE
               END-IF
      *
               MOVE    ORCBZ001S-PTNUM(IDX)
                                       TO  HCMZ001-PTNUM(CNT-LINE)
      *
           END-PERFORM
      *
           .
       210-BODY-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMZ001.red"       TO  SPRT-PRTID
           MOVE    "���������ֹ����ɽ"
                                       TO  SPRT-TITLE
           MOVE    HCMZ001             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-BZ001           =   ZERO
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBZ001 IN " CNT-BZ001 " ***"
           DISPLAY "*** ORCBZ001 END ***"
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
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
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
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
      *    �����ޥ����ɤ߹���
      *****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC      =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.