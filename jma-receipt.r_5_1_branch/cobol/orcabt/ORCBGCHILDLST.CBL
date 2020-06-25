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
       PROGRAM-ID.             ORCBGCHILDLST.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : �л�������������å��ꥹ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/09/9     NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-����    10/01/29  ��ĥ�����б�
      *  04.05.01    NACL-ƣ��    10/07/07  �����������б�
      *
      *  04.06.01    NACL-ƣ��    13/06/05  �࿦���ݤ��ݸ����ֹ�򵭺ܤ���
      *
      *  04.07.01    NACL-ƣ��    12/01/24  monpe-0.6.20�б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  BUNBENERR-FILE  ASSIGN  BUNBENERR
                                   FILE    STATUS  IS  STS-BUNBENERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  BUNBENERR-FILE.
       01  BUNBENERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //BUNBENERR//.
      *
           COPY    "HCMSL55.INC".
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-BUNBENERR       PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-TEISYUTUSAKI
                                   PIC X(01).
               05  KEY-N-SKYKBN    PIC X(01).
               05  KEY-N-HKNJANUM  PIC X(08).
               05  KEY-N-BUNBENKBN PIC X(01).
           03  KEY-OLD.
               05  KEY-O-TEISYUTUSAKI
                                   PIC X(01).
               05  KEY-O-SKYKBN    PIC X(01).
               05  KEY-O-HKNJANUM  PIC X(08).
               05  KEY-O-BUNBENKBN PIC X(01).
      *
      *    �����ΰ�
       01  SUM-AREA.
           03  SUM-OCC             OCCURS  3.
               05  SUM-CNT         PIC 9(03).
               05  SUM-MONEY       PIC 9(08).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(02).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SKYYM      PIC X(06).
           03  WRK-PARA-SKYKBN     PIC X(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-BUNBENERR       PIC X(200).
      *
           03  WRK-TEISYUTUSAKI    PIC X(01).
      *
           03  WRK-SRYYM           PIC X(06).
           03  WRK-SKYYM           PIC X(16).
           03  WRK-SKYYMH          PIC X(05).
           03  WRK-BIRTHDAYH       PIC X(07).
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZZZZZ9        PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    ��������
       01  HEAD-01.
           03  FILLER                  PIC X(20)   VALUE   SPACE.
           03  H01-SRYYM               PIC X(16).
           03  H01-SRYYM-TITLE         PIC X(08).
           03  FILLER                  PIC X(28)   VALUE
               "�л�������������å��ꥹ��".
           03  H01-TEISYUTUSAKI        PIC X(10).
           03  H01-SKYKBN              PIC X(16).
           03  FILLER                  PIC X(08)   VALUE   "��������".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC ZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(12)   VALUE
               "�ݸ����ֹ桡".
           03  FILLER                  PIC X(10)   VALUE
               "ʬ�ڶ�ʬ��".
           03  FILLER                  PIC X(22)   VALUE
               "�����ֹ桡������������".
           03  FILLER                  PIC X(02)   VALUE   SPACE.
           03  FILLER                  PIC X(36)   VALUE
               "��̾".
           03  FILLER                  PIC X(12)   VALUE
               "�л�ǯ����".
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  FILLER                  PIC X(08)   VALUE
               "��������". 
           03  FILLER                  PIC X(09)   VALUE   SPACE.
           03  FILLER                  PIC X(10)   VALUE
               "���������".
      *
       01  BODY-01.
           03  B01-FIL1                PIC X(2).
           03  B01-HKNJANUM            PIC X(10).
           03  B01-BUNBENKBN           PIC X(12).
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(40).
           03  B01-FIL2                PIC X(01).
           03  B01-BIRTHDAY            PIC X(09).
           03  B01-FIL3                PIC X(03).
           03  B01-TEISYUTUBIKBN       PIC X(08).
           03  B01-FIL4                PIC X(09).
           03  B01-DAIRI-MONEY         PIC X(10).
      *
       01  BODY-02.
           03  FILLER                  PIC X(28)   VALUE   SPACE.
           03  B02-TITLE               PIC X(20).
           03  FILLER                  PIC X(06)   VALUE   "�����".
           03  B02-KENSU               PIC X(03).
           03  FILLER                  PIC X(18)   VALUE
               "����������۹�ס�".
           03  B02-GOKEI               PIC X(10).
      *
       01  WRK-CONST-AREA.
            03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   55.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ʬ�ڴ���
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    ��������
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    ����
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK2.INC".
      *
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
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
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           MOVE    LOW-VALUE       TO  KEY-AREA
      *
           INITIALIZE                  BUNBENERR
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
                                       BUNBENERR
                                       WRK-PARA-SKYYM
                                       WRK-PARA-SKYKBN
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-SKYYM =" WRK-PARA-SKYYM
           DISPLAY "WRK-PARA-SKYKBN=" WRK-PARA-SKYKBN
      *
           IF      WRK-PARA-SKYKBN =   SPACE
               MOVE    "0"             TO  WRK-PARA-SKYKBN
           END-IF
           IF      WRK-PARA-SKYYM  =   SPACE
               MOVE    "000000"        TO  WRK-PARA-SKYYM
               MOVE    "0"             TO  WRK-PARA-SKYKBN
           END-IF
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBGCHILDLST.CBL"
                                   TO  JOB-PGID
           MOVE    "�л�������������å��ꥹ��"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BUNBENERR       TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BUNBENERR
      *
      *    ����ǯ���Խ�
           IF      WRK-PARA-SKYYM  =   "000000"
               MOVE    "����ǯ������ʤ�"
                                       TO  H01-SRYYM
               MOVE    "������"        TO  H01-SRYYM-TITLE
           ELSE     
               MOVE    WRK-PARA-SKYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3(1:16)
                                       TO  H01-SRYYM
               MOVE    "����ʬ"        TO  H01-SRYYM-TITLE
           END-IF
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
           IF      WRK-PARA-SKYKBN =   "0" OR  "1" OR  "2" OR  "3"
               CONTINUE
           ELSE
               MOVE    "�����ʬ�����꤬���äƤ��ޤ�" 
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ʬ�ڴ����ɹ�
           MOVE    "%"             TO  WRK-TEISYUTUSAKI
           PERFORM 900-BUNBEN-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  SUM-AREA
           ADD     1               TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    ZERO            TO  IDX
           MOVE    KEY-NEW         TO  KEY-OLD
      *
           EVALUATE    KEY-N-TEISYUTUSAKI
               WHEN    SPACE
                   MOVE    "��������"          TO  H01-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "�ʼ��ݡ�"          TO  H01-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    "�ʹ��ݡ�"          TO  H01-TEISYUTUSAKI
           END-EVALUATE
      *
      *    ��ʧ�������Խ�����
           PERFORM          UNTIL   FLG-END        =   1
                            OR      KEY-N-TEISYUTUSAKI
                                               NOT =
                                                     KEY-O-TEISYUTUSAKI
      *        �����ʬ���Խ�����
               MOVE    KEY-NEW         TO  KEY-OLD
               INITIALIZE                  SUM-OCC (2)
      *
               EVALUATE    KEY-N-SKYKBN
                   WHEN    SPACE
                       MOVE    "��������������"    TO  H01-SKYKBN
                   WHEN    "0"
                       MOVE    "�����ᤷ�ʤ���"    TO  H01-SKYKBN
                   WHEN    "1"
                       MOVE    "��̤����ۡ���"    TO  H01-SKYKBN
                   WHEN    "2"
                       MOVE    "������ѡۡ���"    TO  H01-SKYKBN
               END-EVALUATE
               PERFORM 2001-HEAD-PRINT-SEC
               PERFORM             UNTIL   FLG-END        =   1
                                   OR      KEY-N-TEISYUTUSAKI
                                                      NOT =
                                                   KEY-O-TEISYUTUSAKI
                                   OR      KEY-N-SKYKBN
                                                      NOT =
                                                   KEY-O-SKYKBN
      *            ʬ�ڶ�ʬ���ݸ������Խ�����
                   INITIALIZE                  SUM-OCC (1)
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM          UNTIL   FLG-END        =  1
                                    OR      KEY-NEW    NOT = KEY-OLD
      *                �����Խ�����
                       PERFORM 2002-BODY-PRINT-SEC
                   END-PERFORM
      *
      *            ʬ�ڶ�ʬ���ݸ����̹���Խ�����
                   MOVE    1                TO  IDX
                   PERFORM 2003-TERM-PRINT-SEC
               END-PERFORM
      *
      *        �����ʬ�̹���Խ�����
               MOVE    2                TO  IDX
               PERFORM 2003-TERM-PRINT-SEC
           END-PERFORM
      *
      *    ��ʧ�����̹���Խ�����
           MOVE    3                TO  IDX
           PERFORM 2003-TERM-PRINT-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ф��Խ�����
      *****************************************************************
       2001-HEAD-PRINT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL55
           MOVE    HEAD-01             TO  HCMSL55-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL55-MOJI (3)
      *
           MOVE    4                   TO  CNT-LINE
           .
       2001-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������
      *****************************************************************
       2002-BODY-PRINT-SEC          SECTION.
      *
           MOVE    SPACE               TO  BODY-01
      *    �ݸ����ֹ�
           MOVE    BUNBEN-HKNJANUM     TO  B01-HKNJANUM
      *    ʬ�ڶ�ʬ
           EVALUATE    BUNBEN-BUNBENKBN
               WHEN    "1"
                   MOVE    "�����"      TO  B01-BUNBENKBN
               WHEN    "2"
                   MOVE    "���۾"      TO  B01-BUNBENKBN
           END-EVALUATE
      *
           PERFORM          UNTIL   FLG-END        =   1
                            OR      KEY-NEW    NOT =   KEY-OLD
      *
               IF      CNT-LINE             >=  WRK-CONS-LINE-MAX
      *            Ģɼ��������
                   PERFORM 2004-PRINT-OUT-SEC
      *            ���Ф��Խ�����
                   PERFORM 2001-HEAD-PRINT-SEC
               END-IF
      *
      *        �����ֹ�
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    BUNBEN-PTID         TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               MOVE    SPTID-PTNUM         TO  B01-PTNUM
      *
      *****    MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *****    MOVE    BUNBEN-PTID         TO  PTINF-PTID
      *****    MOVE    PTINF-REC           TO  MCPDATA-REC
      *****    PERFORM 900-PTINF-READ-SEC
               MOVE    BUNBEN-NAME         TO  B01-NAME
               INSPECT B01-NAME            REPLACING  ALL "  "  BY  "��"
               IF    ( SPA-ENCODING    =   2 )
                   INITIALIZE              ORCSKANACHK2AREA
                   MOVE    "1"         TO  KANACHK2-SYORI
                   MOVE    B01-NAME    TO  KANACHK2-MAE-INPUT
                   CALL    "ORCSKANACHK2"  USING
                                           ORCSKANACHK2AREA
                   MOVE    KANACHK2-MAE-INPUT
                                       TO  B01-NAME
               END-IF
      *
               MOVE    BUNBEN-BIRTHDAY     TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD1    TO  B01-BIRTHDAY
      *
           EVALUATE    BUNBEN-TEISYUTUBIKBN
               WHEN    "1"
                   MOVE    "��������"      TO  B01-TEISYUTUBIKBN
               WHEN    "2"
                   MOVE    "��������"      TO  B01-TEISYUTUBIKBN
           END-EVALUATE
      *
               MOVE    BUNBEN-MONEY (12)   TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9        TO  B01-DAIRI-MONEY
      *
               ADD     1               TO  CNT-LINE
               MOVE    BODY-01         TO  HCMSL55-MOJI (CNT-LINE)
      *
               MOVE    SPACE           TO  BODY-01
               MOVE    "��������"      TO  B01-BUNBENKBN
      *
               ADD     1               TO  CNT-OUT
      *
      *        ��׽��׽���
               ADD     1               TO  SUM-CNT (1)
                                           SUM-CNT (2)
                                           SUM-CNT (3)
               ADD     BUNBEN-MONEY (12)
                                       TO  SUM-MONEY (1)
                                           SUM-MONEY (2)
                                           SUM-MONEY (3) 
      *
      *        ʬ�ڴ����ɹ�   
               PERFORM 900-BUNBEN-READ-N-SEC
           END-PERFORM
           .
       2002-BODY-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       2003-TERM-PRINT-SEC                SECTION.
      *
      *    ����ǯ�����ʤ��ΤȤ�����װʳ����Խ����ʤ� 
           IF    ( IDX                <   3        )
           AND   ( WRK-PARA-SKYYM     =   "000000" ) 
               GO  TO  2003-TERM-PRINT-EXT
           END-IF
      *    
           EVALUATE    IDX
               WHEN    1
                   MOVE    "��������������������"
                                           TO  B02-TITLE
                   ADD     1               TO  CNT-LINE
               WHEN    2
                   MOVE    "�������ʬ�̹�סۡ�"
                                           TO  B02-TITLE
               WHEN    3
                   IF      WRK-PARA-SKYYM      =   "000000" 
                       MOVE    "�ڹ硡�������סۡ���"
                                                   TO  B02-TITLE
                       ADD     1                   TO  CNT-LINE
                   ELSE
                       MOVE    "��������̹�סۡ���"
                                                   TO  B02-TITLE
                   END-IF
           END-EVALUATE
      *
           MOVE    SUM-CNT   (IDX) TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  B02-KENSU
           MOVE    SUM-MONEY (IDX) TO  WRK-ZZZZZZZ9
           MOVE    WRK-ZZZZZZZ9    TO  B02-GOKEI
      *
           IF      CNT-LINE            >=  WRK-CONS-LINE-MAX
      *        Ģɼ��������
               PERFORM 2004-PRINT-OUT-SEC
      *        ���Ф��Խ�����
               PERFORM 2001-HEAD-PRINT-SEC
           END-IF
      *
           ADD     1               TO  CNT-LINE
           MOVE    BODY-02         TO  HCMSL55-MOJI (CNT-LINE)
      *
           EVALUATE    IDX
               WHEN    1
      *            �ݸ����ֹ桦ʬ�ڶ�ʬ�̹�פθ�ˣ��Զ�����
                   IF      KEY-N-SKYKBN     =   KEY-O-SKYKBN
                       ADD     1                TO  CNT-LINE
                   END-IF
               WHEN    2
                   IF      KEY-N-TEISYUTUSAKI
                                            =   KEY-O-TEISYUTUSAKI
      *                Ģɼ��������
                       PERFORM 2004-PRINT-OUT-SEC
                   END-IF
               WHEN    3
      *            Ģɼ��������
                   PERFORM 2004-PRINT-OUT-SEC
           END-EVALUATE
           .
      *
       2003-TERM-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       2004-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
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
           MOVE   "HCMSL55.red"        TO  SPRT-PRTID
           MOVE    HCMSL55             TO  SPRT-PRTDATA
           EVALUATE    KEY-O-TEISYUTUSAKI
               WHEN    SPACE
                   MOVE
                       "������ǯ��ʤ��˽л�������������å��ꥹ��"
                                           TO  SPRT-TITLE
               WHEN    "1"
                   MOVE
                       "�ʼ��ݡ˽л�������������å��ꥹ��"
                                           TO  SPRT-TITLE
               WHEN    "2"
                   MOVE
                       "�ʹ��ݡ˽л�������������å��ꥹ��"
                                           TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤˹����Ǥ��ޤ���Ǥ���"
                                          TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2004-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               BUNBENERR-FILE
           IF        ( STS-BUNBENERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT            BUNBENERR-FILE
      *
               MOVE    WRK-BUNBENERR     TO  BUNBENERR-REC
               WRITE   BUNBENERR-REC
               CLOSE   BUNBENERR-FILE
      *
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-BUNBENERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBGCHILDLST.CBL "
                                   DELIMITED  BY  SIZE
                   WRK-BUNBENERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           DISPLAY "*** ORCBGCHILDLST IN   "  CNT-IN
           DISPLAY "*** ORCBGCHILDLST OUT  "  CNT-OUT
           DISPLAY "*** ORCBGCHILDLST PAGE "  CNT-PAGE
           DISPLAY "*** ORCBGCHILDLST END ***"
      *
           IF      CNT-IN          =   ZERO
               MOVE    "�оݴ��Ԥ�����ޤ���Ǥ���"
                                       TO  WRK-BUNBENERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʬ�ڴ����ɹ�
      *****************************************************************
       900-BUNBEN-READ-SEC             SECTION.
      * 
           INITIALIZE                  BUNBEN-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  BUNBEN-HOSPNUM
           MOVE    WRK-PARA-SKYYM  TO  BUNBEN-SKYYM
           MOVE    WRK-TEISYUTUSAKI
                                   TO  BUNBEN-TEISYUTUSAKI
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-BUNBEN-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-BUNBEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʬ�ڴ����ɹ�
      *****************************************************************
       900-BUNBEN-READ-N-SEC           SECTION.
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  BUNBEN-REC
               IF      BUNBEN-SKYKBN   =   SPACE
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    BUNBEN-PTID     TO  PTINF-PTID
               MOVE    PTINF-REC       TO  MCPDATA-REC
               PERFORM 900-PTINF-READ-SEC
               IF      PTINF-TSTPTNUMKBN   =   "1"
                   GO TO  900-BUNBEN-READ-N-SEC
               END-IF
      *
               EVALUATE    WRK-PARA-SKYKBN
                   WHEN    "1"
                       IF      BUNBEN-SKYKBN   =   "2"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
                   WHEN    "2"
                       IF      BUNBEN-SKYKBN   =   "1"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
                   WHEN    "3"
                       IF      BUNBEN-SKYKBN   =   "0"
                           CONTINUE
                       ELSE
                           GO TO  900-BUNBEN-READ-N-SEC
                       END-IF
               END-EVALUATE
      *
               IF      WRK-PARA-SKYYM  =   "000000"
                   MOVE    SPACE           TO  KEY-N-SKYKBN
                                               KEY-N-TEISYUTUSAKI
                                               KEY-N-BUNBENKBN
                                               KEY-N-HKNJANUM
               ELSE
                   MOVE    BUNBEN-SKYKBN   TO  KEY-N-SKYKBN
                   MOVE    BUNBEN-TEISYUTUSAKI
                                           TO  KEY-N-TEISYUTUSAKI
                   MOVE    BUNBEN-BUNBENKBN
                                           TO  KEY-N-BUNBENKBN
      *********    MOVE    BUNBEN-HKNJANUM-S
                   MOVE    BUNBEN-HKNJANUM TO  KEY-N-HKNJANUM    
               END-IF
               ADD     1               TO  CNT-IN
      *  
               DISPLAY "PTNUM="         BUNBEN-PTNUM
                       " TEISYUTUSAKI=" BUNBEN-TEISYUTUSAKI
                       " SKYKBN="       BUNBEN-SKYKBN
                       " BUNBENKBN="    BUNBEN-BUNBENKBN
                       " HKNJANUM="     BUNBEN-HKNJANUM-S
           ELSE
               MOVE    1               TO  FLG-END
               MOVE    HIGH-VALUE      TO  KEY-NEW
           END-IF
           .
       900-BUNBEN-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *