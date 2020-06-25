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
       PROGRAM-ID.             A00000K100.
      *******************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ���Ź԰ٶ�ʬ��Ģɼ�������
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  04/03/26    @@@@-Ⱦ��     ��������
      *******************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������      ����      ����
      *   01.00.01    @@@@-����   10/12/14  open-cobol1.0�б�
      *   01.00.01    @@@@-����   10/12/14  ������������
      *   02.00.01    @@@@-���   14/10/31  ver4.8.0�б��ʰ���ǥ��쥯�ȥ��ѹ���
      *   02.00.02    @@@@-���   18/02/01  ���ɽ����ǽ���ɲ�
      *******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
      *    ���������ΰ�
           COPY    "CPCOMMONSHELL.INC".
      *
      *    ���顼�ե����� ̾���ΰ�
           COPY    "CPRECEERR.INC".
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-PARA            PIC 9(02).
           03  STS-PARA2           PIC 9(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYKA           PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-NG              PIC 9(01).
           03  FLG-ATEND           PIC 9(01).
           03  FLG-GAISW           PIC 9(01).
           03  FLG-NYUSW           PIC 9(01).
           03  FLG-SRYKA-ALL       PIC 9(01).
           03  INP-STS             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0               PIC 9(04).
           03  IDX1               PIC 9(02).
           03  IDX2               PIC 9(04).
           03  IDX3               PIC 9(04).
           03  IDX4               PIC 9(02).
           03  IDX5               PIC 9(04).
           03  IDX                PIC 9(04).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC 9(02).
           03  WRK-PARA-PRTYPE       PIC X(01).
           03  WRK-PARA-YM           PIC X(06).
           03  WRK-PARA-NYUGAIKBN    PIC X(01).
           03  WRK-PARA-HKNTG        PIC X(01).
           03  WRK-PARA-HOUKATU      PIC X(01).
           03  WRK-PARA-TKKBN        PIC X(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-PRDTLCNT          PIC 9(06).
           03  WRK-RECCNT            PIC 9(06).
           03  WRK-HONKZKKBN         PIC X(04).
           03  WRK-CHK-SRYKA         PIC X(02).
      *
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
      *    ͹���ֹ��Ѵ�
       03  WRK-POSTMAEG.
               05  WRK-POSTMAE1      PIC X(03).
               05  WRK-POSTMAE2      PIC X(04).
           03  WRK-POSTATOG.
               05  WRK-POSTATO1      PIC X(03).
               05  WRK-POSTNAKA      PIC X(01)   VALUE  "-".
               05  WRK-POSTATO2      PIC X(04).
           03  WRK-PTID2             PIC X(10).
           03  WRK-SRYCD2            PIC X(10).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-ADRS              PIC X(200).
           03  WRK-PRSRYKA           PIC X(02).
           03  WRK-Z3                PIC ZZ9.
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-Z9                PIC ZZZZZZZZZZZ9.
           03  WRK-ZZ                PIC ZZZZZZZZZZZZ.
           03  WRK-S9                PIC -----------9.
           03  WRK-SZ                PIC -----------Z.
           03  DEBUG-CNT             PIC 9(03).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ���ż���̾�ơ��֥�
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
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
      *    �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "A00000K100LNK.INC".
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
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  WRK-PARA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-PRTYPE
                                               WRK-PARA-YM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HKNTG
                                               WRK-PARA-HOUKATU
                                               WRK-PARA-TKKBN
      *
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    LNK-PRTKANRI-RENNUM    TO   K100LNK-PRTKANRI-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY   TO   K100LNK-PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                     TO   K100LNK-PRTKANRI-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                     TO   K100LNK-PRTKANRI-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SRYYM     TO   K100LNK-PRTKANRI-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD    TO   K100LNK-PRTKANRI-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID   TO   K100LNK-PRTKANRI-SHELLID
           MOVE    LNK-PRTKANRI-PRIORITY  TO   K100LNK-PRTKANRI-PRIORITY
           MOVE    LNK-PRTKANRI-TERMID    TO   K100LNK-PRTKANRI-TERMID
           MOVE    LNK-PRTKANRI-OPID      TO   K100LNK-PRTKANRI-OPID
           MOVE    LNK-PRTKANRI-PRTNM     TO   K100LNK-PRTKANRI-PRTNM
      *
           MOVE    WRK-PARA-JOBID         TO   K100LNK-JOBID
           MOVE    WRK-PARA-SHELLID       TO   K100LNK-SHELLID
           MOVE    WRK-PARA-HOSPNUM       TO   K100LNK-HOSPNUM
           MOVE    WRK-PARA-YM            TO   K100LNK-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN     TO   K100LNK-NYUGAIKBN
           MOVE    WRK-PARA-HKNTG         TO   K100LNK-HKNTG
           MOVE    WRK-PARA-HOUKATU       TO   K100LNK-HOUKATU
           MOVE    WRK-PARA-TKKBN         TO   K100LNK-TKKBN
      *
      *    Ģɼ�̥��֥ץ������CALL
           EVALUATE  WRK-PARA-PRTYPE
             WHEN  SPACE
      *    (���Ų���)
               CALL    "A00000K140"     USING   A00000K100-LNK
             WHEN  "0"
      *    (�����)
               CALL    "A00000K141"     USING   A00000K100-LNK
             WHEN  "1"
      *    (ǯ����)
               CALL    "A00000K142"     USING   A00000K100-LNK
             WHEN  "2"
      *    (�϶���)
               CALL    "A00000K143"     USING   A00000K100-LNK
             WHEN  "3"
      *    (�ݸ���)
               CALL    "A00000K144"     USING   A00000K100-LNK
             WHEN  "4"
      *    (������)
               CALL    "A00000K145"     USING   A00000K100-LNK
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.