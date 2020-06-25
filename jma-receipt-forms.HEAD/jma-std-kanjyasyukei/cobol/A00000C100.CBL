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
       PROGRAM-ID.             A00000C100.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����Ģɼ
      *  ����ݡ��ͥ��̾  : ���Կ�����ɽ�������
      *  ������            :
      *  ��������    ��ȼ�       ����
      *  04/01/22    @@@@-Ⱦ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev ������       ����      ����
      *  01.00.01    @@@@-���    10/12/15  ������������
      *                                     open-cobol1.0�б�
      *  02.00.01    @@@@-���    14/10/31  ver4.8.0�б��ʰ���ǥ��쥯�ȥ��ѹ���
      *****************************************************************
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
           03  FLG-END                  PIC 9(01).
           03  FLG-SYSKANRI             PIC 9(01).
           03  FLG-SYSKANRI3            PIC 9(01).
           03  FLG-HKNCOMBI             PIC 9(01).
           03  FLG-PTBYOMEI             PIC 9(01).
           03  FLG-PTINF                PIC 9(01).
           03  FLG-PTNUM                PIC 9(01).
           03  FLG-PTHKNINF             PIC 9(01).
           03  FLG-SRYACCT              PIC 9(01).
           03  FLG-JYURRK               PIC 9(01).
           03  FLG-PTNYUINRRK           PIC 9(01).
           03  FLG-SRYACT               PIC 9(01).
           03  FLG-TENSU                PIC 9(01).
           03  FLG-SRYKA                PIC 9(01).
           03  FLG-SRYKBN               PIC 9(01).
           03  FLG-OK                   PIC 9(01).
           03  FLG-NG                   PIC 9(01).
           03  FLG-ATEND                PIC 9(01).
           03  FLG-GAISW                PIC 9(01).
           03  FLG-NYUSW                PIC 9(01).
           03  FLG-SRYKA-ALL            PIC 9(01).
           03  INP-STS                  PIC 9(01).
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
      *
           03  WRK-PARA-PRTYPE       PIC X(01).
           03  WRK-PARA-MDKBN        PIC X(01).
           03  WRK-PARA-YMD1         PIC X(08).
           03  WRK-PARA-YMD2         PIC X(08).
           03  WRK-PARA-NYUGAIKBN    PIC X(01).
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
           03  WRK-RENNUM            PIC 9(05).
           03  WRK-PTNUM             PIC X(20).
           03  WRK-PTNUM6            PIC 9(06).
           03  WRK-SYUHKNNUM         PIC X(03).
      *
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
               05  WRK-POSTMAE1    PIC X(03).
               05  WRK-POSTMAE2    PIC X(04).
           03  WRK-POSTATOG.
               05  WRK-POSTATO1    PIC X(03).
               05  WRK-POSTNAKA          PIC X(01)   VALUE  "-".
               05  WRK-POSTATO2    PIC X(04).
      *
           03  WRK-PTID2             PIC X(10).
           03  WRK-SRYCD2            PIC X(10).
           03  WRK-NYUGAIKBN         PIC X(01).
           03  WRK-NENREI            PIC 9(03).
           03  WRK-ADRS              PIC X(200).
           03  WRK-PRSRYKA           PIC X(02).
      *
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
           COPY    "A00000C100LNK.INC".
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
                                               WRK-PARA-MDKBN
                                               WRK-PARA-YMD1
                                               WRK-PARA-YMD2
                                               WRK-PARA-NYUGAIKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
           MOVE    SPA-HOSPNUM        TO  JOB-HOSPNUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE  LNK-PRTKANRI-RENNUM    TO
                              C100LNK-PRTKANRI-RENNUM
           MOVE  LNK-PRTKANRI-TBL-KEY   TO
                              C100LNK-PRTKANRI-TBL-KEY
           MOVE  LNK-PRTKANRI-TBL-GROUP TO
                              C100LNK-PRTKANRI-TBL-GROUP
           MOVE  LNK-PRTKANRI-SHORI-RENNUM      TO
                              C100LNK-PRTKANRI-SHORI-RENNUM
           MOVE  LNK-PRTKANRI-SRYYM     TO
                              C100LNK-PRTKANRI-SRYYM
           MOVE  LNK-PRTKANRI-SKYYMD    TO
                              C100LNK-PRTKANRI-SKYYMD
           MOVE  LNK-PRTKANRI-SHELLID   TO
                              C100LNK-PRTKANRI-SHELLID
           MOVE  LNK-PRTKANRI-PRIORITY  TO
                              C100LNK-PRTKANRI-PRIORITY
           MOVE  LNK-PRTKANRI-TERMID    TO
                              C100LNK-PRTKANRI-TERMID
           MOVE  LNK-PRTKANRI-OPID      TO
                              C100LNK-PRTKANRI-OPID
           MOVE  LNK-PRTKANRI-PRTNM     TO
                              C100LNK-PRTKANRI-PRTNM
      *
           MOVE  WRK-PARA-JOBID         TO    C100LNK-JOBID
           MOVE  WRK-PARA-SHELLID       TO    C100LNK-SHELLID
           MOVE  WRK-PARA-HOSPNUM        TO    C100LNK-HOSPNUM
           MOVE  WRK-PARA-MDKBN         TO    C100LNK-MDKBN
           MOVE  WRK-PARA-YMD1          TO    C100LNK-YMD1
           MOVE  WRK-PARA-YMD2          TO    C100LNK-YMD2
           MOVE  WRK-PARA-NYUGAIKBN     TO    C100LNK-NYUGAIKBN
      *
      *    Ģɼ�̥��֥ץ������CALL
           EVALUATE  WRK-PARA-PRTYPE
      *    (���Ų���)
               WHEN  SPACE
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *    (���Ų���)�������Կ�����ɽ
                          CALL "A00000C111" USING  A00000C100-LNK
                         WHEN  OTHER
      *    (���Ų���)���贵�Կ�����ɽ
                          CALL "A00000C110" USING  A00000C100-LNK
                     END-EVALUATE
      *    (�����)
               WHEN  "0"
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *    (�����)�������Կ�����ɽ
                          CALL "A00000C113" USING  A00000C100-LNK
                         WHEN  OTHER
      *    (�����)���贵�Կ�����ɽ
                          CALL "A00000C112" USING  A00000C100-LNK
                     END-EVALUATE
      *    (ǯ����)
               WHEN  "1"
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *    (ǯ����)�������Կ�����ɽ
                          CALL "A00000C115" USING  A00000C100-LNK
                         WHEN  OTHER
      *    (ǯ����)���贵�Կ�����ɽ
                          CALL "A00000C114" USING  A00000C100-LNK
                     END-EVALUATE
      *    (�϶���)
               WHEN  "2"
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *    (�϶���)�������Կ�����ɽ
                          CALL "A00000C117" USING  A00000C100-LNK
                         WHEN  OTHER
      *    (�϶���)���贵�Կ�����ɽ
                          CALL "A00000C116" USING  A00000C100-LNK
                     END-EVALUATE
      *    (�ݸ���)
               WHEN  "3"
                     EVALUATE  WRK-PARA-NYUGAIKBN
                         WHEN  "1"
      *    (�ݸ���)�������Կ�����ɽ
                          CALL "A00000C119" USING  A00000C100-LNK
                         WHEN  OTHER
      *    (�ݸ���)���贵�Կ�����ɽ
                          CALL "A00000C118" USING  A00000C100-LNK
                     END-EVALUATE
      *    (������)
               WHEN  "4"
      *    (������)�������Կ�����ɽ
                     CALL "A00000C120" USING  A00000C100-LNK
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.