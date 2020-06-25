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
      ************************************************************
      *    ���Ծ���Ȳ�ơ��֥�    COPY (CPPTSRH.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      ************************************************************
      *    MCPDATA-REC (50,000byte)�˼��ޤ�褦�ˤ��뤳��!!!
      ************************************************************
      *01  PTSRH-REC.
      *���ŵ����ֹ�
           03  PTSRH-HOSPNUM      PIC 9(2).
      *ü���ɣ�
           03  PTSRH-TERMID       PIC X(64).
      *���ԣɣ�
           03  PTSRH-PTID         PIC 9(10).
      *�����ֹ�
           03  PTSRH-PTNUM        PIC X(20).
      *���ʻ�̾
           03  PTSRH-KANANAME     PIC X(100).
      *����
           03  PTSRH-SEX          PIC X(1).
      *������
           03  PTSRH-BIRTHDAY     PIC X(8).
      *������ֹ�
           03  PTSRH-JKYSNUM      PIC X(20).
      *������ʬ
           03  PTSRH-NYUGAIKBN    PIC X(01).
      *�ݸ��μ���
           03  PTSRH-HKNNUM       PIC X(03).
      *����μ��ࣱ
           03  PTSRH-KOHNUM1      PIC X(03).
      *����μ��ࣲ
           03  PTSRH-KOHNUM2      PIC X(03).
      *����
           03  PTSRH-TOTALTEN     PIC 9(10).
      *���Ź԰٥�����
           03  PTSRH-SRYCD        PIC X(09).
      *���Ź԰�̾��
           03  PTSRH-SRYNAME      PIC X(100).
      *���Ź԰٥���̾��
           03  PTSRH-SRYKANANAME  PIC X(100).
      *���Ź԰����Ͻ���
           03  PTSRH-SRYSEQ       PIC 9(01).
      *���
           03  PTSRH-MEMO-G.
               05  PTSRH-MEMO         PIC X(30000).
               05  PTSRH-MEMO-R1      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-SRYYM                 PIC X(06).
                   07  PTSRH-FIRSTDAY              PIC 9(02).
                   07  PTSRH-SRYSURYO              PIC 9(05)V9(05).
                   07  PTSRH-ZAIKAISU              PIC 9(05).
                   07  PTSRH-SURYOTOTAL            PIC 9(07)V9(05).
                   07  PTSRH-SRYYM-SRYSURYO        PIC 9(05)V9(05).
                   07  PTSRH-SRYYM-ZAIKAISU        PIC 9(05).
                   07  PTSRH-SRYYM-SURYOTOTAL      PIC 9(07)V9(05).
                   07  PTSRH-SURYO-EDIT            PIC 9(01).
                   07  PTSRH-TANICD                PIC X(03).
                   07  PTSRH-TANINAME              PIC X(12).
                   07  PTSRH-DAY                   PIC 9(03)
                                                   OCCURS  31.
                   07  PTSRH-RENNUM                PIC 9(05).
                   07  PTSRH-R1-FIL                PIC X(29824).
               05  PTSRH-MEMO-R2            REDEFINES    PTSRH-MEMO.
                   07  PTSRH-CNT-ALL        PIC 9(10).
                   07  PTSRH-CNT-PTID       PIC 9(10).
                   07  PTSRH-CNT-SRYCD      PIC 9(10).
                   07  PTSRH-OUTKBN         PIC X(01).
                   07  PTSRH-DBERR          PIC 9(01).
                   07  PTSRH-ORDER          PIC X(500).
                   07  PTSRH-R2-FIL   PIC X(29468).
               05  PTSRH-MEMO-R3      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-HKNCOMBI       PIC X(04).
                   07  PTSRH-HKNID          PIC 9(10).
                   07  PTSRH-KOH-OCC        OCCURS 4.
                       09  PTSRH-KOHNUM     PIC X(03).
                       09  PTSRH-KOHID      PIC 9(10).
                   07  PTSRH-TOKKI-OCC      OCCURS 5.
                       09  PTSRH-TOKKI      PIC X(02).
                       09  PTSRH-TOKKINM    PIC X(12).
                   07  PTSRH-R3-FIL   PIC X(29864).
               05  PTSRH-MEMO-R4      REDEFINES    PTSRH-MEMO.
                   07  PTSRH-R4-SURYO-EDIT         PIC 9(01).
      *DB��Ǽ���ǡ����ΰ�����ΰ١����͹��ܤ�ʸ������(X)�����
                   07  PTSRH-R4-KAISUTOTAL         PIC X(07).
                   07  PTSRH-R4-SURYOTOTAL         PIC X(12).
                   07  PTSRH-R4-TANICD             PIC X(03).
                   07  PTSRH-R4-TANINAME           PIC X(12).
                   07  PTSRH-R4-OCC                OCCURS 1000.
                       09  PTSRH-R4-SRYYMD         PIC X(08).
                       09  PTSRH-R4-SRYSURYO       PIC X(11).
                       09  PTSRH-R4-ZAIKAISU       PIC X(03).
                   07  PTSRH-R4-FIL                PIC X(7965).
      *�ǡ�����ʬ
      *    0:������(orca13,orca36)
      *    1:�������(orca13,orca36)
      *    2:�����ѥǡ���(orca13)
      *    3:�������(orca13 API ���Ź԰� ����, CSV������)
      *    4:�����ѥǡ���(orca36)
      *    a:������(orca36 �������Կ�)
      *    b:�������(orca36 �������Կ�)
      *    c:�����ѥǡ���(orca36 �������Կ�)
           03  PTSRH-DATAKBN      PIC X(1).
      *Ϣ��
           03  PTSRH-SEQNUM       PIC 9(07).
      *������
           03  PTSRH-CREYMD       PIC X(8).
      *������
           03  PTSRH-UPYMD        PIC X(8).
      *��������
           03  PTSRH-UPHMS        PIC X(6).
      *�ʲ������ѹ���
      *�ǡ�����ʬ
           03  PTSRH-UPDATAKBN    PIC X(1).
      *SQL
           03  PTSRH-SQL          PIC X(15000).
      *DBESCAPEENCODING
           03  PTSRH-DBSTR        PIC X(500).
      *���
           03  PTSRH-KENSU        PIC 9(08).
      *�ƥ����ȡ����ѡ�
           03  PTSRH-VTEXT        PIC X(1000).
      *ʸ��������ѡ�
           03  PTSRH-VSTR-G.
               05  PTSRH-VSTR     PIC X(64) OCCURS 3.