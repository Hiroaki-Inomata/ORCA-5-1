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
      *    ��������Ȳ�ơ��֥�    COPY (CPRRKSRH.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  RRKSRH-REC.
      *���ŵ����ֹ�
            03  RRKSRH-HOSPNUM         PIC 9(2).
      *ü���ɣ�
            03  RRKSRH-TERMID          PIC X(64).
      *���ԣɣ�
            03  RRKSRH-PTID            PIC 9(10).
      *������ʬ
            03  RRKSRH-NYUGAIKBN       PIC X(1).
      *���Ų�
            03  RRKSRH-SRYKA           PIC X(2).
      *�ɥ�����
            03  RRKSRH-DRCD            PIC X(5).
      *�ݸ��ȹ礻
            03  RRKSRH-HKNCOMBINUM     PIC 9(4).
      *������
            03  RRKSRH-SRYYMD          PIC X(8).
      *������
            03  RRKSRH-CREYMD          PIC X(8).
      *������
            03  RRKSRH-UPYMD           PIC X(8).
      *��������
            03  RRKSRH-UPHMS           PIC X(6).
      *�ʲ������ѹ���
      *���Ͽ�����
            03  RRKSRH-STSRYYMD        PIC X(8).
      *��λ������
            03  RRKSRH-EDSRYYMD        PIC X(8).
      *����μ���
            03  RRKSRH-KOHNUM-G.
                05 RRKSRH-KOHNUM       PIC X(3) OCCURS 5.
      *���աܻ���
            03  RRKSRH-YMDHMS          PIC X(14).

