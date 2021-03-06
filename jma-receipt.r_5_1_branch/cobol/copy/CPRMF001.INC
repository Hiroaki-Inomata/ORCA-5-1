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
      *    �����������     COPY (CPRMF001.INC)                *
      *
      *                        ��ʬ��ORCBM001
      *                              2:����Ȱ��ݡ�Ϸ�ͤ�ʻ��
      *                              3:����ȸ����ʻ��
      *                              4:����ñ��
      *                              ORCBM004
      *                              1�Τ�
      *                        ���ܤε��վ���
      *                               1:����
      *                               2:������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF01-REC.
           03  MF01-KEY.
      *��ʬ
               05  MF01-KBN             PIC  X(01).
      *�������
               05  MF01-KOHINF          OCCURS  4.
      *������ݸ��ֹ�
                   07  MF01-KOHNUM      PIC  X(03).
      *���ܤε��վ���
           03  MF01-KYUFUINF            OCCURS  2.
      *���
               05  MF01-KENSU           PIC  9(06).
      *����
               05  MF01-NISSU           PIC  9(06).
      *����
               05  MF01-TOTALTEN        PIC  9(10).
      *��ô���
               05  MF01-FTNMONEY        PIC  9(10).
      *���ް�����ô��
               05  MF01-YKZFTN          PIC  9(10).
      *�������������
           03  MF01-SHOKUJIINF.
      *�������
               05  MF01-SHOKUJIKENSU    PIC  9(06).
      *��������
               05  MF01-SHOKUJINISSU    PIC  9(06).
      *����������
               05  MF01-SHOKUJIRYOYOHI  PIC  9(09).
      *ɸ����ô��
               05  MF01-SHOKUJIFTN      PIC  9(09).
      *
