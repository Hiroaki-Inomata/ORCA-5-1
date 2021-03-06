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
      *    ����������ɽ     COPY (CPSKT0200TMP.INC)            
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT0200TMP-REC.
           03  SKT0200TMP-KEY.
      *��ʬ
               05  SKT0200TMP-KBN             PIC  X(01).
      *���ȶ�ʬ
               05  SKT0200TMP-GENMEN          PIC  X(01).
      *����ơ��֥�
           03    SKT0200TMP-SEIKYU-TBL  OCCURS 15.
      *�����ơ��֥�
               05  SKT0200TMP-NYUGAI-TBL  OCCURS 2.
      *���
                   07  SKT0200TMP-KENSU       PIC 9(05).
      *����
                   07  SKT0200TMP-NISSU       PIC 9(05).
      *����
                   07  SKT0200TMP-TENSU       PIC 9(09).
      *������ô��
                   07  SKT0200TMP-ITBFTN      PIC 9(09).
      *�������
               05  SKT0200TMP-S-KENSU         PIC 9(05).
      *��������
               05  SKT0200TMP-S-NISSU         PIC 9(05).
      *����������
               05  SKT0200TMP-S-KINGAKU       PIC 9(09).
      *ɸ����ô��
               05  SKT0200TMP-S-FTNGAKU       PIC 9(09).
      *����ơ��֥�
           03  SKT0200TMP-KOH-TBL  OCCURS  20.
      *�����ֹ�
               05  SKT0200TMP-KOHNUM          PIC X(03).
      *����̾��
               05  SKT0200TMP-KOHNAME         PIC X(16).
      *����������ơ��֥�
               05  SKT0200TMP-KNYUGAI-TBL    OCCURS 2.
      *����ݷ��
                   07  SKT0200TMP-KKENSU      PIC 9(05).
      *���������
                   07  SKT0200TMP-KNISSU      PIC 9(05).
      *���������
                   07  SKT0200TMP-KTENSU      PIC 9(09).
      *����ݰ�����ô��
                   07  SKT0200TMP-KITBFTN     PIC 9(09).
      *����ݿ������
               05  SKT0200TMP-S-KKENSU        PIC 9(05).
      *����ݿ�������
               05  SKT0200TMP-S-KNISSU        PIC 9(05).
      *����ݿ���������
               05  SKT0200TMP-S-KKINGAKU      PIC 9(09).
      *�����ɸ����ô��
               05  SKT0200TMP-S-KFTNGAKU      PIC 9(09).
      *��׷��
           03  SKT0200TMP-GKENSU              PIC 9(09).
      *Ϸ�͹�׷��
           03  SKT0200TMP-RGKENSU             PIC 9(09).
