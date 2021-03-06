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
      *    �����������     COPY (CPSKT3700TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/09/24  ����б�
      *  01.00.02    ����         19/01/11  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT3700TMP-REC.
           03  SKT3700TMP-KEY.
      *���⡦����
               05  SKT3700TMP-PREFKBN           PIC  X(01).
      *���Ų�
               05  SKT3700TMP-RECEKA            PIC  X(02).
      *
      *����ơ��֥�
           03  SKT3700TMP-HKN-AREA  OCCURS  11.
               05  SKT3700TMP-NYUGAI-TBL  OCCURS  2.
      *���
                   07  SKT3700TMP-KENSU        PIC 9(05).
      *����
                   07  SKT3700TMP-TENSU        PIC 9(09).
      *���ް�����ô��ô��
                   07  SKT3700TMP-YKZFTN       PIC 9(09).
      *������ô��
                   07  SKT3700TMP-ITBFTN       PIC 9(09).
      *
      *��ˡ�����
           03  SKT3700TMP-KZN-AREA  OCCURS  2.
               05  SKT3700TMP-KZN-NYUGAI  OCCURS  2.
      *��ˡ����������
                   07  SKT3700TMP-Z-KENSU      PIC 9(05).
      *��ˡ�����������
                   07  SKT3700TMP-Z-TENSU      PIC 9(09).
      *��ˡ����������ް�����ô��
                   07  SKT3700TMP-Z-YKZFTN     PIC 9(09).
      *��ˡ�������������ô��
                   07  SKT3700TMP-Z-ITBFTN     PIC 9(09).
      *
      *����ơ��֥�
           03  SKT3700TMP-KOH-AREA  OCCURS  20.
      *�����ֹ�
               05  SKT3700TMP-KOHNUM           PIC X(03).
      *����̾��
               05  SKT3700TMP-KOHNAME          PIC X(10).
               05  SKT3700TMP-K-NYUGAI-TBL  OCCURS  2.
      *���񡿷��
                   07  SKT3700TMP-K-KENSU      PIC 9(05).
      *��������
                   07  SKT3700TMP-K-TENSU      PIC 9(09).
      *�������ް�����ô��
                   07  SKT3700TMP-K-YKZFTN     PIC 9(09).
      *���񡿰�����ô��
                   07  SKT3700TMP-K-ITBFTN     PIC 9(09).
