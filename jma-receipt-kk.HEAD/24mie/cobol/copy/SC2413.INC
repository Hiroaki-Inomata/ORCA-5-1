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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SC13.
      *    ����ǯ����
           03  SC13-SEIYMD                     PIC  X(22).
      *    ����ǯ��
           03  SC13-SEIYM                      PIC  X(16).
      *    ͹���ֹ�
           03  SC13-POST                       PIC  X(10).
      *    ���ŵ��ؽ���
           03  SC13-ADRS                       PIC  X(100).
      *    ���ŵ���̾��
           03  SC13-HOSPNAME                   PIC  X(100).
      *    �����ֹ�
           03  SC13-TEL                        PIC  X(15).
      *    ���ŵ��إ�����
           03  SC13-HOSPCD                     PIC  X(20).
      *    ����ɽ��ʬ�ʣ�����ʡ��������ʡ�����Ĵ�ޡ�����ˬ��Ǹ��
           03  SC13-TENHYOKBN                  PIC  X(02)  OCCURS  4.
      *    �����ˡ�ʣ����μ������񡡣����μ���������ɽ��
           03  SC13-TEISYUTU                   PIC  X(02)  OCCURS  2.
      *    ����ơ��֥�
           03  SC13-KENSU-TBL  OCCURS  29.
      *    �������
               05  SC13-KENSU                  PIC  X(05).
      *    ��׷��
           03  SC13-GKENSU                     PIC  X(06).
