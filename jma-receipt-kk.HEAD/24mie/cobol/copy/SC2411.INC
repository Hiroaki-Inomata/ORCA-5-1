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
       01  SC11.
      *    ���շ�
           03  SC11-SEIYM                      PIC  X(16).
      *    ͹���ֹ�
           03  SC11-POST                       PIC  X(18).
      *    ���ŵ��ؽ���
           03  SC11-ADRS                       PIC  X(100).
      *    ���ŵ���̾��
           03  SC11-HOSPNAME                   PIC  X(100).
      *    �����ֹ�
           03  SC11-TEL                        PIC  X(24).
      *    ���ŵ��إ�����
           03  SC11-HOSPCD                     PIC  X(14).
      *    ����ɽ��ʬ
           03  SC11-TENHYOKBN                  PIC  X(02)  OCCURS  4.
      *    ����ơ��֥�
           03  SC11-KENSU-TBL  OCCURS  6.
      *    ���
               05  SC11-KENSU                  PIC  X(07).