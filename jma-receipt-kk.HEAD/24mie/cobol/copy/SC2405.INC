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
      *  01.00.01    ����         14/03/12  ��Į¼�������ѹ�
      ********************************************************************
       01  SC05.
      *    ���ŵ��إ�����
           03  SC05-HOSPCD-G  OCCURS  7.
               05  SC05-HOSPCD                 PIC  X(02).
      *    ȯ��ǯ����
           03  SC05-SEIYMD                     PIC  X(22).
      *    ��Į¼̾
           03  SC05-CITYNAME                   PIC  X(20)  OCCURS  2.
      *    ͹���ֹ�
           03  SC05-POST                       PIC  X(18).
      *    ���ŵ��ؽ���
           03  SC05-ADRS                       PIC  X(100).
      *    ���ŵ���̾��
           03  SC05-HOSPNAME                   PIC  X(100).
      *    ���߼Ի�̾
           03  SC05-KAISETU                    PIC  X(40).
      *    �����ֹ�
           03  SC05-TEL                        PIC  X(24).
      *    ��Į¼������
           03  SC05-CITYCD                     PIC  X(01)  OCCURS  3.
      *    ���շ�
           03  SC05-SEIYM                      PIC  X(16).
      *    ����ơ��֥�
           03  SC05-KENSU-TBL  OCCURS  6.
      *    ���
               05  SC05-KENSU                  PIC  X(05).
