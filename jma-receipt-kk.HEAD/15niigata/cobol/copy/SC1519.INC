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
      *  01.00.01    ����         10/06/07  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02    ����         18/11/07  �������򥳥��Ⱥ��
      *  01.00.03    ����         18/11/07  �����б�
      ********************************************************************
       01  SC19.
      *����ǯ��
           03  SC19-SRYGG                 PIC  X(04).
           03  SC19-SRYYY                 PIC  X(04).
           03  SC19-SRYMM                 PIC  X(04).
      *���ŵ���CD
           03  SC19-HOSPCD                PIC  X(14).
      *���ŵ��ؽ����
           03  SC19-ADRS                  PIC  X(100).
      *���ŵ���̾��
           03  SC19-HOSPNAME              PIC  X(100).
      *���߼Ի�̾
           03  SC19-KAISETU               PIC  X(30).
      *���
           03  SC19-MAISU                 PIC  X(05) OCCURS 6.
      *���ΰ�Ǥ��
           03  SC19-KINGK                 PIC  X(09) OCCURS 6.
