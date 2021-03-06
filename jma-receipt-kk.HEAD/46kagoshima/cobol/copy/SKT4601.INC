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
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    11/03/04    ��������������б�
      *  01.00.02    ����    12/01/16    ������ΥХ��ȿ�����
      *  01.00.02    ���    19/02/04    �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT4601.
      *
      *���ŵ��إ�����
           03  SKT4601-HOSPCD              PIC  X(07).
      *����������
           03  SKT4601-TOKUBETU            PIC  X(10).
      *����ε��եơ��֥�
           03  SKT4601-KOHI        OCCURS  10.
      *ˡ���ֹ�
               05  SKT4601-HBTNUM          PIC  X(02).
      *����̾��
               05  SKT4601-KOHNAME         PIC  X(10).
      *����������ơ��֥�
               05  SKT4601-NYUGAI-TBL      OCCURS    2.
      *���
                   07 SKT4601-KENSU        PIC  X(05).
      *���ް�����ô��
      *             07 SKT4601-YKZFTN       PIC  X(09).
      *������ô��
                   07 SKT4601-ITBFTN       PIC  X(09).
           03  SKT4601-SHOKUJI     OCCURS  10.
      *�������
               05  SKT4601-SKENSU          PIC  X(05).
      *����������
               05  SKT4601-SKINGK          PIC  X(09).
      *ɸ����ô��
               05  SKT4601-SFTNGK          PIC  X(09).
      *
      *����ε��եơ��֥�
           03  SKT4601-ZAITAKU             OCCURS  7.
      *���
               05  SKT4601-Z-KENSU         PIC  X(05).
      *����
               05  SKT4601-Z-TENSU         PIC  X(10).
      *������ô��
               05  SKT4601-Z-ITBFTN        PIC  X(09).
      *����ε��եơ��֥�
           03  SKT4601-ZAITAKU-KOHI        OCCURS  3.
      *ˡ���ֹ�
               05  SKT4601-Z-HBTNUM        PIC  X(02).
      *����̾��
               05  SKT4601-Z-KOHNAME       PIC  X(10).
      *���
               05  SKT4601-ZK-KENSU        PIC  X(05).
      *������ô��
               05  SKT4601-ZK-ITBFTN       PIC  X(09).
