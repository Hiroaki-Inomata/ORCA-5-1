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
      *  01.00.01    ����         10/09/24  ����б�
      *  01.00.02    ����         11/01/11  ��������������б�
      *  01.00.03    ����         19/01/11  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT3720.
      *����ǯ��
           03  SKT3720-SRYY                PIC  X(04).
           03  SKT3720-SRYM                PIC  X(04).
      *���ŵ��ؽ����
           03  SKT3720-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT3720-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT3720-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT3720-KAISETU             PIC  X(40).
      *���ŵ��إ�����
           03  SKT3720-HOSPCDN             PIC  X(20).
      *���Ų�
           03  SKT3720-RECEKA              PIC  X(20).
      *����������
           03  SKT3720-TOKUBETU            PIC  X(10).
      *�������
           03  SKT3720-SEIKYU  OCCURS 11.
      *���������񡧷��
               05  SKT3720-SKENSU       PIC  X(05).
      *���������񡧶��
               05  SKT3720-SKINGK       PIC  X(09).
      *����������ɸ����ô��
               05  SKT3720-SFTNGK       PIC  X(09).
      *��˥��ꥢ
           03  SKT3720-K35-SEIKYU.
      *���������񡧷��
               05  SKT3720-K35-SKENSU       PIC  X(05).
      *���������񡧶��
               05  SKT3720-K35-SKINGK       PIC  X(09).
      *����������ɸ����ô��
               05  SKT3720-K35-SFTNGK       PIC  X(09).
      *���񥨥ꥢ
           03  SKT3720-KOHI-SEIKYU OCCURS 6.
      *ˡ���ֹ�
               05  SKT3720-K-HBTNUM       PIC  X(04).
      *����̾
               05  SKT3720-K-KOHNAME      PIC  X(10).
      *���������񡧷��
               05  SKT3720-K-SKENSU       PIC  X(05).
      *���������񡧶��
               05  SKT3720-K-SKINGK       PIC  X(09).
      *����������ɸ����ô��
               05  SKT3720-K-SFTNGK       PIC  X(09).
