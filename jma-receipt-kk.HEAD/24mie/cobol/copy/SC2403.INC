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
      *  01.00.01    ����         10/08/19  ������ֹ�η������
      *  01.00.02    ����         14/03/05  ������ô�ۡ���Į�����ɤ��ɲ�
      *                                     ����������ŵ��ؾ���ΰ����ѹ�
      *  01.00.03    ����         19/05/23  �������򥳥��Ⱥ��
      *  01.00.04    ����         19/05/27  ǯ�����������
      ********************************************************************
       01  SC03.
      *    ���ŵ��إ�����
           03  SC03-HOSPCD               PIC X(20).
      *    ����ǯ����
           03  SC03-SEIYMD               PIC X(22).
      *    ͹���ֹ�
           03  SC03-POST                 PIC X(10).
      *    �����
           03  SC03-ADRS                 PIC X(100).
      *    ���ŵ���̾��
           03  SC03-HOSPNAME             PIC X(100).
      *    ���߼Ի�̾
           03  SC03-KAISETU              PIC X(40).
      *    �����ֹ�
           03  SC03-TEL                  PIC X(15).
      *    ��Į������
           03  SC03-CITYCD               PIC X(06).
      *    ��Į¼̾
           03  SC03-CITYNAME             PIC X(26).
      *    �����ʬ
           03  SC03-KOHKBN               PIC X(02)  OCCURS  5.
      *    �����ʾ��ֹ�
           03  SC03-JKYSNUM              PIC X(20).
      *    ���Ի�̾
           03  SC03-NAME                 PIC X(26).
      *    ����
           03  SC03-SEX                  PIC X(02)  OCCURS  2.
      *    ǯ��
           03  SC03-NENGO                PIC X(02)  OCCURS  5.
      *    ��ǯ����
           03  SC03-TANJOYMD             PIC X(18).
      *    �ڡ���
           03  SC03-PAGE                 PIC X(03).
      *    ����ơ��֥�
           03  SC03-SEIKYU-TBL  OCCURS  4.
      *    ����ǯ��
               05  SC03-SRYYM            PIC X(16).
      *    ������ô���
               05  SC03-KYURATE          PIC X(02)  OCCURS  4.
      *    ���Ų�̾
               05  SC03-SRYKANAME        PIC X(20).
      *    ���Ųʥ�����
               05  SC03-SRYKA            PIC X(02).
      *    ���������ʬ
               05  SC03-NYUGAIKBN        PIC X(02)  OCCURS  2.
      *    �������ż�����
               05  SC03-NISSU            PIC X(02).
      *    �ݸ������
               05  SC03-SEIKYUGK         PIC X(09).
      *    �ݸ���������
               05  SC03-TENSU            PIC X(09).
      *    ������ô��
               05  SC03-ITBFTN           PIC X(09).
      *    ����Ĺ����ʬ
               05  SC03-TAHO             PIC X(10).
      *    ������������
               05  SC03-EKOHTENSU        PIC X(09).
      *    ����Ĺ��������ô��
               05  SC03-EKOHFTNGK        PIC X(09).
      *    �������ݸ�����ʬ
               05  SC03-SKINGK           PIC X(09).
      *    ������ɸ����ôʬ
               05  SC03-SFTNGK           PIC X(09).
      *    �����ݸ�������ʬ
               05  SC03-EKOHSKINGK       PIC X(09).
      *    �����ݸ���ɸ����ôʬ
               05  SC03-EKOHSFTNGK       PIC X(09).
      *    ��������ȯ�Զ�ʬ
               05  SC03-SYOHOKBN         PIC X(02)  OCCURS  2.
      *    ��������ȯ�԰��ŵ����ֹ�
               05  SC03-SYOHONUM         PIC X(10).
      *    ��������ȯ�԰��ŵ�������̾��
               05  SC03-SYOHONAME        PIC X(40).
