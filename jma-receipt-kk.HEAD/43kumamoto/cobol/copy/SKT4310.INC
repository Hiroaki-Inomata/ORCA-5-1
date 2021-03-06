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
      *  01.00.01    ����         11/02/10  ��������������б�
      *  01.00.02    ���         18/09/05  ���ץ����Ȥ���
      ********************************************************************
       01  SKT4310.
      *����ǯ��
           03  SKT4310-SRYYM               PIC  X(16).
      *͹���ֹ�
           03  SKT4310-POST.
               05  SKT4310-POST-OCC        PIC  X(02) OCCURS 7.
      *ɸ�ֲ�̾
           03  SKT4310-SRYKANAME           PIC  X(16) OCCURS 2.
      *NO
           03  SKT4310-NO                  PIC  X(03).
      *���ŵ��ؽ����
           03  SKT4310-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4310-HOSPNAME            PIC  X(100).
      *�����ֹ�
           03  SKT4310-TEL                 PIC  X(15).
      *���߼Ի�̾
           03  SKT4310-KAISETU             PIC  X(30).
      *���ŵ��إ�����
           03  SKT4310-HOSPCDN             PIC  X(20).
      *����������
           03  SKT4310-TOKUBETU            PIC  X(10).
      *�������
           03  SKT4310-SEIKYU  OCCURS 8.
      *�ݸ���̾
               05  SKT4310-HKNJANAME       PIC  X(20).
      *���ճ��
               05  SKT4310-KYURATE         PIC  X(06).
      *�ݸ��ơ��֥�
               05  SKT4310-HKN-TBL   OCCURS  5.
      *�����ơ��֥�
                   07  SKT4310-NYUGAIKBN OCCURS 2.
      *���
                       09  SKT4310-KENSU   PIC  X(04).
      *����
                       09  SKT4310-TENSU   PIC  X(07).
      *����������
                   07  SKT4310-SKINGK      PIC  X(07).
      *ɸ����ô��
                   07  SKT4310-SFTNGK      PIC  X(07).
      *��ץ��ꥢ
           03  SKT4310-GOKEI-AREA.
               05  SKT4310-G-HKN-TBL   OCCURS  5.
      *�����ơ��֥�
                   07  SKT4310-G-NYUGAIKBN OCCURS 2.
      *���
                       09  SKT4310-G-KENSU   PIC  X(04).
      *����
                       09  SKT4310-G-TENSU   PIC  X(07).
      *����������
                   07  SKT4310-G-SKINGK      PIC  X(07).
      *ɸ����ô��
                   07  SKT4310-G-SFTNGK      PIC  X(07).
      *���񥨥ꥢ
           03  SKT4310-KOHI-TBL  OCCURS 6.
      *����ˡ���ֹ�
               05  SKT4310-HBTNUM            PIC X(02).
      *
               05  SKT4310-KNYUGAIKBN  OCCURS 2.
      *���񡧷��
                   07  SKT4310-K-KENSU       PIC X(04).
      *��������
                   07  SKT4310-K-TENSU       PIC X(07).
      *���񡧿���������
               05  SKT4310-K-SKINGK          PIC X(07).
      *����ɸ����ô��
               05  SKT4310-K-SFTNGK          PIC X(07).
