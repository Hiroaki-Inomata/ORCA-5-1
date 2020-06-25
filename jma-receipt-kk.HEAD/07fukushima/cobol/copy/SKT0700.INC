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
      *  01.00.01    ����    2011/02/09  ��������������б�
      *  01.00.02    ����    2018/10/10  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0700.
      *
           03  SKT0700-TBL-A.
      *����ǯ��
               05  SKT0700-SEIYM                   PIC  X(16).
      *���ŵ��ؽ���
               05  SKT0700-ADRS                    PIC  X(100).
      *���ŵ���̾��
               05  SKT0700-HOSPNAME                PIC  X(100).
      *���߼�̾
               05  SKT0700-KAISETUNAME             PIC  X(40).
      *���ŵ��إ�����
               05  SKT0700-HOSPCD                  PIC  X(28).
      *ɸ�ֿ��Ų�̾
               05  SKT0700-SRYKANAME1              PIC  X(30).
               05  SKT0700-SRYKANAME2              PIC  X(30).
               05  SKT0700-SRYKANAME3              PIC  X(30).
      *���
               05  SKT0700-GINKO                   PIC  X(20).
      *Ź̾
               05  SKT0700-TENMEI                  PIC  X(20).
      *�����ݸ��Կ�
               05  SKT0700-KENNAI-HKNJSU           PIC  X(11).
      *�����ݸ��Կ�
               05  SKT0700-KENGAI-HKNJSU           PIC  X(11).
      *����ơ��֥�
               05  SKT0700-SEIKYU-TBL              OCCURS  4.
      *�����ơ��֥�
                   07  SKT0700-NYUGAI-TBL          OCCURS  2.
      *���
                       09  SKT0700-KENSU           PIC  X(11).
      *����
                       09  SKT0700-NISSU           PIC  X(11).
      *����
                       09  SKT0700-TENSU           PIC  X(14).
      *���������
                   07  SKT0700-SKENSU              PIC  X(06).
      *����������
                   07  SKT0700-SNISSU              PIC  X(06).
      *���������
                   07  SKT0700-SKINGK              PIC  X(11).
      *��ץơ��֥�
                   07  SKT0700-GOKEI-TBL.
      *��ס����
                       09  SKT0700-G-KENSU         PIC  X(11).
      *��ס�����
                       09  SKT0700-G-NISSU         PIC  X(11).
      *��ס�����
                       09  SKT0700-G-TENSU         PIC  X(14).
      *��ס��������
                       09  SKT0700-G-SKENSU        PIC  X(06).
      *��ס���������
                       09  SKT0700-G-SNISSU        PIC  X(06).
      *��ס��������
                       09  SKT0700-G-SKINGK        PIC  X(11).
      *����������
               05  SKT0700-TOKUBETU                PIC  X(10).
           03  SKT0700-TBL-B.
      *����ǯ��
               05  SKT0700-B-SEIYM                 PIC  X(16).
      *���ŵ��ؽ���
               05  SKT0700-B-ADRS                  PIC  X(100).
      *���ŵ���̾��
               05  SKT0700-B-HOSPNAME              PIC  X(100).
      *���߼�̾
               05  SKT0700-B-KAISETUNAME           PIC  X(40).
      *���ŵ��إ�����
               05  SKT0700-B-HOSPCD                PIC  X(28).
      *ɸ�ֿ��Ų�̾
               05  SKT0700-B-SRYKANAME1            PIC  X(30).
               05  SKT0700-B-SRYKANAME2            PIC  X(30).
               05  SKT0700-B-SRYKANAME3            PIC  X(30).
      *���
               05  SKT0700-B-GINKO                 PIC  X(20).
      *Ź̾
               05  SKT0700-B-TENMEI                PIC  X(20).
      *�����ݸ��Կ�
               05  SKT0700-B-KENNAI-HKNJSU         PIC  X(11).
      *�����ݸ��Կ�
               05  SKT0700-B-KENGAI-HKNJSU         PIC  X(11).
      *����ơ��֥�
               05  SKT0700-B-SEIKYU-TBL            OCCURS  4.
      *�����ơ��֥�
                   07  SKT0700-B-NYUGAI-TBL        OCCURS  2.
      *���
                       09  SKT0700-B-KENSU         PIC  X(11).
      *����
                       09  SKT0700-B-NISSU         PIC  X(11).
      *����
                       09  SKT0700-B-TENSU         PIC  X(14).
      *���������
                   07  SKT0700-B-SKENSU            PIC  X(06).
      *����������
                   07  SKT0700-B-SNISSU            PIC  X(06).
      *���������
                   07  SKT0700-B-SKINGK            PIC  X(11).
      *��ץơ��֥�
                   07  SKT0700-B-GOKEI-TBL.
      *��ס����
                       09  SKT0700-B-G-KENSU       PIC  X(11).
      *��ס�����
                       09  SKT0700-B-G-NISSU       PIC  X(11).
      *��ס�����
                       09  SKT0700-B-G-TENSU       PIC  X(14).
      *��ס��������
                       09  SKT0700-B-G-SKENSU      PIC  X(06).
      *��ס���������
                       09  SKT0700-B-G-SNISSU      PIC  X(06).
      *��ס��������
                       09  SKT0700-B-G-SKINGK      PIC  X(11).
      *����������
               05  SKT0700-B-TOKUBETU              PIC  X(10).