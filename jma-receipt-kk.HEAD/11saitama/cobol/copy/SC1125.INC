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
       01  SC25.
      *���ŵ��إ�����
           03  SC25-HOSPCD              PIC  X(14).
      *��������
           03  SC25-SEIYMD              PIC  X(22).
      *���ŵ��ؽ����
           03  SC25-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SC25-HOSPNAME            PIC  X(100).
      *���߼�̾
           03  SC25-KAISETU             PIC  X(40).
      *���������
           03  SC25-GSEIKYUGK           PIC  X(09).
      *
           03  SC25-MEISAI-TBL OCCURS 9.
      *������ֹ�
               05  SC25-JKYSNUM         PIC  X(14).
      *����ǯ��
               05  SC25-SRYYY           PIC  X(02).
               05  SC25-SRYMM           PIC  X(02).
      *���������ʬ
               05  SC25-NYUGAIKBN-TBL   OCCURS 2.
                   07  SC25-NYUGAIKBN   PIC  X(02).
      *��������
               05  SC25-NISSU           PIC  X(02).
      *�ݸ�������
               05  SC25-TENSU           PIC  X(10).
      *¾ˡ����(��ˡ�����)
               05  SC25-TAHOU           PIC  X(10).
      *������ô��(������ô��)
               05  SC25-ITBFTN          PIC  X(07).
      *���ް�����ô��
               05  SC25-YKZFTN          PIC  X(06).
      *��������
               05  SC25-SNISSU          PIC  X(02).
      *��������ɸ����ô��
               05  SC25-FTNGK           PIC  X(06).
      *����Ի�̾
               05  SC25-NAME            PIC  X(20).
      *�ܿͲ�²��ʬ
               05  SC25-HONKZKKBN-TBL OCCURS 2.
                   07  SC25-HONKZKKBN   PIC  X(02).
      *���Ų�̾
               05  SC25-SRYKANAME       PIC  X(10).
      *���
           03  SC23-GOKEI-TBL.
      *���סݰ�����ô����
               05  SC25-SKENSU          PIC  X(01).
      *���סݰ�����ô��
               05  SC25-SITBFTN         PIC  X(07).
      *���ס�����ɸ����ô��
               05  SC25-SFTNGK          PIC  X(07).
      *���ס����ް�����ô����
               05  SC25-SYKZKENSU       PIC  X(01).
      *���ס����ް�����ô���
               05  SC25-SYKZFTN         PIC  X(07).
      *��סݷ��
               05  SC25-GKENSU          PIC  X(03).
      *��סݶ��
               05  SC25-GITBFTN         PIC  X(09).
      *��ס�����ɸ����ô��
               05  SC25-GFTNGK          PIC  X(07).
