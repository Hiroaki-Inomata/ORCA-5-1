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
       01  SC04o.
      *    ���ŵ�������ɽ��ʬ
           03  SC04o-TENSUHYO            PIC  X(02) OCCURS 4.
      *    ���ŵ��إ�����
           03  SC04o-HOSPCDN             PIC  X(40).
      *    ��������
           03  SC04o-PRTYM               PIC  X(16).
      *    ��Į¼̾
           03  SC04o-CITYNAME            PIC  X(30).
      *    ��������
           03  SC04o-SEIYMD              PIC  X(22).
      *    �����
           03  SC04o-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SC04o-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SC04o-TEL                 PIC  X(15).
      *    ���ճ��
           03  SC04o-KYURATE-TBL        OCCURS  8.
               05  SC04o-KYURATE         PIC  X(02).
      *    ����¾�����ֹ�
           03  SC04o-ETCKOHNUM           PIC  X(04).
      *    ����Ի�ʾڵ���
           03  SC04o-JKYSKIGO            PIC  X(02).
      *
           03  SC04o-MEISAI-TBL         OCCURS  12.
      *    ������ֹ�
               05  SC04o-JKYSNUM         PIC  X(14).
      *    �ݸ�����
               05  SC04o-HKNKBN-TBL     OCCURS  5.
                   07  SC04o-HKNKBN      PIC  X(02).
      *    ��̾
               05  SC04o-NAME            PIC  X(30).
      *    ����
               05  SC04o-SEX-TBL        OCCURS  2.
                   07  SC04o-SEX         PIC  X(02).
      *    ��ǯ����
               05  SC04o-BIRTHDAY-TBL.
                   07  SC04o-NENGO-TBL  OCCURS  4.
                       09  SC04o-NENGO   PIC  X(02).
                   07  SC04o-BIRTHDAY    PIC  X(14).
      *    ����
               05  SC04o-TENSU-TBL      OCCURS 2.
                   07  SC04o-TENSU       PIC  X(07).
      *    ����ɸ����ô��
               05  SC04o-SFTNGK          PIC  X(06).
      *    ʡ������������
               05  SC04o-SEIKYUGK        PIC  X(07).
      *    ��������
               05  SC04o-SNISSU          PIC  X(02).
      *    ����
               05  SC04o-BIKO            PIC  X(06).
      *    ���ͣ�
               05  SC04o-BIKO2           PIC  X(10).
      *    ���ͣ�
               05  SC04o-BIKO3           PIC  X(06).
      *    ���͡��ǡ�¿��
               05  SC04o-TEI-TBL        OCCURS 2.
                   07  SC04o-TEI         PIC  X(02).
                   07  SC04o-MARU        PIC  X(02).
      *    ���
           03  SC04o-GOKEI-TBL.
      *    ��ס�����
               05  SC04o-GTENSU-TBL     OCCURS 2.
                   07  SC04o-GTENSU      PIC  X(09).
      *    ��ס����ް�����ô��
               05  SC04o-GSFTNGK         PIC  X(07).
      *    ��ס�ʡ������������
               05  SC04o-GSEIKYUGK       PIC  X(09).
      *    ��סݿ�������
               05  SC04o-GSNISSU         PIC  X(03).
