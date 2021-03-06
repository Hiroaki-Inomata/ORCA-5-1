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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.49    ���    19/05/14    �����б�
      ********************************************************************
       01  SC04.
      *    ���ŵ�������ɽ��ʬ
           03  SC04-TENSUHYO            PIC  X(02) OCCURS 4.
      *    ���ŵ��إ�����
           03  SC04-HOSPCDN             PIC  X(40).
      *    ��������
           03  SC04-PRTYM               PIC  X(16).
      *    ��Į¼̾
           03  SC04-CITYNAME            PIC  X(30).
      *    ��������
           03  SC04-SEIYMD              PIC  X(22).
      *    �����
           03  SC04-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SC04-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SC04-TEL                 PIC  X(15).
      *    ���ճ��
           03  SC04-KYURATE-TBL        OCCURS  8.
               05  SC04-KYURATE         PIC  X(02).
      *    ����¾�����ֹ�
           03  SC04-ETCKOHNUM           PIC  X(04).
      *
           03  SC04-MEISAI-TBL         OCCURS  10.
      *    ��ô���ֹ�
               05  SC04-FTNJANUM        PIC  X(16).
      *    ������ֹ�
               05  SC04-JKYSNUM         PIC  X(14).
      *    ��̾
               05  SC04-NAME            PIC  X(30).
      *    ����
               05  SC04-SEX-TBL        OCCURS  2.
                   07  SC04-SEX         PIC  X(02).
      *    ��ǯ����
               05  SC04-BIRTHDAY-TBL.
                   07  SC04-NENGO-TBL  OCCURS  5.
                       09  SC04-NENGO   PIC  X(02).
                   07  SC04-BIRTHDAY    PIC  X(14).
      *    ����
               05  SC04-TENSU-TBL      OCCURS 2.
                   07  SC04-TENSU       PIC  X(07).
      *    ����ɸ����ô��
               05  SC04-SFTNGK          PIC  X(06).
      *    ʡ������������
               05  SC04-SEIKYUGK        PIC  X(07).
      *    ��������
               05  SC04-SNISSU          PIC  X(02).
      *    �ݸ����ֹ�
               05  SC04-HKNJANUM        PIC  X(16).
      *    ����
               05  SC04-BIKO            PIC  X(06).
      *    ���ͣ�
               05  SC04-BIKO2           PIC  X(10).
      *    ���ͣ�
               05  SC04-BIKO3           PIC  X(06).
      *    ���ͣ�
               05  SC04-BIKO4           PIC  X(14).
      *    ���͡��ǡ�¿��
               05  SC04-TEI-TBL        OCCURS 2.
                   07  SC04-TEI         PIC  X(02).
                   07  SC04-MARU        PIC  X(02).
      *    ���
           03  SC04-GOKEI-TBL.
      *    ��ס�����
               05  SC04-GTENSU-TBL     OCCURS 2.
                   07  SC04-GTENSU      PIC  X(09).
      *    ��ס����ް�����ô��
               05  SC04-GSFTNGK         PIC  X(07).
      *    ��ס�ʡ������������
               05  SC04-GSEIKYUGK       PIC  X(09).
      *    ��סݿ�������
               05  SC04-GSNISSU         PIC  X(03).
