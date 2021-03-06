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
       01  SC03.
      *    ���ŵ�������ɽ��ʬ
           03  SC03-TENSUHYO            PIC  X(02) OCCURS 4.
      *    ���ŵ��إ�����
           03  SC03-HOSPCDN             PIC  X(40).
      *    ��������
           03  SC03-PRTYM               PIC  X(16).
      *    ��Į¼̾
           03  SC03-CITYNAME            PIC  X(30).
      *    ��������
           03  SC03-SEIYMD              PIC  X(22).
      *    ͹���ֹ�
      *     03  SC03-POST                PIC  X(18).
      *    �����
           03  SC03-ADRS                PIC  X(100).
      *    ���ŵ���̾��
           03  SC03-HOSPNAME            PIC  X(100).
      *    �����ֹ�
           03  SC03-TEL                 PIC  X(15).
      *    ���ճ��
           03  SC03-KYURATE-TBL        OCCURS  8.
               05  SC03-KYURATE         PIC  X(02).
      *    ����¾�����ֹ�
           03  SC03-ETCKOHNUM           PIC  X(04).
      *
           03  SC03-MEISAI-TBL         OCCURS  10.
      *    ��ô���ֹ�
               05  SC03-FTNJANUM        PIC  X(16).
      *    ������ֹ�
               05  SC03-JKYSNUM         PIC  X(14).
      *    �ݸ�����
      *    ��̾
               05  SC03-NAME            PIC  X(30).
      *    ����
               05  SC03-SEX-TBL        OCCURS  2.
                   07  SC03-SEX         PIC  X(02).
      *    ��ǯ����
               05  SC03-BIRTHDAY-TBL.
                   07  SC03-NENGO-TBL  OCCURS  5.
                       09  SC03-NENGO   PIC  X(02).
                   07  SC03-BIRTHDAY    PIC  X(14).
      *    ����
               05  SC03-TENSU-TBL      OCCURS 2.
                   07  SC03-TENSU       PIC  X(07).
      *    ���ް�����ô��
      *         05  SC03-YKZFTN          PIC  X(06).
      *    ʡ������������
               05  SC03-SEIKYUGK        PIC  X(07).
      *    �ݸ����ֹ�
               05  SC03-HKNJANUM        PIC  X(16).
      *    ����
               05  SC03-BIKO            PIC  X(06).
      *    ���ͣ�
               05  SC03-BIKO2           PIC  X(10).
      *    ���ͣ�
               05  SC03-BIKO3           PIC  X(06).
      *    ���ͣ�
               05  SC03-BIKO4           PIC  X(14).
      *    ���͡��ǡ�¿��
               05  SC03-TEI-TBL        OCCURS 2.
                   07  SC03-TEI         PIC  X(02).
                   07  SC03-MARU        PIC  X(02).
      *    ���
           03  SC03-GOKEI-TBL.
      *    ��ס�����
               05  SC03-GTENSU-TBL     OCCURS 2.
                   07  SC03-GTENSU      PIC  X(09).
      *    ��ס�ʡ������������
               05  SC03-GSEIKYUGK       PIC  X(09).
