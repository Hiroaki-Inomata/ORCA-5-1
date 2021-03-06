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
      * 01.00.01     @@@@-���    11/01/07  ������������
      * 01.00.02     @@@@-���    11/12/16  ����η���򽸷פ���褦�˽���
      ********************************************************************
       01  KOHS-DATA.
      *    ��Į¼̾
           03  KOHS-CITYNAME                 PIC X(80).
      *    ����̾
           03  KOHS-KOHNAME-TBL.
               05  KOHS-KOHNAME              PIC X(10) OCCURS 15.
      *    ID(�����ֹ�)
           03  KOHS-PTNUM-TBL.
               05  KOHS-PTNUM                PIC X(20) OCCURS 15.
               05  KOHS-PTNUM2               PIC X(40) OCCURS 15.
      *    ��̾
           03  KOHS-NAME-TBL.
               05  KOHS-NAME                 PIC X(20) OCCURS 15.
      *    �եꥬ��
           03  KOHS-FURIGANA-TBL.
               05  KOHS-FURIGANA             PIC X(24) OCCURS 15.
      *    ��ô���ֹ桿������ֹ�
           03  KOHS-FTNJA-JKYS-TBL           OCCURS 15.
               05  KOHS-FTNJANUM             PIC X(28).
               05  KOHS-JKYSNUM              PIC X(28).
      *    ����ǯ��
           03  KOHS-SRYYM-TBL.
               05  KOHS-SRYYM                PIC X(12) OCCURS 15.
      *    ����
           03  KOHS-SEX-TBL.
               05  KOHS-SEX                  PIC X(02) OCCURS 15.
      *    ������ʬ
           03  KOHS-NYUGAIKBN-TBL.
               05  KOHS-NYUGAIKBN            PIC X(02) OCCURS 15.
      *    �ݸ����ֹ�
           03  KOHS-HKNJANUM-TBL.
               05  KOHS-HKNJANUM             PIC X(16) OCCURS 15.
      *    ���桦�ֹ�
           03  KOHS-KIGONUM-TBL              OCCURS 15.
               05  KOHS-KIGO                 PIC X(16).
               05  KOHS-NUM                  PIC X(16).
      *    ������
           03  KOHS-NISSU-TBL.
               05  KOHS-NISSU                PIC X(04) OCCURS 15.
      *    ����
           03  KOHS-TENSU-TBL.
               05  KOHS-TENSU                PIC X(18) OCCURS 15.
      *    �ܿ���ô��
           03  KOHS-ITBFTN-TBL.
               05  KOHS-ITBFTN               PIC X(18) OCCURS 15.
      *    �������
           03  KOHS-SKJKAISU-TBL.
               05  KOHS-SKJKAISU             PIC X(04) OCCURS 15.
      *    ���������������
           03  KOHS-SKJSKY-TBL.
               05  KOHS-SKJSKY               PIC X(18) OCCURS 15.
      *    ����������ɸ����ô��
           03  KOHS-SKJFTN-TBL.
               05  KOHS-SKJFTN               PIC X(18) OCCURS 15.
      *    ���Ų�̾
           03  KOHS-SRYKANAME-TBL.
               05  KOHS-SRYKANAME            PIC X(10) OCCURS 15.
      *    ���ճ��
           03  KOHS-KYURATE-TBL.
               05  KOHS-KYURATE              PIC X(04) OCCURS 15.
      *    ¾ˡ����(ˡ���ֹ�)
           03  KOHS-TAHO-TBL.
               05  KOHS-TAHO                 PIC X(22) OCCURS 15.
      *    �������ʹԡ�
           03  KOHS-TAHO-TBL OCCURS 15.
             05  KOHS-KOHKENSU1              PIC X(10).
             05  KOHS-KENSU1                 PIC X(03).
      *    �������ʲ��ǡ�
           03  KOHS-KOHKENSU2-1              PIC X(12).
           03  KOHS-KOHKENSU2-2              PIC X(02).
           03  KOHS-KENSU2                   PIC X(03).
