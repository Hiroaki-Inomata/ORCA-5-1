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
       01  SKT1000.
      *    ����ǯ��
              03  SKT1000-SRYYM            PIC  X(16).
      *    ����ɽ��ʬ
              03  SKT1000-TENHYOKBN-TBL  OCCURS  4.
                  05  SKT1000-TENHYOKBN    PIC  X(02).
      *    ���ŵ��إ�����
              03  SKT1000-HOSPCDN          PIC  X(07).
      *    ���⡿����
              03  SKT1000-KENKBN1          PIC  X(02).
              03  SKT1000-KENKBN2          PIC  X(02).
      *    ͹���ֹ�
      *        03  SKT1000-POST             PIC  X(18).
      *    ����
              03  SKT1000-ADRS             PIC  X(100).
      *    ���ŵ���̾
              03  SKT1000-HOSPNAME         PIC  X(100).
      *    �����ֹ�
              03  SKT1000-TEL              PIC  X(15).
      *    ���߼�̾
              03  SKT1000-KAISETUNAME      PIC  X(40).
      *    ����������
              03  SKT1000-TOKUBETU         PIC  X(10).
      *    ����ơ��֥�
              03  SKT1000-SEIKYU-TBL    OCCURS  25.
      *    �ݸ���̾
                  05  SKT1000-HKNJANAME    PIC  X(20).
      *    �߰���
                  05  SKT1000-ZAI          PIC  X(02).
      *    �ݸ����ֹ�
                  05  SKT1000-HKNJANUM     PIC  X(12).
      *    �ݸ�����
                  05  SKT1000-SYUBETU-TBL   OCCURS  3.
                      07  SKT1000-SYUBETU  PIC  X(02).
      *    ��ʬ
                  05  SKT1000-KBN          PIC  X(01).
      *    ���
                  05  SKT1000-KYURATE      PIC  X(02).
      *    ���
                  05  SKT1000-KENSU        PIC  X(05).
      *    ����ʸ����ѡ�
                  05  SKT1000-KKENSU       PIC  X(03).
      *    �����ʡ���ѡ�
                  05  SKT1000-FKENSU       PIC  X(03).
      *    ����
                  05  SKT1000-TENSU        PIC  X(09).
      *    ���
              03  SKT1000-SUM-AREA  OCCURS  2.
      *    ��ס����
                  05  SKT1000-GKENSU       PIC  X(05).
      *    ��ס�����
                  05  SKT1000-GTENSU       PIC  X(10).
