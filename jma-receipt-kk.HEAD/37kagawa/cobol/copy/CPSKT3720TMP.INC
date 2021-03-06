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
      ************************************************************
      *    �����������     COPY (CPSKT3720TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����         10/09/24  ����б�
      *  01.00.02    ����         19/01/11  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT3720TMP-REC.
           03  SKT3720TMP-KEY.
      *���⡦����
               05  SKT3720TMP-PREFKBN           PIC  X(01).
      *���Ų�
               05  SKT3720TMP-RECEKA            PIC  X(02).
      *
      *����ơ��֥�
           03  SKT3720TMP-HKN-AREA  OCCURS  11.
      *���
               05  SKT3720TMP-KENSU        PIC 9(05).
      *���
               05  SKT3720TMP-KINGK        PIC 9(09).
      *ɸ����ô��
               05  SKT3720TMP-FTNGK        PIC 9(09).
      *
      *��ˡ�����
           03  SKT3720TMP-35-AREA.
      *��ˡ����
               05  SKT3720TMP-35-KENSU     PIC 9(05).
      *��ˡ����
               05  SKT3720TMP-35-KINGK     PIC 9(09).
      *��ˡ�ɸ����ô��
               05  SKT3720TMP-35-FTNGK     PIC 9(09).
      *
      *����ơ��֥�
           03  SKT3720TMP-KOH-AREA  OCCURS  20.
      *�����ֹ�
               05  SKT3720TMP-KOHNUM      PIC X(03).
      *����̾��
               05  SKT3720TMP-KOHNAME     PIC X(10).
      *���񡿷��
               05  SKT3720TMP-KKENSU      PIC 9(05).
      *���񡿶��
               05  SKT3720TMP-KKINGK      PIC 9(09).
      *����ɸ����ô��
               05  SKT3720TMP-KFTNGK      PIC 9(09).
